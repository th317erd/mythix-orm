'use strict';

const Nife                = require('nife');
const RelationalTypeBase  = require('./relational-type-base');
const ModelUtils          = require('../../utils/model-utils');

// These get injected into the class as
// create{fieldName}, get{fieldName}, etc...
const TYPE_OPERATIONS = {
  'queryFor': async function({ field, type }, userQuery, options, ...args) {
    return await type.prepareQuery({ connection: null, self: this, field, options }, [ userQuery ].concat(args));
  },
  'create': async function({ field, type, get }, model, options) {
    if (!model)
      return false;

    let result = await this.getConnection(options && options.connection).transaction(async (connection) => {
      let TargetModel = type.getTargetModel(connection);

      // Why are we fetching on get? Because this
      // could be a complex relationship, and there
      // might already be something that exists.
      // TODO: Likely a good area for performance
      // improvements... we can likely detect if
      // the relationship is too complex to guess
      // or not.
      let fetchedModel  = await get.call(this, TargetModel.where(connection).LIMIT(1), options);
      if (fetchedModel && fetchedModel instanceof TargetModel) {
        fetchedModel.setAttributes(model, true);
        if (fetchedModel.isDirty())
          await fetchedModel.save(options);

        return fetchedModel;
      }

      let storedModels  = await ModelUtils.createAndSaveAllRelatedModels(connection, this, field, [ model ], options);
      let resultModel   = storedModels[0];

      // Update this model to reflect the update
      ModelUtils.setRelationalValues(connection, this.getModel(), this, resultModel.getModel(), resultModel);

      return resultModel;
    }, options);

    // Save needs to go outside of the transaction
    // otherwise some DBs will violate foreign key
    // constraints
    if (this.isDirty())
      await this.save(options);

    return result;
  },
  'get': async function({ field, type }, userQuery, options, ...args) {
    let query = await type.prepareQuery({ connection: null, self: this, field, options }, [ userQuery ].concat(args));
    return await query.first(null, options);
  },
  'update': async function({ fullMethodName, field, type, create }, attributes, _options, ...args) {
    let options = _options || {};

    let result = await this.getConnection(options.connection).transaction(async (connection) => {
      let query = await type.prepareQuery({ connection, self: this, field, options }, args);
      let model = await query.first(null, options);

      if (!model) {
        if (options.force === true)
          return await create.call(this, attributes, options);

        throw new Error(`${field.Model.getModelName}::${fullMethodName}: Model not found to update. You can pass "{ force: true }" to the options to force a creation instead.`);
      }

      model.setAttributes(attributes, true);
      if (!model.isDirty())
        return model;

      let [ storedModel ] = await connection.update(model.getModel(), [ model ], options);

      // Update this model to reflect the update
      ModelUtils.setRelationalValues(connection, this.getModel(), this, storedModel.getModel(), storedModel);

      return storedModel;
    }, options);

    // Save needs to go outside of the transaction
    // otherwise some DBs will violate foreign key
    // constraints
    if (this.isDirty())
      await this.save(options);

    return result;
  },
  'destroy': async function({ field, type }, options, ...args) {
    let result = await this.getConnection(options && options.connection).transaction(async (connection) => {
      let query = await type.prepareQuery({ connection, self: this, field, options }, args);
      let model = await query.first(options);

      if (!model)
        return false;

      await connection.destroy(model.getModel(), [ model ], options);

      // Update this model to reflect the deletion
      ModelUtils.setRelationalValues(connection, this.getModel(), this, model.getModel());

      return true;
    }, options);

    // Save needs to go outside of the transaction
    // otherwise some DBs will violate foreign key
    // constraints
    if (this.isDirty())
      await this.save(options);

    return result;
  },
  'has': async function({ field, type }, options, ...args) {
    let query = await type.prepareQuery({ connection: null, self: this, field, options }, args);
    return await query.exists(options);
  },
  'pluck': async function({ field, type }, fields, options, ...args) {
    let query = await type.prepareQuery({ connection: null, self: this, field, options }, args);
    return await query.pluck(fields, options);
  },
};

/// A "virtual" field `type` that defines a one to one
/// relationship with another model.
///
/// Relationships (also known as "associations") in Mythix ORM
/// are defined by virtual fields that return a "relationship query"
/// from a method known as a "query generator" that is defined along
/// with the type definition.
/// This "relationship query" itself defines the model relationship, no matter
/// how complicated. Other ORMs require that you define relationships
/// via their "through table", matching foreign key, type, etc...
/// In Mythix ORM, everything is simply defined as a query for the
/// relationship.
///
/// This field `type` can be used to define a virtual field
/// that defines a relationship to another model. When your
/// model is instantiated into an instance, then this type will
/// inject the following methods into your model instance to
/// assist you in interacting with the relationship. The "injected methods"
/// listed below are "prefixes". The full names of these methods are
/// always postfixed with the actual name of your field that defines
/// the relationship. For example, if you had a `primaryRole` field
/// that defined this type of relationship, then the methods injected
/// would be `model.getPrimaryRole`, `model.destroyPrimaryRole`, `model.pluckPrimaryRole`, etc...
/// | Injected Method Prefix | Full Method Name | Signature | Description |
/// | ---------------------- | ---------------- | ----------- | --------- |
/// | `create` | `create${fieldName}` | `create(modelInstanceOrAttributes: Model \| object, options?: object, ...args: Array<any>): Promise<Model>` | Create the model defined by the relationship. Returns the newly created model. |
/// | `destroy` | `destroy${fieldName}` | `destroy(options?: object, ...args: Array<any>): Promise<number>` | Destroy the model defined by the relationship. Returns the number of models destroyed. |
/// | `get` | `get${fieldName}` | `get(userQuery?: QueryEngine, options?: object, ...args: Array<any>): Promise<Model>` | Get the model specified by the relationship, if one is found. Returned the related model (if there is any). |
/// | `has` | `has${fieldName}` | `has(options?: object, ...args: Array<any>): Promise<boolean>` | Check if the model defined by the relationship exists. |
/// | `pluck` | `pluck${fieldName}` | `pluck(fields: Field \| string \| Array<Field \| string>, options?: object, ...args: Array<any>): Promise<Array<any>>` | Pluck specific fields from the related model. |
/// | `queryFor` | `queryFor${fieldName}` | `queryFor(userQuery?: QueryEngine, options?: object, ...args: Array<any>): Promise<QueryEngine>` | Get the raw relationship query defined by this field type (returned by the "query generator"). |
/// | `update` | `update${fieldName}` | `update(modelInstanceOrAttributes: Model \| object, options?: object, ...args: Array<any>): Promise<Model>` | Update the model specified by the relationship. Return the updated model. |
///
/// The `...args` provided for each injected method are pass-through args for the user, that allow
/// the user to pass any arguments they desire to the "relational query generator" method defined
/// by the field (the `...userArgs` as seen in the example below). One minor exception to this is
/// any method that has a `userQuery` argument. The `userQuery` argument will always also be passed as the
/// first argument to the `...userArgs` of the "query generator" method. The `userQuery` argument
/// is to allow the user to define an extra query to merge into the primary/root query of the relationship.
/// For example, in the example provided below, we could call `await user.getPrimaryRole(Role.where.name.EQ('admin'))`
/// to only get the primary role if it also had a `name` attribute with the value `'admin'`.
///
/// This type also injects duplicates of each of these relational methods with an underscore prefix, i.e.
/// `_getPrimaryRole`. These are known as "root methods". They are provided so the user can overload the
/// default implementations. For example, you could define a `getPrimaryRole` method on your model class,
/// and from that call the "super" method simply by calling `this._getPrimaryRole`. This is required because
/// with method injection `super` doesn't actually exist, because the method was injected, instead of being
/// defined on the model class's prototype.
///
/// Example:
///   class User extends Model {
///     static fields = {
///       ...,
///       // Target primary role ID
///       // as a foreign key
///       primaryRoleID: {
///         type: Types.FOREIGN_KEY('Role:id', {
///           // When the role is deleted then
///           // clear this attribute by setting it to NULL
///           onDelete: 'SET NULL',
///           onUpdate: 'CASCADE',
///         }),
///         allowNull:    true,
///         index:        true,
///       },
///       // Add a virtual relationship field, which creates a
///       // one to one relationship with the Role model.
///       //
///       // Notice how a "type" can always be used directly as
///       // the field definition, instead of defining an object
///       // with a "type" property.
///       primaryRole: Types.Model(
///         // Specify the target/root model.
///         'Role',
///         // Define our "query generator" method that
///         // will return our "relationship query".
///         (context, connectionModels, ...userArgs) => {
///           // Get the model we need from the connection,
///           // which is conveniently passed to us as the
///           // `connectionModels` argument here.
///           let { Role } = connectionModels;
///
///           // Get the "self", which is the model instance
///           // calling this method
///           // (i.e. with `model.getPrimaryRole()`, "self" would be "model")
///           let { self } = context;
///
///           // Now return a relationship query
///           return Role.where
///             .id
///               .EQ(self.primaryRoleID)
///             .AND
///             .MERGE(userArgs[0]); // Apply the `userQuery` (will do nothing if nullish)
///          },
///        ),
///      };
///    }
///
///    // ... later on
///    // get the "primary role" for the first user in the database
///    let user = await User.first();
///    // Call relationship method injected by the `Types.Model` type.
///    let primaryRole = await user.getPrimaryRole();
///
/// Note:
///   See the [Associations](./Associations) article for a more in-depth discussion
///   of Mythix ORM model relationship/associations.
///
/// Note:
///   The "query generator" method defined with the type can be an asynchronous method.
///
/// Note:
///   Relational methods **will not** be injected into the model instance if a method of
///   the same name already exists on the model instance. For example, if you define a
///   `getPrimaryRole` method on your model class, then the default `get` relational method
///   (`getPrimaryRole` in our example) won't be injected. However, the "root method" `_getPrimaryRole`
///   will still be injected, allowing you to call that as the `super` method instead.
///
/// Note:
///   Behind the scenes the relationship methods provided always apply a `LIMIT(1)` to
///   any query used. This is because this relationship type is singular, so it is expected
///   that only a single model can ever be returned.
class ModelType extends RelationalTypeBase {
  isManyRelation() {
    return false;
  }

  /// Internal method used to generate injected method names.
  ///
  /// This method likely should never be called by the user.
  /// It is used to generate the injected method names.
  ///
  /// Return: string
  ///   The method name to inject into the model instance.
  ///
  /// Arguments:
  ///   field: <see>Field</see>
  ///     The parent field defining this relationship.
  ///   operation: string
  ///     The operation being injected into the model instance, which is one of
  ///     the prefixes listed above, i.e. `update`, `get`, `destroy`, etc...
  ///   rootMethod: boolean
  ///     If `true`, then return a generated "root method" name, instead of a "user method".
  ///     The only difference is that the "root method" name is prefixed with an underscore `_`.
  fieldNameToOperationName(field, operation, rootMethod) {
    if (rootMethod)
      return `_${operation}${Nife.capitalize(field.fieldName)}`;
    else
      return `${operation}${Nife.capitalize(field.fieldName)}`;
  }

  /// Inject type methods into the model instance.
  ///
  /// The <see>Type.initialize</see> method is always called
  /// for each model that is instantiated. For this type, this
  /// is used to inject the relational methods into the model
  /// instance.
  ///
  /// Arguments:
  ///   connection: <see>Connection</see>
  ///     The connection for the model being instantiated.
  ///   self: <see>Model</see>
  ///     The model instance the type is being initialized for.
  initialize(connection, self) {
    return super.initialize(connection, self, TYPE_OPERATIONS);
  }
}

module.exports = {
  Model: RelationalTypeBase.wrapConstructor(ModelType),
  ModelType,
};
