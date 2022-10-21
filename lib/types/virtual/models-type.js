'use strict';

const Nife                = require('nife');
const Inflection          = require('inflection');
const RelationalTypeBase  = require('./relational-type-base');
const Utils               = require('../../utils');

const NAMED_METHOD = false;

async function destroyRelatedModels(connection, field, type, TargetModel, storedModels, invertMatch, options, operation) {
  // Destroy all relations not matching stored set
  let thisMethodName      = type.fieldNameToOperationName(field, operation, NAMED_METHOD);
  let parentModelName     = this.getModelName();
  let idMap               = Utils.getPrimaryKeysForModels(connection, TargetModel, storedModels, { includeRelations: true, skipRelations: [ field.Model.getModelName() ] });
  let { relationalMap }   = await Utils.getRelationalModelStatusForField(connection, this, field);
  let sortedModelNames    = Nife.subtractFromArray(Utils.sortModelNamesByCreationOrder(connection, Object.keys(relationalMap)), [ parentModelName ]);
  let targetRelationName  = sortedModelNames[0];
  let destroyModelNames   = sortedModelNames.slice(1).reverse();
  let targetIDs           = idMap[targetRelationName];
  let targetPKFieldName   = TargetModel.getPrimaryKeyFieldName();

  if (destroyModelNames.length === 0) {
    // No through-table... this is a one-to-many relation.
    // We can only destroy if the TargetModel has a foreign key
    // to the parent model.

    // Set destroyModelNames to the first relation.
    // The following code will ensure they only get
    // deleted if there is a foreign key relationship
    // to the parent model.
    destroyModelNames = [ targetRelationName ];
  }

  for (let i = 0, il = destroyModelNames.length; i < il; i++) {
    let modelName = destroyModelNames[i];
    let ids       = idMap[modelName];

    // If we have no ids for this model, then
    // there is nothing we can destroy
    if (Nife.isEmpty(ids))
      continue;

    let ThisModel             = connection.getModel(modelName);
    let thisModelPKFieldName  = ThisModel.getPrimaryKeyFieldName();
    if (!thisModelPKFieldName) {
      // Not sure how this would happen... since we obviously
      // have a list of ids already... but let's be safe anyway
      continue;
    }

    // We can only destroy models that have a foreign key
    // to the parent model.
    let foreignFieldNames = ThisModel.getForeignKeysTargetFieldNames(connection, parentModelName);
    if (Nife.isEmpty(foreignFieldNames))
      continue;

    // Create query to destroy related models
    let query = ThisModel.where(connection)[thisModelPKFieldName];
    if (invertMatch)
      query = query.EQ(ids);
    else
      query = query.NOT.EQ(ids);

    for (let j = 0, jl = foreignFieldNames.length; j < jl; j++) {
      let { targetFieldName, sourceFieldName } = foreignFieldNames[j];
      let parentModelValue = this[targetFieldName];

      // If this value is empty then something is wrong...
      // so fail early to prevent unwanted data loss
      if (parentModelValue == null)
        throw new Error(`${parentModelName}::${thisMethodName}: Field "${parentModelName}.${targetFieldName}" can not be empty for this operation.`);

      query = query.AND[sourceFieldName].EQ(parentModelValue);
    }

    // If there is a foreign field that points to the target
    // relation, then also match against those
    if (targetPKFieldName && Nife.isNotEmpty(targetIDs)) {
      let targetForeignField = ThisModel.getForeignKeysTargetField(connection, targetRelationName, targetPKFieldName);
      if (targetForeignField) {
        if (invertMatch)
          query = query.AND[modelName][targetForeignField.sourceFieldName].EQ(targetIDs);
        else
          query = query.AND[modelName][targetForeignField.sourceFieldName].NOT.EQ(targetIDs);
      }
    }

    // Now destroy relations to update set
    await query.destroy(options);
  }
}

// These get injected into the class as
// addTo{fieldName}, get{fieldName}, etc...
const TYPE_OPERATIONS = {
  'queryFor': async function({ field, type }, userQuery, options, ...args) {
    return await type.prepareQuery({ connection: null, self: this, field, options }, [ userQuery ].concat(args));
  },
  'addTo': async function({ field }, _models, options) {
    let models = Nife.toArray(_models).filter(Boolean);
    if (Nife.isEmpty(models))
      return [];

    // TODO: Needs to manage unique constraints
    // for through tables. Even though the model
    // might be persisted, the through table record
    // might still be created, which might blow up
    // if a constraint fails because the records
    // already exist. The opposite might also be true,
    // where the target models already exist, and
    // so shouldn't be created.
    return this.getConnection(options && options.connection).transaction(async (connection) => {
      let currentModels = this[field.fieldName];
      if (Nife.isEmpty(currentModels))
        currentModels = [];

      let storedModels  = await Utils.createAndSaveAllRelatedModels(connection, this, field, models, options);
      let allModels     = currentModels.concat(storedModels);

      this[field.fieldName] = allModels;

      return storedModels;
    }, options);
  },
  'get': function({ field, type }, userQuery, options, ...args) {
    const doGet = async function*() {
      let query                       = await type.prepareQuery({ connection: null, self: this, field, options }, [ userQuery ].concat(args));
      let results                     = query.all(Object.assign({}, options, { stream: true }));
      let primaryModelRelationalArray = [];

      this[field.fieldName] = primaryModelRelationalArray;

      for await (let item of results) {
        primaryModelRelationalArray.push(item);

        yield item;
      }
    };

    if (options && options.stream === true)
      return doGet.call(this);
    else
      return Utils.collect(doGet.call(this));
  },
  'set': async function({ field, type, addTo }, models, options) {
    return this.getConnection(options && options.connection).transaction(async (connection) => {
      let TargetModel = type.getTargetModel(connection);

      // Reset relation so we don't end up with
      // current persisted model instances
      this[field.fieldName] = [];

      // First, create models in set
      let storedModels = await addTo.call(this, models, Object.assign({}, options || {}, { connection }));

      await destroyRelatedModels.call(this, connection, field, type, TargetModel, storedModels, false, options, 'set');

      return storedModels;
    }, options);
  },
  'removeFrom': async function({ field, type, get }, _models, _options) {
    return this.getConnection(_options && _options.connection).transaction(async (connection) => {
      let { relationalMap, TargetModel } = await Utils.getRelationalModelStatusForField(connection, this, field);
      let primaryModelName  = this.getModelName();
      let targetModelName   = TargetModel.getModelName();
      let models            = Nife.toArray(_models);

      const needsLoad = async () => {
        let sortedModelNames = Utils.sortModelNamesByCreationOrder(connection, Object.keys(relationalMap));

        for (let i = 0, il = models.length; i < il; i++) {
          let model = models[i];

          if (!(model instanceof TargetModel))
            return true;

          if (!model.isPersisted())
            return true;

          for (let j = 0, jl = sortedModelNames.length; j < jl; j++) {
            let modelName = sortedModelNames[j];
            if (modelName === targetModelName || modelName === primaryModelName)
              continue;

            let relation        = relationalMap[modelName];
            let ThisModel       = relation.Model;
            let pluralModelName = ThisModel.getPluralModelName();

            if (!model[pluralModelName])
              return true;

            let relationSet = model[pluralModelName];
            if (Nife.isEmpty(relationSet))
              return true;

            if (!relationSet.every((relatedModel) => relatedModel.isPersisted()))
              return true;
          }
        }

        return false;
      };

      let options = Object.assign({}, _options || {}, { connection });
      let storedModels;

      if (await needsLoad()) {
        let query = Utils.generateQueryFromFilter(connection, TargetModel, models);
        if (!query)
          throw new Error(`${this.getModelName()}::${type.fieldNameToOperationName(field, 'removeFrom', NAMED_METHOD)}: Data provided is insufficient to complete operation.`);

        storedModels = await get.call(this, TargetModel.where(connection).AND(query), Object.assign({}, options, { stream: false, includeRelations: true, connection }));
      } else {
        storedModels = models;
      }

      // Destroy target relations to removed
      // requested models from set
      await destroyRelatedModels.call(this, connection, field, type, TargetModel, storedModels, true, options, 'removeFrom');

      return storedModels.length;
    }, _options);
  },
  'destroy': async function({ field, type }, userQuery, options, ...args) {
    let query = await type.prepareQuery({ connection: null, self: this, field, options }, [ userQuery ].concat(args));
    return await query.destroy(options);
  },
  'count': async function({ field, type }, userQuery, options, ...args) {
    let query = await type.prepareQuery({ connection: null, self: this, field, options }, [ userQuery ].concat(args));
    return await query.count(null, options);
  },
  'pluck': async function({ field, type }, userQuery, fields, options, ...args) {
    let query = await type.prepareQuery({ connection: null, self: this, field, fields, options }, [ userQuery ].concat(args));
    return await query.pluck(fields, options);
  },
  'has': async function({ count }, userQuery, options, ...args) {
    let itemCount = await count.call(this, userQuery, options, ...args);
    return (itemCount > 0);
  },
};

/// A "virtual" field `type` that defines a one to many,
/// or many to many relationship with other model(s).
///
/// Many to many relationships interact with "sets", which in
/// Mythix ORM is the term used to define an "array of models",
/// in our N to many relationship.
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
/// that defines a relationship to other models. When your
/// model is instantiated into an instance, then this type will
/// inject the following methods into your model instance to
/// assist you in interacting with the relationship set. The "injected methods"
/// listed below are "prefixes". The full names of these methods are
/// always postfixed with the actual name of your field that defines
/// the relationship. For example, if you had a `roles` field
/// that defined this type of relationship, then the methods injected
/// would be `model.getRoles`, `model.destroyRoles`, `model.pluckRoles`, etc...
/// | Injected Method Prefix | Full Method Name | Signature | Description |
/// | ---------------------- | ---------------- | ----------- | --------- |
/// | `addTo` | `addTo${fieldName}` | `addTo(models: Array<Model \| object>, options?: object): Promise<Array<Model>>` | Create the models defined by the relationship, and add them to the set. Returns the newly created models. |
/// | `count` | `count${fieldName}` | `count(userQuery?: QueryEngine, options?: object, ...args: Array<any>): Promise<number>` | Count the number of models in the set. |
/// | `destroy` | `destroy${fieldName}` | `destroy(options?: object, ...args: Array<any>): Promise<number>` | Destroy the models defined by the relationship set. Returns the number of models destroyed. Note that this not only destroys the target models, but also any through-table relationships involved. |
/// | `get` | `get${fieldName}` | `get(userQuery?: QueryEngine, options?: object, ...args: Array<any>): Promise<Array<Model>>` | Get the models specified by the relationship set. Return the entire model set. |
/// | `has` | `has${fieldName}` | `has(userQuery?: QueryEngine, options?: object, ...args: Array<any>): Promise<boolean>` | Check if the relationship set contains any models. |
/// | `pluck` | `pluck${fieldName}` | `pluck(userQuery?: QueryEngine, fields: Field \| string \| Array<Field \| string>, options?: object, ...args: Array<any>): Promise<Array<any>>` | Pluck specific fields from the related models (set). |
/// | `queryFor` | `queryFor${fieldName}` | `queryFor(userQuery?: QueryEngine, options?: object, ...args: Array<any>): Promise<QueryEngine>` | Get the raw relationship query defined by this field type (returned by the "query generator"). |
/// | `removeFrom` | `removeFrom${fieldName}` | `removeFrom(models: Array<Model>, options?: object): Promise<number>` | Remove the specified models from the relationship set without destroying the target models (destroy only the through-table linking models). Returns the number of models remaining in the set after the operation. |
/// | `set` | `set${fieldName}` | `set(models: Array<Model \| object>, options?: object): Promise<Array<Model>>` | Overwrite the model set with the models specified, destroying models outside the specified set of models. Returns the new models. |
///
/// The `...args` provided for each injected method are pass-through args for the user, that allow
/// the user to pass any arguments they desire to the "relational query generator" method defined
/// by the field (the `...userArgs` as seen in the example below). One minor exception to this is
/// any method that has a `userQuery` argument. The `userQuery` argument will always also be passed as the
/// first argument to the `...userArgs` of the "query generator" method. The `userQuery` argument
/// is to allow the user to define an extra query to merge into the primary/root query of the relationship.
/// For example, in the example provided below, we could call `await user.getRoles(Role.where.name.EQ('admin'))`
/// to only get the related roles where each role also has a `name` attribute with the value `'admin'`.
///
/// This type also injects duplicates of each of these relational methods with an underscore prefix, i.e.
/// `_getRoles`. These are known as "root methods". They are provided so the user can overload the
/// default implementations. For example, you could define a `getRoles` method on your model class,
/// and from that call the "super" method simply by calling `this._getRoles`. This is required because
/// with method injection `super` doesn't actually exist, because the method was injected, instead of being
/// defined on the model class's prototype.
///
/// Example:
///   class User extends Model {
///     static fields = {
///       ...,
///       // Add a virtual relationship field, which creates a
///       // one to many relationship with the Role model.
///       //
///       // Notice how a "type" can always be used directly as
///       // the field definition, instead of defining an object
///       // with a "type" property.
///       roles: Types.Models(
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
///           // (i.e. with `model.getRoles()`, "self" would be "model")
///           let { self } = context;
///
///           // Now return a relationship query
///           return Role.where
///             .userID
///               .EQ(self.id)
///             .AND
///             .MERGE(userArgs[0]); // Apply the `userQuery` (will do nothing if nullish)
///          },
///        ),
///      };
///    }
///
///    // ... later on
///    // get the "roles" for the first user in the database
///    let user = await User.first();
///    // Call relationship method injected by the `Types.Models` type.
///    let allUserRoles = await user.getRoles();
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
///   `getRoles` method on your model class, then the default `get` relational method
///   (`getRoles` in our example) won't be injected. However, the "root method" `_getRoles`
///   will still be injected, allowing you to call that as the `super` method instead.
///
/// Note:
///   This field type will not be "exposed" to models. This means that your model will not
///   have an attribute with the name provided by the field defining this type. The only
///   thing that will exist on your model will be the relational methods injected by this type.
///   Said another way, and using the examples provided, there will be no "roles" attribute
///   on your `User` model.
class ModelsType extends RelationalTypeBase {
  static exposeToModel() {
    return false;
  }

  isManyRelation() {
    return true;
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
  ///     the prefixes listed above, i.e. `addTo`, `get`, `destroy`, etc...
  ///   rootMethod: boolean
  ///     If `true`, then return a generated "root method" name, instead of a "user method".
  ///     The only difference is that the "root method" name is prefixed with an underscore `_`.
  fieldNameToOperationName(field, operation, rootMethod) {
    let fieldName = field.pluralName;
    if (!fieldName)
      fieldName = Nife.capitalize(Inflection.pluralize(field.fieldName));

    if (rootMethod)
      return `_${operation}${fieldName}`;
    else
      return `${operation}${fieldName}`;
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
  Models: RelationalTypeBase.wrapConstructor(ModelsType),
  ModelsType,
};
