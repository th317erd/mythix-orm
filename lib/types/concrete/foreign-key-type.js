'use strict';

const Nife        = require('nife');
const Type        = require('../type');
const ModelUtils  = require('../../utils/model-utils');

/// `FOREIGN_KEY` type.
///
/// This represents a foreign key to another column. It takes
/// on the type of the column it points to. For example, if it
/// points to an INTEGER column, then it will also be the same
/// type of integer.
///
/// Foreign keys are one of the ways Mythix ORM knows that models
/// are related, along with the `Model` and `Models` virtual types.
/// The "target" field must be a fully qualified field name. A
/// fully qualified field name is a name that is also prefixed by
/// the model that owns it. For example, `'User:firstName'` would
/// be a fully qualified field name, but `'firstName'` would not be.
///
/// Client-side storage for this field will be backed by
/// the same type as the field the foreign key targets.
///
/// Example:
///   class Role extends Model {
///     static fields = {
///       userID: {
///         type: Types.FOREIGN_KEY('User:id', {
///           onDelete: 'CASCADE',
///           onUpdate: 'CASCADE',
///         }),
///         allowNull: false,
///       },
///       roleMetaID: {
///         type: Types.FOREIGN_KEY({
///           modelName: 'RoleMeta',
///           fieldName: 'id',
///           onDelete: 'CASCADE',
///           onUpdate: 'CASCADE',
///         }),
///         allowNull: false,
///       },
///     };
///   }
///
/// See: Type
class ForeignKeyType extends Type {
  /// Check if this is a foreign key type.
  ///
  /// There are multiple different "type checking"
  /// static methods that exist for all field types.
  /// This is one of them. It checks if the type
  /// class is a foreign key type. This returns
  /// `true` for `FOREIGN_KEY`.
  ///
  /// Return: boolean
  ///   Return `true`, as the `FOREIGN_KEY` type is a
  ///   foreign key field.
  static isForeignKey() {
    return true;
  }

  /// Get the "display" name for this type.
  ///
  /// This method is called from <see>Model.toString</see>
  /// when stringifying the model for representation.
  ///
  /// Note:
  ///   This is also an instance method that can be called from
  ///   an instance of the type.
  ///
  /// Return: string
  ///   Return the string value `'FOREIGN_KEY'`
  static getDisplayName() {
    return 'FOREIGN_KEY';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  /// Construct a new `ForeignKeyType` type.
  ///
  /// This constructor has two call patterns.
  /// First, it can be called the shorthand way
  /// with a string (a fully qualified field name)
  /// as the first argument, and the field `options`
  /// as the second argument. Or, you can simply pass
  /// a single `options` argument, that must contain
  /// a `modelName` and `fieldName` property that are
  /// strings.
  ///
  /// Interface:
  ///   interface ForeignKeyTypeOptions {
  ///     // A direct field from a model, used for the target field
  ///     // directly. i.e. `Field: User.fields.id`.
  ///     // If this is present and valid then it
  ///     // will supersede the `modelName` and `fieldName`
  ///     // properties.
  ///     Field?: Field;
  ///
  ///     // The model name that owns the target field
  ///     modelName?: string;
  ///
  ///     // The field name of the target field on the
  ///     // target `modelName`.
  ///     fieldName?: string;
  ///
  ///     // onDelete specifies what action to take when
  ///     // a row in the target table is deleted.
  ///     onDelete: 'CASCADE' | 'SET NULL' | 'NO ACTION' | 'SET DEFAULT' | 'RESTRICT'
  ///
  ///     // onUpdate specifies what action to take when
  ///     // a row in the target table is updated (specifically
  ///     the target column).
  ///     onUpdate: 'CASCADE' | 'SET NULL' | 'NO ACTION' | 'SET DEFAULT' | 'RESTRICT'
  ///   }
  ///
  /// Return: ForeignKeyType
  ///   A new instance of the `ForeignKeyType` type.
  ///
  /// Arguments:
  ///   fullyQualifiedName: string | object
  ///     1) The fully qualified name of the target field as
  ///     a string (i.e. `'User:id'`), optionally followed
  ///     an `options` argument, or 2) the one and only
  ///     `options` argument requiring `modelName` and
  ///     `fieldName` properties.
  ///   options?: object
  ///     If `fullyQualifiedName` is defined as a string, then
  ///     this will be the options to the field.
  constructor(_fullyQualifiedName, _options) {
    if (arguments.length === 0)
      throw new TypeError('ForeignKeyType::constructor: You must specify a fully qualified field name, or provide complete options.');

    let fullyQualifiedName  = _fullyQualifiedName;
    let options             = _options || {};

    if (arguments.length === 1) {
      if (Nife.instanceOf(fullyQualifiedName, 'object'))
        options = fullyQualifiedName;
    } else {
      let def = ModelUtils.parseQualifiedName(fullyQualifiedName);
      if (Nife.isEmpty(def.modelName))
        throw new TypeError('ForeignKeyType::constructor: No model found. You must specify a model.');

      options = Object.assign(
        {},
        options,
        {
          modelName: def.modelName,
          fieldName: def.fieldNames[0],
          fullyQualifiedName,
        },
      );
    }

    super(fullyQualifiedName, options);

    this.options = options;
    this.fullyQualifiedName = fullyQualifiedName;
  }

  /// Cast provided value to underlying type.
  ///
  /// This will cast the incoming value to the
  /// underlying type of this field, which is
  /// the type of the field that it targets.
  ///
  /// See <see>Type.castToType</see> for a more
  /// detailed description.
  ///
  /// Return: any
  ///   Return the incoming `value`, cast to this
  ///   type. `null` and `undefined` are simply
  ///   returned without casting.
  ///
  /// Arguments:
  ///   context: <see name="CastToTypeContext">Type.castToType</see>
  castToType(args) {
    let { value } = args;
    if (value == null)
      return value;

    let targetField = this.getTargetField(args.connection);
    if (!targetField) {
      let field         = this.getField();
      let debugFieldStr = '';

      if (field) {
        let Model = field.Model;
        if (Model)
          debugFieldStr = ` "${Model.getModelName()}:${field.fieldName}"`;
      }

      throw new TypeError(`ForeignKeyType::castToType: Target field not defined${debugFieldStr}.`);
    }

    return targetField.type.castToType(args);
  }

  /// Check if the provided value is valid.
  ///
  /// This will check if the provided value valid.
  /// It does so by calling the `isValidValue` of
  /// the type that it inherits from its target
  /// field.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check.
  isValidValue(value, options) {
    let targetField = this.getTargetField(options && options.connection);
    if (!targetField)
      throw new TypeError('ForeignKeyType::isValidValue: Target field not defined.');

    return targetField.type.isValidValue(value, options);
  }

  /// This is called when the field is first initialized on
  /// any model instance. It will verify the options provided
  /// to the field, and ensure that it can find the target
  /// field requested. If it can't find the target field
  /// requested for any reason, then it will throw an exception.
  ///
  /// Return: { Model: ModelClass, Field: Field }
  ///   Return the target model and field.
  ///
  /// Arguments:
  ///   SourceModel: ModelClass
  ///     The source model (model owning this foreign key field).
  ///   sourceField:
  ///     The source field (the field containing this foreign key type).
  ///   connection: <see>Connection</see>
  ///     The database connection. This is needed to fetch the target
  ///     field and model.
  parseOptionsAndCheckForErrors(SourceModel, sourceField, connection) {
    let options             = this.options;
    let fullyQualifiedName  = this.fullyQualifiedName;
    let Field               = options.Field;
    let fieldName           = options.fieldName;
    let Model               = (Field && Field.Model);

    if (!Model) {
      if (options.modelName) {
        Model = connection.getModel(options.modelName);
      } else {
        let def = ModelUtils.parseQualifiedName(fullyQualifiedName);
        Model = connection.getModel(def.modelName);

        if (!fieldName)
          fieldName = def.fieldNames[0];
      }
    }

    if (!Field) {
      let modelName = Model.getModelName();

      if (options.fieldName) {
        Field = connection.getField(options.fieldName, modelName);
      } else {
        let def = ModelUtils.parseQualifiedName(fullyQualifiedName);
        Field = connection.getField(def.fieldNames[0], modelName);
      }
    }

    if (!Field)
      throw new TypeError(`ForeignKeyType::parseOptionsAndCheckForErrors: No target field found for "${SourceModel.getModelName()}:${sourceField.fieldName}". You must specify a field.`);

    if (!Model)
      Model = Field.Model;

    if (!Model)
      throw new TypeError(`ForeignKeyType::parseOptionsAndCheckForErrors: No target model found for field "${SourceModel.getModelName()}:${sourceField.fieldName}". You must specify a model.`);

    return {
      Model,
      Field,
    };
  }

  /// Initialize a model instance against this type.
  ///
  /// Initialize is called whenever a model instance is
  /// created. Note that the type instance is shared
  /// across all model instances. `initialize` is still
  /// called for every model instance that is created however,
  /// because the type class can modify the model it
  /// exists on. For example, the `Model` and `Models` type
  /// inject custom relational methods onto each model instance.
  ///
  /// This specific `initialize` for the `ForeignKeyType`
  /// only runs once however. The first owning model instance
  /// that is created will call this method, and at that point
  /// the target field will be looked up and found. After this
  /// first lookup, the target field will be cached for all
  /// future model instances of the same type.
  ///
  /// Return: undefined
  ///
  /// Arguments:
  ///   connection: <see>Connection</see>
  ///     The database connection of the calling model instance.
  ///   self: Model
  ///     The actual model instance that is calling this method.
  initialize(connection, self) {
    if (this.targetModel)
      return;

    super.initialize(connection, self);

    if (!connection)
      throw new Error('ForeignKeyType::initialize: A connection is required to use the ForeignKeyType.');

    let {
      Model,
      Field,
    } = this.parseOptionsAndCheckForErrors(this.getModel(), this.getField(), connection);

    Object.defineProperties(this, {
      'targetModel': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        Model,
      },
      'targetField': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        Field,
      },
    });
  }

  /// Get the options that were passed to the
  /// `ForeignKeyType` on creation.
  ///
  /// Return: object
  ///   The options object that was given to the field.
  getOptions() {
    return this.options;
  }

  /// Fetch the target model, which is the model
  /// owning the target field.
  ///
  /// Note:
  ///   If this is called before any model has been initialized,
  ///   or before a connection has been bound to your models, then
  ///   you **must** provide a `connection` or this method will fail.
  ///
  /// Return: ModelClass
  ///   The target field's owning model.
  ///
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     A database connection, which will be used to fiend the
  ///     target field and owning model.
  getTargetModel(connection) {
    if (connection && !this.targetModel)
      this.initialize(connection);

    return this.targetModel;
  }

  /// Fetch the name of target model, which is the model
  /// owning the target field.
  ///
  /// Note:
  ///   If this is called before any model has been initialized,
  ///   or before a connection has been bound to your models, then
  ///   you **must** provide a `connection` or this method will fail.
  ///
  /// Return: string
  ///   The target field's owning model name.
  ///
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     A database connection, which will be used to fiend the
  ///     target field and owning model.
  getTargetModelName(connection) {
    let targetModel = this.getTargetModel(connection);
    return targetModel.getModelName();
  }

  /// Fetch the target field. You can always access
  /// the owning model via the `field.Model` property
  /// on the field.
  ///
  /// Note:
  ///   If this is called before any model has been initialized,
  ///   or before a connection has been bound to your models, then
  ///   you **must** provide a `connection` or this method will fail.
  ///
  /// Return: <see>Field</see>
  ///   The target field from the target model.
  ///
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     A database connection, which will be used to fiend the
  ///     target field.
  getTargetField(connection) {
    if (connection && !this.targetModel)
      this.initialize(connection);

    return this.targetField;
  }

  /// Fetch the target field's name.
  ///
  /// Note:
  ///   If this is called before any model has been initialized,
  ///   or before a connection has been bound to your models, then
  ///   you **must** provide a `connection` or this method will fail.
  ///
  /// Return: string
  ///   The target field's name.
  ///
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     A database connection, which will be used to fiend the
  ///     target field.
  getTargetFieldName(connection) {
    let targetField = this.getTargetField(connection);
    if (!targetField)
      return;

    return targetField.fieldName;
  }

  toConnectionType(connection, options) {
    let targetField = this.getTargetField(connection);
    if (!targetField)
      return '';

    return targetField.type.toConnectionType(connection, options);
  }

  /// Stringify the type itself. **This will be
  /// the type of the target field**.
  ///
  /// If a `connection` argument is provided, then this
  /// will go through the connection to generate the type
  /// for the underlying database. If no connection is
  /// provided, then a "standard" SQL type will be returned
  /// for target field type instead.
  ///
  /// Return: string
  ///
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection. If provided, send this
  ///     type through <see>Type.toConnectionType</see>
  ///     to have the connection itself generate the underlying
  ///     type for the database. If `connection` is not provided,
  ///     then this will simply return a "standard" generic matching
  ///     SQL type of the target field type.
  toString(...args) {
    if (args.length === 0)
      return 'ForeignKeyType {}';

    let targetField = this.getTargetField(...args);
    if (!targetField)
      return '';

    return targetField.type.toConnectionType(...args);
  }
}

module.exports = {
  FOREIGN_KEY: Type.wrapConstructor(ForeignKeyType),
  ForeignKeyType,
};
