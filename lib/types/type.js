'use strict';

const { checkDefaultValueFlags }  = require('./helpers/default-helpers');

/// Base type class for all other field types.
class Type {
  static getDisplayName() {
    return '<UNKNOWN>';
  }

  /// Get the "display name" for this type instance.
  ///
  /// This will return a "human friendly" name for the type,
  /// generally used for logging or debugging. It will change
  /// based on the type it is called upon. For example, a
  /// <see>BooleanType</see> will return the string value `'BOOLEAN'`.
  ///
  /// Note:
  ///   This method is also a static method on the type class.
  ///
  /// Return: string
  ///   A string representing the type. This is not the same as
  ///   the database equivalent type. Rather, it is simply used
  ///   to represent the type for logging or debugging.
  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  static _isMythixFieldType = true;

  static clone = function() {
    return this;
  };

  /// Use this method to check if a class
  /// is a Mythix ORM field type. It will return
  /// `true` if the provided value is a class
  /// that inherits from <see>Type</see>, or
  /// if the provided value has an attribute
  /// named `_isMythixFieldType` that is truthy.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: Function
  ///     Value to check.
  static isTypeClass(value) {
    if (!value)
      return false;

    if (value.prototype instanceof Type)
      return true;

    if (value._isMythixFieldType)
      return true;

    return false;
  }

  /// Check to see if the provided value is
  /// an *instance* of a Mythix ORM <see>Type</see>.
  /// Unlike <see>Type.static isTypeClass</see>, which
  /// checks if a *class* is a <see>Type</see>, this will check
  /// to see if an *instance* is an instance of a
  /// Mythix ORM <see>Type</see>. It will return
  /// `true` if the provided value is an `instanceof`
  /// <see>Type</see>, or if the value's `constructor`
  /// property has a truthy `_isMythixFieldType` property
  /// (`value.constructor._isMythixFieldType`)
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     Value to check
  static isType(value) {
    if (!value)
      return false;

    if (value instanceof Type)
      return true;

    if (value.constructor._isMythixFieldType)
      return true;

    return false;
  }

  /// Check to see if the provided type is the same as this type.
  ///
  /// This checks if the two types are the same type by comparing
  /// the type names. For example, a `Types.BooleanType.isSameType(new BooleanType())`
  /// would return `true`.
  ///
  /// Arguments:
  ///   value: <see>Type</see>
  ///     A type instance to check. This must be a type instance, not a type class.
  ///
  /// Return: boolean
  ///   Return `true` if the provided `value` is the same type instance as this type class.
  ///   Return `false` otherwise. Similarity is based on the name of the types. If the class
  ///   names between the two match, then the types are considered the same type.
  static isSameType(value) {
    return (this.isType(value) && value.constructor && value.constructor.name === this.name);
  }

  /// Instantiate this field type.
  ///
  /// This method will instantiate the type class it
  /// is called from. It won't instantiate the type if
  /// it is already instantiated, and it will call the
  /// type wrapper if one was provided (see <see>Type.static wrapConstructor</see>).
  /// It may not need to instantiate the type if the user already
  /// did so (for example `type: new Types.StringType()`,
  /// or `type: Types.STRING()`). It may need to instantiate
  /// however if the user didn't do so, for example `type: Types.StringType`,
  /// or `type: Types.STRING`.
  ///
  /// Note:
  ///   If the type is already instantiated, then it will be
  ///   cloned instead using <see>Type.clone</see>.
  ///
  /// Return: <see>Type</see>
  ///   The instantiated type instance.
  static instantiateType(_type) {
    let type = _type;
    if (!type)
      throw new TypeError('Type::instantiateType: "type" is required.');

    // Already instantiated?
    if (Type.isType(type))
      return type.clone();

    const ThisType = type;
    return new ThisType();
  }

  static isVirtual() {
    return false;
  }

  static isRelational() {
    return false;
  }

  static isForeignKey() {
    return false;
  }

  static exposeToModel() {
    return true;
  }

  /// This method is used to create the "shortcut" types
  /// for Mythix ORM. For example, a "string" type can be
  /// created either via `new Types.StringType()`, or via `Types.STRING()`.
  /// Both are equivalent. `Types.STRING` was created by calling
  /// `Types.STRING = Type.wrapConstructor(Types.StringType)`.
  ///
  /// This method "wraps" a type constructor, providing a more
  /// convenient way to create and use field types. It works
  /// by wrapping the provided class's constructor in a callable
  /// method. If the method **is not** called (i.e. `type: Types.STRING`)
  /// then the type system still knows what to do, because the wrapping
  /// method will be called by Mythix ORM to instantiate the type
  /// when the model's fields are first fetched.
  ///
  /// Arguments:
  ///   TypeKlass: class <see>Type</see>
  ///     A `Type` class to wrap.
  ///
  /// Return: Function
  ///   A callable function to instantiate the type class. If not called
  ///   by the user, then Mythix ORM will call the method when the type
  ///   is being created to fetch the type instance.
  static wrapConstructor(TypeKlass) {
    let TypeWrapper = function(...args) {
      return new TypeKlass(...args);
    };

    const staticPropertiesToCopy = [
      'Default',
      '_isMythixFieldType',
      'clone',
      'isTypeClass',
      'isType',
      'instantiateType',
      'isVirtual',
      'isRelational',
      'isForeignKey',
      'exposeToModel',
      'getDisplayName',
    ];

    for (let i = 0, il = staticPropertiesToCopy.length; i < il; i++) {
      let propName  = staticPropertiesToCopy[i];
      let value     = TypeKlass[propName];

      if (value !== undefined)
        TypeWrapper[propName] = value;
    }

    Object.defineProperties(TypeWrapper, {
      'name': {
        writable:     false,
        enumerable:   false,
        configurable: false,
        value:        `${TypeKlass.name}Wrapper`,
      },
    });

    return TypeWrapper;
  }

  /// Instantiate a new field Type.
  ///
  /// Arguments:
  ///   ...args: Array<any>
  ///     All field types "record" their constructor arguments, always.
  ///     Mythix ORM requires that all field types, when calling `super` in
  ///     their constructor, provide the base `Type` class all arguments
  ///     provided to their constructor.
  ///     This is so that the field type can later be cloned or serialized.
  constructor(...args) {
    Object.defineProperties(this, {
      '_args': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        args,
      },
      '_Model': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        null,
      },
      '_field': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        null,
      },
    });
  }

  /// Clone this field type.
  ///
  /// Return: Type
  ///   This field type, cloned to a new instance.
  clone() {
    let Type        = this.constructor;
    let newInstance = new Type(...this._args);

    newInstance._Model = this._Model;
    newInstance._field = this._field;

    return newInstance;
  }

  /// Check if this field type is a virtual field type.
  ///
  /// Note:
  ///   This method is also a static method on the type class.
  ///
  /// Return: boolean
  ///   Return `true` if this field type is a virtual field
  ///   type. Virtual fields are fields that don't *directly*
  ///   store a concrete value themselves. They often pull
  ///   and combine values from other fields, or other models.
  ///   Currently the only virtual fields in Mythix ORM are
  ///   <see>ModelType</see> and <see>ModelsType</see>.
  isVirtual() {
    return this.constructor.isVirtual.call(this.constructor);
  }

  /// Check if this field type is a relational field type.
  ///
  /// Note:
  ///   This method is also a static method on the type class.
  ///
  /// Return: boolean
  ///   Return `true` if this field type defines a relation,
  ///   instead of a direct value. Field types such as
  ///   <see>ModelType</see> and <see>ModelsType</see> are
  ///   examples of relational field types. <see>ForeignKeyType</see>
  ///   **is not** a relational type, but is instead considered
  ///   a "concrete" type. Even though it does technically define
  ///   a relationship, it is also a direct field, that actually
  ///   has a concrete value, backed by database storage.
  isRelational() {
    return this.constructor.isRelational.call(this.constructor);
  }

  /// Check if this field type is a foreign key type.
  ///
  /// Note:
  ///   This method is also a static method on the type class.
  ///
  /// Return: boolean
  ///   Return `true` if this field type defines a foreign key,
  ///   or `false` otherwise.
  isForeignKey() {
    return this.constructor.isForeignKey.call(this.constructor);
  }

  /// Used to decide if a field should be exposed on the model
  /// or not.
  ///
  /// Some fields, such as types like <see>ModelType</see> and
  /// <see>ModelsType</see> do not expose themselves on the model
  /// as model fields. Instead they only expose themselves via the
  /// relationship methods they inject onto the model instance.
  /// Most fields **do** expose themselves on the model, but any
  /// field that overrides this method and returns `false` will
  /// not be exposed on the model instance.
  ///
  /// Note:
  ///   This method is also a static method on the type class.
  ///
  /// Return: boolean
  ///   If `true` is returned, then the field owning this type
  ///   will be added to the model instance, and accessible by
  ///   the user. If `false` is returned, then the field will
  ///   be "hidden", and not exposed to the user on model instances.
  exposeToModel() {
    return this.constructor.exposeToModel.call(this.constructor);
  }

  /// Check if this field's value is driven by the database.
  ///
  /// For example, given an AUTOINCREMENTING id field, this
  /// would return `true`, because the value of the field is
  /// provided by the database itself. This can be true for
  /// nearly any field type, but is generally only true for
  /// auto-incrementing ids, and date/time types.
  ///
  /// Return: boolean
  ///   Return `true` for any field type that has a value provided
  ///   directly by the underlying database, or `false` otherwise.
  isRemote() {
    let field = this.getField();
    if (!field)
      throw new Error(`${this.constructor.name}::isRemote: Error, the model type must be initialized before you call "isRemote".`);

    if (field.remote === true)
      return true;

    return checkDefaultValueFlags(field.defaultValue, [ 'remote' ]);
  }

  /// Check if the value provided is a valid value for this field type.
  ///
  /// This is used to check if any value *would* be a valid value for
  /// this type. For example, if this type happens to be a <see>BooleanType</see>,
  /// then `isValidValue` would only return `true` if the provided `value`
  /// was either `true` or `false`. All field types have their own custom
  /// `isValidValue` implementation, to verify values against their type.
  ///
  /// Arguments:
  ///   value: any
  ///     Any value to check.
  ///
  /// Return: boolean
  ///   Return `true` if the provided `value` is a valid value for the field
  ///   type this was called upon, or `false` otherwise.
  // eslint-disable-next-line no-unused-vars
  isValidValue(value) {
    return true;
  }

  /// Get the field that owns this type.
  getField() {
    return this._field;
  }

  /// Set the field that owns this type.
  ///
  /// Arguments:
  ///   field: <see>Field</see>
  ///     The field that owns this type.
  setField(field) {
    this._field = field;
  }

  /// Get the model class that owns this field type.
  getModel() {
    let Model = this._Model;
    if (!Model) {
      let field = this.getField();
      if (!field)
        return;

      Model = field.Model;
      if (!Model)
        return;

      this._Model = Model;
    }

    return Model;
  }

  /// Set the model class that this type belongs to.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class that owns this field type.
  setModel(Model) {
    this._Model = Model;
  }

  /// Cast a value to the underlying field type.
  ///
  /// This method is implemented differently for
  /// every field type. Its job is to cast any
  /// value provided to the type. For many types,
  /// if a `null` or `undefined` value is provided,
  /// then that value will simply be returned.
  ///
  /// Interface:
  ///   interface CastToTypeContext {
  ///     connection: Connection;  // The connection of the model.
  ///     field: Field;            // The field descriptor that has this type.
  ///     Model: class Model;      // The parent Model class of the field.
  ///     self: Model;             // The model instance.
  ///     value: any;              // The value that should be cast.
  ///   };
  ///
  /// Return: any
  ///
  /// Arguments:
  ///   context: CastToTypeContext
  ///     The "context" passed to `castToType`. This contains
  ///     all that any type should need to cast a value.
  castToType({ value }) {
    return value;
  }

  /// Initialize a model instance against this type.
  ///
  /// Initialize is called whenever a model instance is
  /// created. Note that the type instance is shared
  /// across all model instances. `initialize` is still
  /// called for every model instance that is created however,
  /// because the type class can modify the model it
  /// exists on. For example, the <see>ModelType</see> and <see>ModelsType</see> types
  /// inject custom relational methods onto each model instance.
  ///
  /// Return: undefined
  ///
  /// Arguments:
  ///   connection: <see>Connection</see>
  ///     The database connection of the calling model instance.
  ///   self: Model
  ///     The actual model instance that is calling this method.
  // eslint-disable-next-line no-unused-vars
  initialize(connection, self) {
  }

  /// Check if the field value is dirty.
  ///
  /// This is only used by some field types, such as <see>SerializedType</see>.
  /// It will respond with `true` if the field is dirty, or `false` if it isn't.
  ///
  /// Interface:
  ///   interface CheckDirtyContext {
  ///     value: any; // The field's current value
  ///     field: Field; // The field that is being checked
  ///     fieldName: string; // The fieldName of the field that is being checked
  ///     self: Model; // The model instance the field is from
  ///     connection: ConnectionBase; // The current connection
  ///   }
  ///
  /// Arguments:
  ///   context: CheckDirtyContext
  ///     The context provided to the call.
  ///
  /// Return: boolean
  ///  Returns `true` if the field is dirty, or `false` otherwise.
  // eslint-disable-next-line no-unused-vars
  isDirty(context) {
  }

  /// This method is called any time a field's value is set on a model.
  ///
  /// It is only used by the <see>SerializedType</see> to update its internal
  /// "dirty" cache. It can be used by any field type however to detect when
  /// a model's field's value changes.
  ///
  /// Interface:
  ///   interface SetFieldValueContext {
  ///     value: any;
  ///     field: Field;
  ///     fieldName: string;
  ///     self: Model;
  ///   }
  ///
  /// Arguments:
  ///   context: SetFieldValueContext
  ///     The context provided to the call.
  ///
  /// Return: undefined
  ///   This method returns nothing.
  onSetFieldValue() {
  }

  /// Serialize the field's value. This is the opposite of
  /// <see>Type.deserialize</see>.
  ///
  /// This is used mostly by the <see>Model.toJSON</see> method
  /// to serialize values for JSON format. However, it is also used
  /// in some other cases, such as formatting for <see>DateType</see>
  /// and <see>DateTimeType</see>, and for checking dirtiness in the
  /// <see>SerializedType</see>.
  ///
  /// The job of this method is to serialize the field's value for any serialize operation,
  /// generally to JSON. If a `connection` is provided as the second argument,
  /// then it is assumed by the field type that it is being serialized for the
  /// database. In this case the connection generally assists with the serializing
  /// of the value. This is used for example with <see>DateType</see>
  /// and <see>DateTimeType</see> types, when they need to be serialized to or from
  /// a database operation.
  ///
  /// Arguments:
  ///   value: any
  ///     The field's value to serialize.
  ///   connection?: <see>Connection</see>
  ///     An optional connection, which if provided will likely change
  ///     how the value is serialized.
  ///
  /// Return: any | string
  ///   Though the return value will generally be a string, it isn't required
  ///   to be.
  ///
  /// See: Type.deserialize
  // eslint-disable-next-line no-unused-vars
  serialize(value, connection) {
    return value;
  }

  /// Deserialize the field's value. This is the opposite of
  /// <see>Type.serialize</see>.
  ///
  /// The job of this method to serialize the field's value for any serialize operation,
  /// generally to JSON. If a `connection` is provided as the second argument,
  /// then it is assumed by the field type that it is being serialized for the
  /// database. In this case the connection generally assists with the serializing
  /// of the value. This is used for example with <see>DateType</see>
  /// and <see>DateTimeType</see> types, when they need to be serialized to or from
  /// a database operation.
  ///
  /// Arguments:
  ///   value: any
  ///     The field's value to deserialize.
  ///   connection?: <see>Connection</see>
  ///     An optional connection, which if provided will likely change
  ///     how the value is deserialized.
  ///
  /// Return: any
  ///   The deserialized value for the field.
  ///
  /// See: Type.serialize
  // eslint-disable-next-line no-unused-vars
  deserialize(value, connection) {
    return value;
  }

  /// Convert this type to the underlying type
  /// of the database driver.
  ///
  /// This is generally called by a `QueryGenerator`
  /// instance to convert the field's type to the
  /// underlying type of the database. If a `connection`
  /// argument is provided, then it will proxy the
  /// call to <see>Connection.typeToString</see>.
  /// If no `connection` argument is provided, then
  /// it will simply return the "common" SQL string
  /// representation for this type.
  ///
  /// Return: string
  ///   A string type representing the underlying
  ///   database driver type, or the "common" SQL
  ///   type if not `connection` is provided.
  ///
  /// Arguments:
  ///   connection: <see>Connection</see>
  ///     The database driver connection this type
  ///     is being converted to.
  ///   options?: object
  ///     Any options to pass through with the type
  ///     conversion. This options object will be
  ///     passed to the <see>Connection.typeToString</see>
  ///     call.
  toConnectionType(connection, options) {
    if (!connection)
      return this.toString();

    return connection.typeToString(this, options);
  }
}

module.exports = Type;
