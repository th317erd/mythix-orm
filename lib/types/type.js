'use strict';

const { checkDefaultValueFlags }  = require('./helpers/default-helpers');

/// Base type class for all other field types.
class Type {
  static _isMythixFieldType = true;

  static clone = function() {
    return this;
  };

  static isTypeClass(value) {
    if (!value)
      return false;

    if (value.prototype instanceof Type)
      return true;

    if (value._isMythixFieldType)
      return true;

    return false;
  }

  static isType(value) {
    if (!value)
      return false;

    if (value instanceof Type)
      return true;

    if (value.constructor._isMythixFieldType)
      return true;

    return false;
  }

  static isSameType(value) {
    return (this.isType(value) && value.constructor && value.constructor.name === this.name);
  }

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
      '_modelInstance': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        null,
      },
      '_connection': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        null,
      },
      '_initialized': {
        enumerable:   false,
        configurable: true,
        get:          () => {
          return !!(this._Model && this._field);
        },
        set:          () => {},
      },
    });
  }

  clone() {
    let Type        = this.constructor;
    let newInstance = new Type(...this._args);

    newInstance._Model = this._Model;
    newInstance._field = this._field;
    newInstance._modelInstance = this._modelInstance;

    return newInstance;
  }

  isVirtual() {
    return this.constructor.isVirtual.call(this.constructor);
  }

  isRelational() {
    return this.constructor.isRelational.call(this.constructor);
  }

  isForeignKey() {
    return this.constructor.isForeignKey.call(this.constructor);
  }

  exposeToModel() {
    return this.constructor.exposeToModel.call(this.constructor);
  }

  isRemote() {
    let field = this.getField();
    if (!field)
      throw new Error(`${this.constructor.name}::isRemote: Error, the model type must be initialized before you call "isRemote".`);

    if (field.remote === true)
      return true;

    return checkDefaultValueFlags(field.defaultValue, [ 'remote' ]);
  }

  // eslint-disable-next-line no-unused-vars
  isValidValue(value) {
    return true;
  }

  getField() {
    return this._field;
  }

  setField(field) {
    this._field = field;
  }

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
  /// exists on. For example, the `Model` and `Models` type
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

  isDirty() {
  }

  onSetFieldValue() {
  }

  serialize(value) {
    return value;
  }

  deserialize(value) {
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
