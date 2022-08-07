'use strict';

const { checkDefaultValueFlags }  = require('./helpers/default-helpers');

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
        enumberable:  false,
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
        enumberable:  false,
        configurable: true,
        value:        args,
      },
      '_Model': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
      '_field': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
      '_modelInstance': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
      '_connection': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
      '_initialized': {
        enumberable:  false,
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

  castToType({ value }) {
    return value;
  }

  // eslint-disable-next-line no-unused-vars
  initialize(connection, modelInstance) {
  }

  serialize(value) {
    return value;
  }

  deserialize(value) {
    return value;
  }

  toConnectionType(connection, options) {
    if (!connection)
      return this.toString();

    return connection.typeToString(this, options);
  }
}

module.exports = Type;
