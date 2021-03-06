'use strict';

const { checkDefaultValueFlags }  = require('./helpers/default-helpers');
const MiscUtils                   = require('../utils/misc-utils');

class Type {
  static uninitializedType = true;

  static clone() {
    return this;
  }

  static instantiateType(Model, modelInstance, field, _Type) {
    let Type = _Type;
    if (!Type)
      throw new TypeError('Type::instantiateType: Provided field "type" is empty, but "type" is required.');

    if (Type.uninitializedType) {
      Type = new Type();
    } else if (Type.mythixType) {
      let typeFunc = Type;
      Type = typeFunc();
    }

    if (Type._initialized !== true)
      Type.initialize(Model, modelInstance, field);

    return Type;
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

  // eslint-disable-next-line no-unused-vars
  static onModelInitialize(Model, field, type, connection) {
  }

  static wrapConstructor(TypeKlass) {
    let TypeWrapper = function(...args) {
      return new TypeKlass(...args);
    };

    Object.defineProperties(TypeWrapper, {
      'mythixType': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        TypeKlass,
      },
    });

    MiscUtils.copyStaticProps(TypeKlass, TypeWrapper);

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
      '_modelInstance': {
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
      '_initialized': {
        enumberable:  false,
        configurable: true,
        get:          () => {
          return !!(this._Model && this._modelInstance && this._field);
        },
        set:          () => {},
      },
    });
  }

  clone() {
    return new this.constructor(...this._args);
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

  onModelInitialize(...args) {
    return this.constructor.onModelInitialize.call(this.constructor, ...args);
  }

  getModelInstance() {
    return this._modelInstance;
  }

  setModelInstance(modelInstance) {
    this._modelInstance = modelInstance;
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
      let modelInstance = this.getModelInstance();
      if (!modelInstance)
        return;

      Model = modelInstance.getModel();
    }

    return Model;
  }

  setModel(Model) {
    this._Model = Model;
  }

  getConnection() {
    let modelInstance = this.getModelInstance();
    if (!modelInstance) {
      let Model = this.getModel();
      if (Model)
        return Model.getConnection();

      return null;
    }

    return modelInstance.getConnection();
  }

  _castToType(params) {
    let Model         = this.getModel();
    let modelInstance = this.getModelInstance();

    return this.castToType.call(
      this,
      Object.assign({}, params || {}, {
        typeInstance: this,
        connection:   this.getConnection(),
        Model,
        modelInstance,
      }),
    );
  }

  castToType({ value }) {
    return value;
  }

  initialize(Model, modelInstance, field) {
    if (!Model)
      throw new TypeError(`${this.constructor.name}::initialize: "Model" is required.`);

    if (!modelInstance)
      throw new TypeError(`${this.constructor.name}::initialize: "modelInstance" is required.`);

    if (!field)
      throw new TypeError(`${this.constructor.name}::initialize: "field" is required.`);

    this.setModel(Model);
    this.setModelInstance(modelInstance);
    this.setField(field);
  }

  onModelInstantiated(/* modelInstance, field, type */) {
  }

  serialize(value) {
    return value;
  }

  deserialize(value) {
    return value;
  }
}

module.exports = Type;
