'use strict';

const MiscUtils = require('../utils/misc-utils');

class Type {
  static uninitializedType = true;

  static instantiateType(Model, modelInstance, _Type) {
    let Type = _Type;
    if (!Type)
      throw new TypeError('Type::instantiateType: Provided field "type" is empty, but "type" is required.');

    if (Type.uninitializedType) {
      Type = new Type();

      Type.setModelInstance(modelInstance);
      Type.setModel(Model);
    } else if (Type.mythixType) {
      let typeFunc = Type;
      Type = typeFunc();

      Type.setModelInstance(modelInstance);
      Type.setModel(Model);
    }

    return Type;
  }

  static isVirtual() {
    return false;
  }

  static wrapConstructor(TypeKlass) {
    let func = function(...args) {
      return new TypeKlass(...args);
    };

    Object.defineProperties(func, {
      'mythixType': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        TypeKlass,
      },
    });

    MiscUtils.copyStaticProps(TypeKlass, func);

    return func;
  }

  constructor(...args) {
    Object.defineProperties(this, {
      '_args': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        args,
      },
      '_modelInstance': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
      '_Model': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
    });
  }

  getModelInstance() {
    return this._modelInstance;
  }

  setModelInstance(modelInstance) {
    this._modelInstance = modelInstance;
  }

  getModel() {
    let Model = this._Model;
    if (!Model) {
      let modelInstance = this.getModelInstance();
      if (!modelInstance)
        return;

      Model = modelInstance.getModelClass();
    }

    return Model;
  }

  setModel(Model) {
    this._Model = Model;
  }

  castToType(params) {
    let Model = this.getModel();
    let modelInstance = this.getModelInstance();

    return this.constructor.castToType.call(
      this.constructor,
      Object.assign({}, params || {}, {
        typeInstance: this,
        connection:   (modelInstance) ? modelInstance.getConnection() : (Model && Model.getConnection()),
        Model,
        modelInstance,
      }),
    );
  }

  initialize(/* modelInstance, connection */) {
  }
}

module.exports = Type;
