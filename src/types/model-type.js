'use strict';

const Nife = require('nife');
const Type = require('./type');

class ModelType extends Type {
  static castToType(value, modelInstance, connection) {
    if (!modelInstance)
      throw new TypeError('ModelType::castToType: Model instance is required to cast.');

    let Model = modelInstance.getTargetModel(connection);
    if (value instanceof Model)
      return value;

    if (!Nife.instanceOf(value, 'object'))
      throw new TypeError('ModelType::castToType: Unable to cast provided value. Value must be a model instance, or a raw object.');

    return new Model(value);
  }

  static isVirtual() {
    return true;
  }

  getTargetModel(connection) {
    if (!connection)
      throw new TypeError('ModelType::getTargetModel: Must have a valid "connection" to get the requested model.');

    let modelName = this.getTargetModelName();
    return connection.getModel(modelName);
  }

  getTargetModelName() {
    return this._targetModelName;
  }

  getThroughModelNames() {
    return this._throughModelNames;
  }

  toString() {
    return '';
  }
}

module.exports = {
  MODEL: Type.wrapConstructor(ModelType),
  ModelType,
};
