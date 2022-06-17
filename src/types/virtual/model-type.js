'use strict';

const Nife                = require('nife');
const RelationalTypeBase  = require('./relational-type-base');
const ModelUtils          = require('../../utils/model-utils');

// These get injected into the class as
// i.e. create{fieldName}
// get{fieldName}
const INJECT_TYPE_METHODS = {
  'create': async function(field, type, model, options) {
    if (!model)
      return false;

    let targetModel = type.getTargetModel();
    let connection  = this.getConnection();
    let result      = await connection.insert(targetModel, [ model ], options);

    let sourceField = type.getSourceField(connection, true);
    let targetField = type.getTargetField(connection, true);
    if (targetField && sourceField && targetModel.getModelName() === targetField.Model.getModelName()) {
      this[sourceField.fieldName] = result[0][targetField.fieldName];
      await this.save();
    }

    return true;
  },
  'get': async function(field, type, queryEngine, options) {
    let query = type.prepareQuery(this, field, queryEngine);
    return await query.first(options);
  },
  'update': async function(field, type, attributes, options) {
    let query = type.prepareQuery(this, field);
    let model = await query.first(options);

    if (!model)
      return false;

    model.setAttributes(attributes, true);

    let connection = this.getConnection();
    await connection.update(model.getModel(), [ model ], options);

    return true;
  },
  'destroy': async function(field, type, options) {
    let query = type.prepareQuery(this, field);
    let model = await query.first(options);

    if (!model)
      return false;

    let connection = this.getConnection();
    await connection.destroy(model.getModel(), [ model ], options);

    let sourceField = type.getSourceField(connection, true);
    if (sourceField && sourceField.Model.getModelName() === this.getModelName()) {
      this[sourceField.fieldName] = null;
      await this.save();
    }

    return true;
  },
  'exists': async function(field, type, options) {
    let query = type.prepareQuery(this, field);
    return await query.exists(options);
  },
};

const INJECT_TYPE_METHODS_KEYS = Object.keys(INJECT_TYPE_METHODS);

class ModelType extends RelationalTypeBase {
  castToType({ value, typeInstance, connection }) {
    if (!typeInstance)
      throw new TypeError('ModelType::castToType: Type instance is required to cast.');

    if (value == null)
      return value;

    let Model = typeInstance.getTargetModel(connection);
    if (!Model)
      throw new TypeError('ModelType::castToType: Failed when attempting to fetch the required model.');

    if (value instanceof Model)
      return value;

    if (!Nife.instanceOf(value, 'object'))
      throw new TypeError('ModelType::castToType: Unable to cast provided value. Value must be a model instance, or a raw object.');

    return new Model(value);
  }

  isManyRelation() {
    return false;
  }

  onModelInstantiated(modelInstance, field) {
    let fieldName = field.fieldName;
    for (let i = 0, il = INJECT_TYPE_METHODS_KEYS.length; i < il; i++) {
      let key             = INJECT_TYPE_METHODS_KEYS[i];
      let method          = INJECT_TYPE_METHODS[key];
      let methodName      = `${key}${Nife.capitalize(fieldName)}`;
      let fullMethodName  = `__${key}${Nife.capitalize(fieldName)}`;

      ModelUtils.injectModelMethod(modelInstance, method.bind(modelInstance, field, this), methodName, fullMethodName);
    }
  }
}

module.exports = {
  Model: RelationalTypeBase.wrapConstructor(ModelType),
  ModelType,
};
