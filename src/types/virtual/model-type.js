'use strict';

const Nife                = require('nife');
const RelationalTypeBase  = require('./relational-type-base');
const ModelUtils          = require('../../utils/model-utils');

const NAMED_METHOD  = false;
const ROOT_METHOD   = true;

// These get injected into the class as
// create{fieldName}, get{fieldName}, etc...
const INJECT_TYPE_METHODS = {
  'create': async function({ field, type }, model, _options) {
    if (!model)
      return false;

    let options         = (_options === true) ? { update: true } : (_options || {});
    let targetModel     = type.getTargetModel({ recursive: true, followForeignKeys: true });
    let targetModelName = targetModel.getModelName();
    let connection      = this.getConnection();
    let fetchedModel;

    // TODO: This probably needs to be an outer join
    let getterMethodName = type.fieldNameToOperationName(field, 'get', NAMED_METHOD);
    fetchedModel = await this[getterMethodName](targetModel.where.LIMIT(1), options);

    if (fetchedModel && fetchedModel instanceof targetModel) {
      if (options.update) {
        fetchedModel.setAttributes(model, true);
        if (fetchedModel.isDirty())
          await fetchedModel.save();
      }

      return fetchedModel;
    }

    let {
      relationsStatus,
      modelCreationOrder,
    } = type._getModelCreationInfo(this, field, targetModel, fetchedModel, connection);

    // Create and store any models that don't already exist
    for (let i = 0, il = modelCreationOrder.length; i < il; i++) {
      let modelNameToCreate = modelCreationOrder[i];
      let Model             = connection.getModel(modelNameToCreate);
      let modelAttributes   = {};

      if (modelNameToCreate === targetModelName) {
        if (typeof model.getAttributes === 'function')
          modelAttributes = model.getAttributes();
        else
          modelAttributes = model;
      }

      type._updateValuesToRelated(Model, modelAttributes, relationsStatus, connection);

      let storedModel = await Model.create(modelAttributes, options);
      let status      = relationsStatus[modelNameToCreate];

      status.create = false;
      status.value = storedModel;
    }

    // Now update any remaining relational values of the target model
    let storedTargetModel = relationsStatus[targetModelName].value;
    type._updateValuesToRelated(targetModel, storedTargetModel, relationsStatus, connection);
    if (storedTargetModel.isDirty())
      await storedTargetModel.save();

    // Now update any remaining relational values on this model
    type._updateValuesToRelated(this.getModel(), this, relationsStatus, connection);
    if (this.isDirty())
      await this.save();

    return storedTargetModel;
  },
  'get': async function({ field, type }, queryEngine, options) {
    let query = type.prepareQuery(this, field, queryEngine);
    return await query.first(null, options);
  },
  'update': async function({ fullMethodName, field, type }, attributes, _options) {
    let options = _options || {};
    let query   = type.prepareQuery(this, field);
    let model   = await query.first(null, options);

    if (!model) {
      if (options.force === true) {
        let creatorMethodName = type.fieldNameToOperationName(field, 'create', NAMED_METHOD);
        return await this[creatorMethodName](attributes, options);
      }

      throw new Error(`${field.Model.getModelName}::${fullMethodName}: Model not found to update. You can pass "{ force: true }" to the options to force a creation instead.`);
    }

    model.setAttributes(attributes, true);
    if (!model.isDirty())
      return model;

    let connection      = this.getConnection();
    let [ storedModel ] = await connection.update(model.getModel(), [ model ], options);

    // Update this model to reflect the update
    type.setRelationalValues(this.getModel(), this, storedModel.getModel(), storedModel);
    if (this.isDirty())
      await this.save();

    return storedModel;
  },
  'destroy': async function({ field, type }, options) {
    let query = type.prepareQuery(this, field);
    let model = await query.first(options);

    if (!model)
      return false;

    let connection = this.getConnection();
    await connection.destroy(model.getModel(), [ model ], options);

    // Update this model to reflect the deletion
    type.setRelationalValues(this.getModel(), this, model.getModel());
    if (this.isDirty())
      await this.save();

    return true;
  },
  'exists': async function({ field, type }, options) {
    let query = type.prepareQuery(this, field);
    return await query.exists(options);
  },
};

const INJECT_TYPE_METHODS_KEYS = Object.keys(INJECT_TYPE_METHODS);

class ModelType extends RelationalTypeBase {
  castToType({ value, connection }) {
    if (value == null)
      return value;

    let Model = this.getTargetModel({ recursive: true, followForeignKeys: true }, connection);
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

  fieldNameToOperationName(field, operation, rootMethod) {
    if (rootMethod)
      return `__${operation}${Nife.capitalize(field.fieldName)}`;
    else
      return `${operation}${Nife.capitalize(field.fieldName)}`;
  }

  onModelInstantiated(modelInstance, field) {
    for (let i = 0, il = INJECT_TYPE_METHODS_KEYS.length; i < il; i++) {
      let operation       = INJECT_TYPE_METHODS_KEYS[i];
      let method          = INJECT_TYPE_METHODS[operation];
      let methodName      = this.fieldNameToOperationName(field, operation, NAMED_METHOD);
      let fullMethodName  = this.fieldNameToOperationName(field, operation, ROOT_METHOD);

      ModelUtils.injectModelMethod(modelInstance, method.bind(modelInstance, { methodName, fullMethodName, field, type: this }), methodName, fullMethodName);
    }
  }
}

module.exports = {
  Model: RelationalTypeBase.wrapConstructor(ModelType),
  ModelType,
};
