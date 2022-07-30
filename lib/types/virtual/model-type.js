'use strict';

const Nife                = require('nife');
const RelationalTypeBase  = require('./relational-type-base');
const ModelUtils          = require('../../utils/model-utils');

const NAMED_METHOD  = false;
const ROOT_METHOD   = true;

// These get injected into the class as
// create{fieldName}, get{fieldName}, etc...
const INJECT_TYPE_METHODS = {
  'create': async function({ field, type }, model, options) {
    if (!model)
      return false;

    let TargetModel = type.getTargetModel({ recursive: true, followForeignKeys: true });

    return this.getConnection().transaction(async (connection) => {
      let fetchedModel;

      // TODO: This probably needs to be an outer join
      let getterMethodName = type.fieldNameToOperationName(field, 'get', NAMED_METHOD);
      fetchedModel = await this[getterMethodName](TargetModel.where.LIMIT(1), options);

      if (fetchedModel && fetchedModel instanceof TargetModel) {
        fetchedModel.setAttributes(model, true);
        if (fetchedModel.isDirty())
          await fetchedModel.save();

        return fetchedModel;
      }

      let storedModels  = await ModelUtils.createAndSaveAllRelatedModels(connection, this, field, [ model ], options);
      let resultModel   = storedModels[0];

      // Update this model to reflect the update
      ModelUtils.setRelationalValues(this.getModel(), this, resultModel.getModel(), resultModel);
      if (this.isDirty())
        await this.save();

      return resultModel;
    });
  },
  'get': async function({ field, type }, queryEngine, options) {
    let query = type.prepareQuery(this, field, queryEngine);
    return await query.first(null, options);
  },
  'update': async function({ fullMethodName, field, type }, attributes, _options) {
    return this.getConnection().transaction(async (connection) => {
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

      let [ storedModel ] = await connection.update(model.getModel(), [ model ], options);

      // Update this model to reflect the update
      ModelUtils.setRelationalValues(this.getModel(), this, storedModel.getModel(), storedModel);
      if (this.isDirty())
        await this.save();

      return storedModel;
    });
  },
  'destroy': async function({ field, type }, options) {
    return this.getConnection().transaction(async (connection) => {
      let query = type.prepareQuery(this, field);
      let model = await query.first(options);

      if (!model)
        return false;

      await connection.destroy(model.getModel(), [ model ], options);

      // Update this model to reflect the deletion
      ModelUtils.setRelationalValues(this.getModel(), this, model.getModel());
      if (this.isDirty())
        await this.save();

      return true;
    });
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
