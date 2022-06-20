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

    let options         = _options || {};
    let targetModel     = type.getTargetModel({ recursive: true, followForeignKeys: true });
    let targetModelName = targetModel.getModelName();
    let connection      = this.getConnection();
    let fetchedModel;

    if (options.force !== true) {
      // TODO: This probably needs to be an outer join
      let getterMethodName = type.fieldNameToOperationName(field, 'get', NAMED_METHOD);
      fetchedModel = await this[getterMethodName](targetModel.where.LIMIT(1), options);
    }

    let {
      relationsStatus,
      modelCreationOrder,
    } = type.getModelCreationInfo(this, field, targetModel, fetchedModel, connection);

    // Update a single models attributes to
    // any relational field value that points
    // to this model
    const updateValuesToRelated = (Model, modelAttributes, thisModelName) => {
      let modelNames = Object.keys(relationsStatus);
      for (let i = 0, il = modelNames.length; i < il; i++) {
        let modelName = modelNames[i];
        if (thisModelName === modelName)
          continue;

        let status = relationsStatus[modelName];
        if (status.create)
          continue;

        let RelatedModel          = connection.getModel(modelName);
        let relatedModelInstance  = status.value;

        type.setRelationalValues(Model, modelAttributes, RelatedModel, relatedModelInstance);
      }
    };

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

      updateValuesToRelated(Model, modelAttributes, modelNameToCreate);

      let storedModel = await Model.create(modelAttributes, options);
      let status      = relationsStatus[modelNameToCreate];

      status.create = false;
      status.value = storedModel;
    }

    // Now update any remaining relational values of the target model
    let storedTargetModel = relationsStatus[targetModelName].value;
    updateValuesToRelated(targetModel, storedTargetModel, targetModelName);
    if (storedTargetModel.isDirty())
      await storedTargetModel.save();

    // Now update any remaining relational values on this model
    updateValuesToRelated(this.getModel(), this, this.getModel().getModelName());
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
  castToType({ value, typeInstance, connection }) {
    if (!typeInstance)
      throw new TypeError('ModelType::castToType: Type instance is required to cast.');

    if (value == null)
      return value;

    let Model = typeInstance.getTargetModel({ recursive: true }, connection);
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

  getModelCreationInfo(modelInstance, originField, targetModel, fetchedModel, connection) {
    let relationMap     = {};
    let relationsStatus = {
      [modelInstance.getModelName()]: { create: false, value: modelInstance, modelName: modelInstance.getModelName() },
    };

    const addModelStatus = (modelName, status) => {
      relationsStatus[modelName] = Object.assign(status, { modelName });
    };

    const findModelStatus = (modelName) => {
      return relationsStatus[modelName];
    };

    const hasModelStatus = (modelName) => {
      let status = findModelStatus(modelName);
      return !!status;
    };

    const addRelationalStatus = (type, Model, fieldType) => {
      let modelName = Model.getModelName();

      if (fieldType.isRelational() && fieldType.isManyRelation()) {
        if (hasModelStatus(modelName))
          return;

        // we must create
        addModelStatus(modelName, { create: true, value: Model });
        return;
      } else {
        // we might already have
        addSingleRelationStatus(type, Model, fieldType);
      }
    };

    const addSingleRelationStatus = (type, Model, fieldType) => {
      let modelName = Model.getModelName();
      if (hasModelStatus(modelName))
        return;

      let model = findLoadedRelationalModel(Model.getPluralName());
      if (!model)
        addModelStatus(modelName, { create: true, value: Model });
      else
        addModelStatus(modelName, { create: false, value: model });
    };

    const findLoadedRelationalModel = (pluralModelName) => {
      if (!fetchedModel)
        return;

      if (targetModel.getPluralName() === pluralModelName)
        return fetchedModel;

      let relationName = Nife.uncapitalize(pluralModelName);
      let value = fetchedModel._[relationName];

      if (Array.isArray(value)) {
        if (value.length === 1)
          return value[0];
        else
          return; // we need to create
      }

      return value;
    };

    originField.type.walkSourceRelation(({ source, target }) => {
      let { field: sourceField, fieldType: sourceFieldType } = source;
      let { field: targetField, fieldType: targetFieldType } = target;
      let sourceModelName = sourceField.Model.getModelName();
      let targetModelName = targetField.Model.getModelName();

      if (sourceModelName !== targetModelName) {
        let sourceTargets = relationMap[sourceModelName];
        if (!sourceTargets)
          sourceTargets = relationMap[sourceModelName] = [];

        if (sourceTargets.indexOf(targetModelName) < 0)
          sourceTargets.push(targetModelName);
      }

      addRelationalStatus('source', sourceField.Model, sourceFieldType);
      addRelationalStatus('target', targetField.Model, targetFieldType);
    }, connection);

    let modelCreationOrder = Object.keys(relationsStatus).filter((status) => relationsStatus[status].create);
    modelCreationOrder = modelCreationOrder.sort((a, b) => {
      let relationA = relationMap[a];
      let relationB = relationMap[b];

      if (!relationA && !relationB)
        return 0;

      if (!relationA)
        return -1;

      if (!relationB)
        return 1;

      if (relationA.indexOf(b) && relationB.indexOf(a))
        return 0;

      if (relationA.indexOf(b) >= 0)
        return 1;

      if (relationB.indexOf(a) >= 0)
        return -1;

      return 0;
    });

    return { relationsStatus, relationMap, modelCreationOrder };
  }
}

module.exports = {
  Model: RelationalTypeBase.wrapConstructor(ModelType),
  ModelType,
};
