'use strict';

const Nife  = require('nife');
const UUID  = require('uuid');

function isUUID(value) {
  return UUID.validate(value);
}

function sanitizeFieldString(str) {
  return ('' + str).replace(/[^\w:.]+/g, '').replace(/([:.])+/g, '$1');
}

function parseQualifiedName(str) {
  let fieldNames = [];
  let modelName;

  let parts = sanitizeFieldString(str).split(/:/).filter(Boolean);
  if (parts.length > 1) {
    modelName = parts[0];
    fieldNames = parts.slice(1).join('').split(/\.+/g).filter(Boolean);
  } else {
    fieldNames = parts.join('').split(/\.+/g).filter(Boolean);

    if (fieldNames.length === 1 && fieldNames[0].match(/^[A-Z]/)) {
      modelName = fieldNames[0];
      fieldNames = [];
    }
  }

  return { modelName, fieldNames };
}

function injectModelMethod(modelInstance, method, methodName, fullMethodName) {
  Object.defineProperties(modelInstance, {
    [fullMethodName]: {
      writable:     true,
      enumberable:  false,
      configurable: true,
      value:        method,
    },
  });

  if (!Object.prototype.hasOwnProperty.call(modelInstance, methodName)) {
    Object.defineProperties(modelInstance, {
      [methodName]: {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        function(...args) {
          return method.apply(this, args);
        },
      },
    });
  }
}

function fieldToFullyQualifiedName(field, Model) {
  if (!field)
    return;

  if (field.Model && field.fieldName)
    return `${field.Model.getModelName()}:${field.fieldName}`;

  if (!Nife.instanceOf(field, 'string'))
    return;

  let def = parseQualifiedName(field);
  if (!def.modelName && Model)
    def.modelName = Model.getModelName();

  if (Nife.isEmpty(def.modelName) || Nife.isEmpty(def.fieldNames))
    return;

  return `${def.modelName}:${def.fieldNames[0]}`;
}

function sortModelNamesByCreationOrder(connection, modelNames) {
  const getForeignKeyTargets = (Model) => {
    let modelNames = new Set();

    Model.iterateFields(({ field }) => {
      let fieldType = field.type;
      if (!fieldType.isForeignKey())
        return;

      let TargetModel = fieldType.getTargetModel();
      if (!TargetModel)
        return;

      let targetModelName = TargetModel.getModelName();
      modelNames.add(targetModelName);
    });

    return Array.from(modelNames.values());
  };

  let modelOrderMap = {};
  for (let i = 0, il = modelNames.length; i < il; i++) {
    let modelName         = modelNames[i];
    let Model             = connection.getModel(modelName);
    let targetModelNames  = getForeignKeyTargets(Model);

    modelOrderMap[modelName] = targetModelNames;
  }

  return Object.keys(modelOrderMap).sort((a, b) => {
    if (a === b)
      return 0;

    let x = modelOrderMap[a];
    let y = modelOrderMap[b];

    let xIndex = x.indexOf(b);
    let yIndex = y.indexOf(a);

    if (xIndex >= 0 && yIndex >= 0)
      return 0;

    if (xIndex >= 0)
      return 1;

    if (yIndex >= 0)
      return -1;

    return (a < b) ? -1 : 1;
  });
}

function getRelationalModelStatusForField(connection, modelInstance, originField) {
  if (!originField)
    return;

  if (!originField.type.isRelational())
    return;

  let primaryModelName = modelInstance.getModelName();
  let relationModelMap = {
    [primaryModelName]: {
      create:     !modelInstance.isPersisted(),
      instance:   modelInstance,
      modelName:  primaryModelName,
      Model:      modelInstance.getModel(),
      fields:     new Map(),
    },
  };

  const hasValidPrimaryKey = (model) => {
    if (!model)
      return false;

    if (typeof model.hasValidPrimaryKey === 'function')
      return model.hasValidPrimaryKey();

    return false;
  };

  const addModelStatus = (Model, modelName, source, target, status) => {
    if (!Object.prototype.hasOwnProperty.call(relationModelMap, modelName))
      relationModelMap[modelName] = Object.assign(status, { Model, modelName, fields: new Map() });
    else if (status.instance)
      relationModelMap[modelName] = Object.assign(relationModelMap[modelName], status);

    if (modelName !== target.modelName)
      return;

    if (!target.fieldType.isRelational()) {
      let SourceModel     = source.fieldType.getTargetModel({ recursive: true, followForeignKeys: true });
      let sourceField     = source.fieldType.getTargetField();
      let sourceModelName = SourceModel.getModelName();

      if (target.modelName === sourceModelName && target.fieldName === sourceField.fieldName)
        return;

      relationModelMap[modelName].fields.set(`${target.modelName}:${target.fieldName}`, `${sourceModelName}:${sourceField.fieldName}`);

      return;
    }

    let TargetModel = target.fieldType.getTargetModel({ recursive: false, followForeignKeys: false });
    relationModelMap[modelName].fields.set(`${target.modelName}:${target.fieldName}`, TargetModel.getModelName());
  };

  const findAvailableRelationalModel = (modelName, field) => {
    let relation = relationModelMap[modelName];
    let instance = (relation) ? relation.instance : null;
    if (!instance)
      return;

    let value = instance[field.fieldName];
    if (Array.isArray(value))
      return; // we need to create

    return value;
  };

  const addRelationalStatus = (source, target) => {
    if (source.fieldType.isRelational() && source.fieldType.isManyRelation()) {
      // we need to create
      addModelStatus(source.Model, source.modelName, source, target, { create: true });
      addModelStatus(target.Model, target.modelName, source, target, { create: true });
      return;
    }

    let model = findAvailableRelationalModel(source.modelName, source.field);
    if (!hasValidPrimaryKey(model))
      addModelStatus(source.Model, source.modelName, source, target, { create: true, instance: model });
    else
      addModelStatus(source.Model, source.modelName, source, target, { create: false, instance: model });

    model = findAvailableRelationalModel(target.modelName, target.field);
    if (!hasValidPrimaryKey(model))
      addModelStatus(target.Model, target.modelName, source, target, { create: true, instance: model });
    else
      addModelStatus(target.Model, target.modelName, source, target, { create: false, instance: model });
  };

  originField.type.walkSourceRelation(({ source, target }) => {
    addRelationalStatus(source, target);
  }, connection);

  return relationModelMap;
}

function constructModelsForCreationFromOriginField(connection, modelInstance, originField, attributes) {
  if (!originField)
    return;

  if (!originField.type.isRelational())
    return;

  const getRelationalModel = (relationalMap, modelName) => {
    let status = relationalMap[modelName];
    return (status) ? status.instance : null;
  };

  let TargetModel       = originField.type.getTargetModel({ recursive: true, followForeignKeys: true });
  let targetModelName   = TargetModel.getModelName();
  let relationalMap     = getRelationalModelStatusForField(connection, modelInstance, originField);
  let sortedModelNames  = sortModelNamesByCreationOrder(connection, Array.from(Object.keys(relationalMap)));

  for (let i = 0, il = sortedModelNames.length; i < il; i++) {
    let modelName         = sortedModelNames[i];
    let relationStatus    = relationalMap[modelName];
    let RelatedModel      = relationStatus.Model;
    let thisModelInstance = relationStatus.instance;

    if (!(thisModelInstance instanceof RelatedModel)) {
      let modelAttributes = relationStatus.instance;
      if (modelName === targetModelName)
        modelAttributes = attributes;

      // Is this a persisted model?
      if (modelAttributes instanceof RelatedModel && modelAttributes.isPersisted())
        thisModelInstance = modelAttributes;
      else
        thisModelInstance = new RelatedModel(modelAttributes);
    }

    setRelationalValues(TargetModel, thisModelInstance, modelInstance.getModel(), modelInstance);
    relationStatus.instance = thisModelInstance;

    for (let [ targetFQField, sourceFQField ] of relationStatus.fields.entries()) {
      let targetDef = parseQualifiedName(targetFQField);
      if (Nife.isEmpty(targetDef.fieldNames))
        continue;

      let sourceDef             = parseQualifiedName(sourceFQField);
      let relatedModelInstance  = (modelName === sourceDef.modelName) ? thisModelInstance : getRelationalModel(relationalMap, sourceDef.modelName);
      if (!relatedModelInstance)
        continue;

      thisModelInstance[targetDef.fieldNames[0]] = relatedModelInstance[sourceDef.fieldNames[0]];
    }
  }

  return { relationalMap, sortedModelNames };
}

async function createAndSaveAllRelatedModels(connection, modelInstance, originField, allModelAttributes, options) {
  if (!modelInstance)
    return;

  if (!modelInstance.isPersisted())
    throw new Error('ModelUtils::createAndSaveAllRelatedModels: Parent model must be persisted before you attempt to save related child models.');

  // Find related models that need to be created
  let TargetModel     = originField.type.getTargetModel({ recursive: true, followForeignKeys: true });
  let targetModelName = TargetModel.getModelName();
  let parentModelName = modelInstance.getModelName();
  let storedModelMap  = new Map();
  let parentModelSet  = new Set();
  let relatedInfos    = [];

  parentModelSet.add(modelInstance);
  storedModelMap.set(parentModelName, parentModelSet);

  for (let i = 0, il = allModelAttributes.length; i < il; i++) {
    let modelAttributes = allModelAttributes[i];
    if (!modelAttributes)
      continue;

    let result = constructModelsForCreationFromOriginField(connection, modelInstance, originField, modelAttributes);
    relatedInfos.push(result);
  }

  // Create a map of all model instances by type
  // so that we can bulk insert models
  let fullModelMap        = new Map();
  let relationalModelMap  = new Map();
  let persistedModelMap   = new Map();
  let index               = 0;

  for (let i = 0, il = relatedInfos.length; i < il; i++) {
    let info = relatedInfos[i];
    let { relationalMap, sortedModelNames } = info;

    for (let j = 0, jl = sortedModelNames.length; j < jl; j++) {
      let modelName = sortedModelNames[j];
      let modelSet  = fullModelMap.get(modelName);
      if (!modelSet) {
        modelSet = new Set();
        fullModelMap.set(modelName, modelSet);
      }

      let thisStatus        = relationalMap[modelName];
      let thisModelInstance = thisStatus.instance;

      if (!thisModelInstance.isPersisted()) {
        thisModelInstance.__order = index;
        modelSet.add(thisModelInstance);
      } else {
        let storedSet = storedModelMap.get(modelName);
        if (!storedSet) {
          storedSet = new Set();
          storedModelMap.set(modelName, storedSet);
        }

        thisModelInstance.__order = index;
        storedSet.add(thisModelInstance);

        let persistedSet = persistedModelMap.get(modelName);
        if (!persistedSet) {
          persistedSet = new Set();
          persistedModelMap.set(modelName, persistedSet);
        }

        persistedSet.add(thisModelInstance);
      }

      index++;

      relationalModelMap.set(thisModelInstance, info);
    }
  }

  const updateRelatedModelAttributes = (SetModel, modelName, models, assignRelated) => {
    // Iterate all models and update their
    // related model attributes
    for (let model of models) {
      let relationInfo  = relationalModelMap.get(model);
      let { relationalMap, sortedModelNames } = relationInfo;

      // Assign related attributes from stored models
      for (let k = 0, kl = sortedModelNames.length; k < kl; k++) {
        let relatedModelName = sortedModelNames[k];

        // Model can't be related to itself
        if (relatedModelName === modelName)
          continue;

        let relationStatus      = relationalMap[relatedModelName];
        let SourceModel         = relationStatus.Model;
        let sourceModelInstance = relationStatus.instance;

        if (assignRelated)
          assignRelatedModels(model, [ sourceModelInstance ]);

        // Is this model related to the set model?
        // If not, simply continue
        if (!SetModel.isForeignKeyTargetModel(relatedModelName))
          continue;

        setRelationalValues(SetModel, model, SourceModel, sourceModelInstance);
      }
    }
  };

  // Create each model and all related models
  index = 0;
  for (let [ modelName, models ] of fullModelMap) {
    let SetModel = connection.getModel(modelName);

    if (index > 0) {
      // Now assign related attributes from all other
      // related models before we save
      updateRelatedModelAttributes(SetModel, modelName, models);
    }

    index++;

    if (models.size === 0)
      continue;

    // Create models
    let storedModels  = await connection.insert(SetModel, models, options);
    let storedSet     = storedModelMap.get(modelName);
    if (!storedSet) {
      storedSet = new Set();
      storedModelMap.set(modelName, storedSet);
    }

    for (let j = 0, jl = storedModels.length; j < jl; j++) {
      let storedModel = storedModels[j];
      storedSet.add(storedModel);
    }
  }

  // Now update all related fields for all stored models
  for (let [ modelName, models ] of fullModelMap) {
    let SetModel = connection.getModel(modelName);

    // Now assign related attributes from all other
    // related models before we save
    updateRelatedModelAttributes(SetModel, modelName, models, true);
  }

  // Now update all related fields for all pre-persisted models
  // Models that were already persisted before the operation
  // are split apart and treated separately
  for (let [ modelName, models ] of persistedModelMap) {
    let SetModel = connection.getModel(modelName);

    // Now assign related attributes from all other
    // related models before we save
    updateRelatedModelAttributes(SetModel, modelName, models, true);
  }

  // Finally return created models
  // Sorting final results by provided model
  // order is important, since we split out any
  // pre-persisted models and treat them separately
  let finalModels = Array.from(storedModelMap.get(targetModelName).values()).sort((a, b) => {
    let x = a.__order;
    let y = b.__order;

    if (x === y)
      return 0;

    return (x < y) ? -1 : 1;
  });

  return finalModels;
}

function setRelationalValues(Model, modelInstance, RelatedModel, relatedModelInstance) {
  let fieldsToSet       = {};
  let modelName         = Model.getModelName();
  let relatedModelName  = RelatedModel.getModelName();

  if (modelName === relatedModelName)
    return modelInstance;

  // Collect fields that are connected to the related model
  Model.iterateFields(({ field, fieldName }) => {
    let fieldType = field.type;
    if (fieldType.isRelational()) {
      let sourceField = fieldType.getSourceField({ recursive: true, followForeignKeys: true });
      if (sourceField.Model.getModelName() !== modelName)
        return;

      let targetField = fieldType.getTargetField({ recursive: true, followForeignKeys: true });
      if (targetField.Model.getModelName() !== relatedModelName)
        return;

      if (targetField.primaryKey)
        return;

      fieldsToSet[sourceField.fieldName] = targetField.fieldName;

      return;
    } else if (fieldType.isForeignKey()) {
      let targetField = fieldType.getTargetField();
      if (targetField.Model.getModelName() !== relatedModelName)
        return;

      fieldsToSet[fieldName] = targetField.fieldName;
    }
  });

  // Update fields to related model
  let fieldNames = Object.keys(fieldsToSet);
  for (let i = 0, il = fieldNames.length; i < il; i++) {
    let fieldName         = fieldNames[i];
    let relatedFieldName  = fieldsToSet[fieldName];
    let relatedModelValue = (relatedModelInstance) ? relatedModelInstance[relatedFieldName] : null;

    modelInstance[fieldName] = relatedModelValue;
  }

  return modelInstance;
}

function assignRelatedModels(model, _relatedModels) {
  if (!model || !_relatedModels)
    return;

  let relatedModels = Nife.toArray(_relatedModels);
  for (let i = 0, il = relatedModels.length; i < il; i++) {
    let relatedModel  = relatedModels[i];
    let pluralName    = relatedModel.getPluralName();
    let pkFieldName   = relatedModel.getPrimaryKeyFieldName();
    let pkValue       = (pkFieldName) ? relatedModel[pkFieldName] : null;
    let relatedScope  = model._[pluralName];

    if (!relatedScope) {
      relatedScope = model._[pluralName] = [];

      Object.defineProperty(relatedScope, 'Model', {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        relatedModel.getModel(),
      });
    }

    relatedScope.push(relatedModel);

    if (Nife.isNotEmpty(pkValue)) {
      Object.defineProperty(relatedScope, pkValue, {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        relatedModel,
      });
    }
  }
}

function getPrimaryKeysForModels(Model, _models, _options) {
  let pkFieldName = Model.getPrimaryKeyFieldName();
  if (!pkFieldName)
    return [];

  let models      = Nife.toArray(_models).filter(Boolean);
  let primaryIDs  = models.map((model) => model[pkFieldName]).filter((id) => (id != null));

  let options = _options || {};
  if (!options.includeRelations)
    return primaryIDs;

  let primaryModelName      = Model.getModelName();
  let idMap                 = { [primaryModelName]: primaryIDs };
  let providedRelationNames = (Array.isArray(options.includeRelations)) ? (options.includeRelations.filter(Boolean)) : null;

  for (let i = 0, il = models.length; i < il; i++) {
    let model = models[i];
    if (!model || !model._)
      continue;

    let relationScope = model._;
    let relationNames = (providedRelationNames) ? providedRelationNames : Object.keys(relationScope);
    for (let j = 0, jl = relationNames.length; j < jl; j++) {
      let relationName  = relationNames[j];
      let relatedModels = relationScope[relationName];
      if (!relatedModels || !relatedModels.Model)
        continue;

      let relationModelName   = relatedModels.Model.getModelName();
      let relationPKFieldName = relatedModels.Model.getPrimaryKeyFieldName();
      if (!relationPKFieldName)
        continue;

      let idScope = idMap[relationModelName];
      if (!idScope)
        idScope = idMap[relationModelName] = [];

      for (let k = 0, kl = relatedModels.length; k < kl; k++) {
        let relatedModel  = relatedModels[k];
        let id            = relatedModel[relationPKFieldName];

        if (!id)
          continue;

        if (idScope.indexOf(id) >= 0)
          continue;

        idScope.push(id);
      }
    }
  }

  return idMap;
}

module.exports = {
  assignRelatedModels,
  constructModelsForCreationFromOriginField,
  createAndSaveAllRelatedModels,
  fieldToFullyQualifiedName,
  getRelationalModelStatusForField,
  injectModelMethod,
  isUUID,
  parseQualifiedName,
  sanitizeFieldString,
  setRelationalValues,
  sortModelNamesByCreationOrder,
  getPrimaryKeysForModels,
};
