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

    let fieldPart = parts.slice(1).join('');
    if (fieldPart !== '*')
      fieldNames = fieldPart.split(/\.+/g).filter(Boolean);
  } else {
    if ((/^[A-Z]/).test('' + parts[0]))
      return { modelName: parts[0], fieldNames: [] };

    fieldNames = parts.join('').split(/\.+/g).filter(Boolean);

    if (fieldNames.length === 1 && fieldNames[0].match(/^[A-Z]/)) {
      modelName = fieldNames[0];
      fieldNames = [];
    }
  }

  return { modelName, fieldNames };
}

function injectModelMethod(self, method, methodName, fullMethodName) {
  Object.defineProperties(self, {
    [fullMethodName]: {
      writable:     true,
      enumberable:  false,
      configurable: true,
      value:        method,
    },
  });

  if (!(methodName in self)) {
    Object.defineProperties(self, {
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

function sortModelNamesByDependencyOrder(connection, modelNames, dependencyHelper) {
  const adjustModelScores = (orderMap, relatedModelNames) => {
    for (let i = 0, il = relatedModelNames.length; i < il; i++) {
      let modelName   = relatedModelNames[i];
      let orderNumber = orderMap.get(modelName);

      if (orderNumber == null) {
        // eslint-disable-next-line no-magic-numbers
        orderNumber = 0;
      }

      orderMap.set(modelName, orderNumber - 1);
    }
  };

  let modelOrderMap = {};
  for (let i = 0, il = modelNames.length; i < il; i++) {
    let modelName           = modelNames[i];
    let Model               = connection.getModel(modelName);
    let dependentModelNames = dependencyHelper(Model, modelName);

    modelOrderMap[modelName] = dependentModelNames;
  }

  let orderMap = new Map();
  for (let i = 0, il = modelNames.length; i < il; i++) {
    let modelName   = modelNames[i];
    let modelOrder  = modelOrderMap[modelName];
    let orderNumber = orderMap.get(modelName);

    if (orderNumber == null)
      orderMap.set(modelName, 0);

    adjustModelScores(orderMap, modelOrder);
  }

  return modelNames.sort((a, b) => {
    if (a === b)
      return 0;

    let x = orderMap.get(a);
    let y = orderMap.get(b);

    if (x === y) {
      if (a === b)
        return 0;

      return (a < b) ? -1 : 1;
    }

    return (x < y) ? -1 : 1;
  });
}

function sortModelNamesByCreationOrder(connection, modelNames) {
  const getForeignKeyTargets = (Model) => {
    let relatedModelNames = new Set();

    Model.iterateFields(({ field }) => {
      let fieldType = field.type;
      if (!fieldType.isForeignKey())
        return;

      let TargetModel = fieldType.getTargetModel(connection);
      if (!TargetModel)
        return;

      let targetModelName = TargetModel.getModelName();
      relatedModelNames.add(targetModelName);
    });

    return Array.from(relatedModelNames.values());
  };

  return sortModelNamesByDependencyOrder(connection, modelNames, getForeignKeyTargets);
}

async function getRelationalModelStatusForField(connection, self, originField) {
  if (!originField)
    return;

  if (!originField.type.isRelational())
    return;

  let primaryModelName = self.getModelName();
  let relationModelMap = {
    [primaryModelName]: {
      create:     !self.isPersisted(),
      instance:   self,
      modelName:  primaryModelName,
      Model:      self.getModel(),
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

  const addModelStatus = async (Model, modelName, source, target, status) => {
    if (!Object.prototype.hasOwnProperty.call(relationModelMap, modelName))
      relationModelMap[modelName] = Object.assign(status, { Model, modelName, fields: new Map() });
    else if (status.instance)
      relationModelMap[modelName] = Object.assign(relationModelMap[modelName], status);

    if (modelName !== target.modelName)
      return;

    if (!target.fieldType.isRelational()) {
      let TargetModel     = source.fieldType.getTargetModel(connection);
      let targetField     = source.fieldType.getTargetField(connection);
      let sourceModelName = TargetModel.getModelName();

      if (target.modelName === sourceModelName && target.fieldName === targetField.fieldName)
        return;

      relationModelMap[modelName].fields.set(`${target.modelName}:${target.fieldName}`, `${sourceModelName}:${targetField.fieldName}`);

      return;
    }

    let TargetModel = await target.fieldType.getTargetModel(connection, { self, originField });
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

  const addRelationalStatus = async (source, target) => {
    if (source.fieldType.isRelational() && source.fieldType.isManyRelation()) {
      // we need to create
      await addModelStatus(source.Model, source.modelName, source, target, { create: true });
      await addModelStatus(target.Model, target.modelName, source, target, { create: true });
      return;
    }

    let model = findAvailableRelationalModel(source.modelName, source.field);
    if (!hasValidPrimaryKey(model))
      await addModelStatus(source.Model, source.modelName, source, target, { create: true, instance: model });
    else
      await addModelStatus(source.Model, source.modelName, source, target, { create: false, instance: model });

    model = findAvailableRelationalModel(target.modelName, target.field);
    if (!hasValidPrimaryKey(model))
      await addModelStatus(target.Model, target.modelName, source, target, { create: true, instance: model });
    else
      await addModelStatus(target.Model, target.modelName, source, target, { create: false, instance: model });
  };

  await originField.type.walkSourceRelation(connection, async ({ source, target }) => {
    await addRelationalStatus(source, target);
  });

  return relationModelMap;
}

async function constructModelsForCreationFromOriginField(connection, self, originField, attributes) {
  if (!originField)
    return;

  if (!originField.type.isRelational())
    return;

  const getRelationalModel = (relationalMap, modelName) => {
    let status = relationalMap[modelName];
    return (status) ? status.instance : null;
  };

  let TargetModel       = await originField.type.getTargetModel(connection, { self, originField });
  let targetModelName   = TargetModel.getModelName();
  let relationalMap     = await getRelationalModelStatusForField(connection, self, originField);
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
        thisModelInstance = new RelatedModel(modelAttributes, { connection });
    }

    setRelationalValues(connection, TargetModel, thisModelInstance, self.getModel(), self);
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

async function createAndSaveAllRelatedModels(connection, self, originField, allModelAttributes, options) {
  if (!self)
    return;

  if (!self.isPersisted())
    throw new Error('ModelUtils::createAndSaveAllRelatedModels: Parent model must be persisted before you attempt to save related child models.');

  // Find related models that need to be created
  let TargetModel     = await originField.type.getTargetModel(connection, { self, originField });
  let targetModelName = TargetModel.getModelName();
  let parentModelName = self.getModelName();
  let storedModelMap  = new Map();
  let parentModelSet  = new Set();
  let relatedInfos    = [];

  parentModelSet.add(self);
  storedModelMap.set(parentModelName, parentModelSet);

  for (let i = 0, il = allModelAttributes.length; i < il; i++) {
    let modelAttributes = allModelAttributes[i];
    if (!modelAttributes)
      continue;

    let result = await constructModelsForCreationFromOriginField(connection, self, originField, modelAttributes);
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
        if (!SetModel.isForeignKeyTargetModel(connection, relatedModelName))
          continue;

        setRelationalValues(connection, SetModel, model, SourceModel, sourceModelInstance);
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

function setRelationalValues(connection, TargetModel, targetModelInstance, SourceModel, sourceModelInstance) {
  let targetModelName   = TargetModel.getModelName();
  let sourceModelName   = SourceModel.getModelName();

  if (targetModelName === sourceModelName)
    return targetModelInstance;

  let fieldSets = TargetModel.getForeignKeysTargetFieldNames(connection, sourceModelName);

  // Update fields to related model
  for (let i = 0, il = fieldSets.length; i < il; i++) {
    let fieldSet = fieldSets[i];

    // This might be confusing... the right hand side is
    // pulling from the "source" table in the relationship
    // (aka the "child" relationship), whereas we are pulling
    // from the "target" or "parent" relationship on the left.
    // Source table -> target model (copy destination).
    let targetFieldName   = fieldSet.sourceFieldName;
    let sourceFieldName   = fieldSet.targetFieldName;
    let sourceModelValue  = (sourceModelInstance) ? sourceModelInstance[sourceFieldName] : null;

    targetModelInstance[targetFieldName] = sourceModelValue;
  }

  return targetModelInstance;
}

function assignRelatedModels(model, _relatedModels) {
  if (!model || !_relatedModels)
    return;

  let relatedModels = Nife.toArray(_relatedModels);
  for (let i = 0, il = relatedModels.length; i < il; i++) {
    let relatedModel  = relatedModels[i];
    let pluralName    = relatedModel.getPluralModelName();
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

function buildQueryFromModelsAttributes(connection, Model, _models) {
  let models = Nife.toArray(_models);
  if (Nife.isEmpty(models))
    return;

  let isValidQuery        = false;
  let query               = Model.where(connection);
  let concreteFieldNames  = [];

  Model.iterateFields(({ fieldName, field }) => {
    if (field.type.isVirtual())
      return;

    concreteFieldNames.push(fieldName);
  });

  if (concreteFieldNames.length === 0)
    return;

  for (let i = 0, il = models.length; i < il; i++) {
    let model = models[i];
    if (!model)
      continue;

    let subQuery = Model.where(connection);
    for (let j = 0, jl = concreteFieldNames.length; j < jl; j++) {
      let fieldName = concreteFieldNames[j];
      let value = model[fieldName];
      if (value == null)
        continue;

      isValidQuery = true;
      subQuery = subQuery.AND[fieldName].EQ(value);
    }

    query = query.AND(subQuery);
  }

  if (!isValidQuery)
    return;

  return query;
}

module.exports = {
  assignRelatedModels,
  buildQueryFromModelsAttributes,
  constructModelsForCreationFromOriginField,
  createAndSaveAllRelatedModels,
  fieldToFullyQualifiedName,
  getPrimaryKeysForModels,
  getRelationalModelStatusForField,
  injectModelMethod,
  isUUID,
  parseQualifiedName,
  sanitizeFieldString,
  setRelationalValues,
  sortModelNamesByCreationOrder,
  sortModelNamesByDependencyOrder,
};
