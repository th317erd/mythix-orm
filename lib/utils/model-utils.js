///! DocScope: ModelUtils

'use strict';

const Nife  = require('nife');
const UUID  = require('uuid');

function isUUID(value) {
  return UUID.validate(value);
}

function sanitizeFieldString(str) {
  return ('' + str).replace(/[^\w:.]+/g, '').replace(/([:.])+/g, '$1');
}

/// Parse a fully qualified field name.
///
/// A fully qualified field name is a field name that
/// is prefixed by its parent model. For example,
/// `'User:email'` is a fully qualified field name,
/// whereas just `'email'` is not.
///
/// This method will actually parse both formats however.
/// It will parse fully qualified field names, and short-hand
/// field names (just the field name). If just a field name
/// is provided with no model name prefix, then the resulting
/// `modelName` property of the returned object will be `undefined`.
///
/// Field names themselves can be deeply nested. For example,
/// `'User:metadata.ipAddress'`. Field nesting was designed into
/// the Mythix ORM framework, and is partially supported in some
/// areas of the framework. However, field nesting likely won't work
/// in many places without further support being added. This feature
/// was left in-place for now, because it will likely be expanded upon
/// in the future.
/// Because of this "field nesting", instead of just one field name being
/// returned, an array of field names is *always* returned. Generally,
/// to get the field name you are looking for you would just get the first
/// index (`fieldNames[0]`) on the returned object.
///
/// This method can and will also parse just a model name, so long as the provided
/// name starts with an upper-case letter. If no colon is found (meaning no model
/// prefix is present), then the provided value will be assumed to be a model name
/// only if it starts with an upper-case letter. Otherwise, it is treated as a field
/// name, and returned in the `fieldNames` array.
///
/// Arguments:
///   fieldName: string
///     A model name, field name, or fully qualified field name to parse.
///
/// Return: object
///   An object with the shape `{ modelName: string | undefined; fieldNames: Array<string> }`.
///   The `modelName` property will contain the name of the model for the field, if it is known,
///   which will only be the case for a fully qualified name (i.e. `'User:email'`), or a
///   model name alone (i.e. `'User'`). `fieldNames` will always be an array of field names
///   parsed, which will generally only be one. It can be empty if no field name was provided,
///   for example if only a model name was provided (i.e. `'User'`).
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
      enumerable:   false,
      configurable: true,
      value:        method,
    },
  });

  if (!(methodName in self)) {
    Object.defineProperties(self, {
      [methodName]: {
        writable:     true,
        enumerable:   false,
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

function sortModelNamesByDependencyOrder(connection, _modelNames, dependencyHelper) {
  const recursiveAdd = (modelName, _depth) => {
    if (modelNames.indexOf(modelName) < 0)
      return;

    let depth = _depth || 0;

    // eslint-disable-next-line no-magic-numbers
    if (depth > 10)
      throw new Error(`ModelUtils::sortModelNamesByDependencyOrder: Cyclic dependency detected with model "${modelName}".`);

    if (finalOrder.has(modelName))
      return;

    let dependentModelNames = modelOrderMap[modelName];
    if (dependentModelNames == null) {
      let Model = connection.getModel(modelName);
      dependentModelNames = modelOrderMap[modelName] = (dependencyHelper(Model, modelName) || []);
    }

    if (dependentModelNames.length === 0) {
      finalOrder.set(modelName, dependentModelNames);
      return;
    }

    for (let j = 0, jl = dependentModelNames.length; j < jl; j++) {
      let dependentModelName = dependentModelNames[j];
      recursiveAdd(dependentModelName, depth + 1);
    }

    finalOrder.set(modelName, dependentModelNames);
  };

  let modelOrderMap = {};
  let modelNames    = Nife.toArray(_modelNames).slice().sort();
  let finalOrder    = new Map();

  for (let i = 0, il = modelNames.length; i < il; i++) {
    let modelName = modelNames[i];
    if (!modelName)
      continue;

    recursiveAdd(modelName);
  }

  let sortedModelNames = Array.from(finalOrder.keys());
  return sortedModelNames;
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

async function getRelationalModelStatusForField(connection, self, field, ...args) {
  if (!field || !field.type.isRelational())
    return { relationalMap: {} };

  let PrimaryModel      = self.getModel();
  let primaryModelName  = self.getModelName();
  let relationalMap  = {
    [primaryModelName]: {
      create:     !self.isPersisted(),
      instance:   self,
      modelName:  primaryModelName,
      Model:      PrimaryModel,
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

  const addModelStatus = (relation, opposingRelation, swapFieldOrder, _status) => {
    if (relation.field && relation.field.type.isRelational() && relation.field.type.isManyRelation())
      return;

    if (opposingRelation.field && opposingRelation.field.type.isRelational() && opposingRelation.field.type.isManyRelation())
      return;

    let status                    = _status || {};
    let relationFieldName         = (relation.fieldName) ? `:${relation.fieldName}` : '';
    let relationFQName            = `${relation.modelName}${relationFieldName}`;
    let opposingRelationFieldName = (opposingRelation.fieldName) ? `:${opposingRelation.fieldName}` : '';
    let opposingRelationFQName    = `${opposingRelation.modelName}${opposingRelationFieldName}`;
    let fields;

    let modelRelation = relationalMap[relation.modelName];
    if (!modelRelation) {
      fields = new Map();
      relationalMap[relation.modelName] = Object.assign(status, { create: !status.instance, Model: relation.Model, modelName: relation.modelName }, { fields });
    } else {
      let instance = modelRelation.instance || status.instance;

      fields = modelRelation.fields || new Map();
      relationalMap[relation.modelName] = Object.assign(relationalMap[relation.modelName], status, { create: !instance, instance, fields });
    }

    // this key/value pair must be flipped so the
    // parent faces the child. This is so that the field
    // set can hold many model types and fields.
    fields.set((swapFieldOrder) ? relationFQName : opposingRelationFQName, (swapFieldOrder) ? opposingRelationFQName : relationFQName);
  };

  const findAvailableRelationalModel = (modelName, field) => {
    if (modelName === primaryModelName)
      return self;

    if (!field)
      return;

    if (!field.type.isRelational())
      return;

    // Don't attempt multi-relational fields
    if (field.type.isManyRelation())
      return;

    let relation = relationalMap[modelName];
    let instance = (relation) ? relation.instance : null;
    if (!instance)
      return;

    let value = instance[field.fieldName];
    if (Array.isArray(value))
      return; // we need to create

    return value;
  };

  const addRelationalStatus = async (source, target) => {
    if (field.type.isManyRelation()) {
      // we need to create
      addModelStatus(source, target);
      addModelStatus(target, source);
      return;
    }

    let model = findAvailableRelationalModel(source.modelName, source.field);
    if (!hasValidPrimaryKey(model))
      addModelStatus(source, target, { instance: model });
    else
      addModelStatus(source, target, { instance: model });

    model = findAvailableRelationalModel(target.modelName, target.field);
    if (!hasValidPrimaryKey(model))
      addModelStatus(target, source, { instance: model });
    else
      addModelStatus(target, source, { instance: model });
  };

  let TargetModel;
  let TargetField;

  await field.type.walkQueryRelations(connection, async ({ TargetModel: _TargetModel, TargetField: _TargetField, source, target }) => {
    if (!TargetModel)
      TargetModel = _TargetModel;

    if (!TargetField)
      TargetField = _TargetField;

    await addRelationalStatus(source, target);
  }, { self, field: field }, ...args);

  return { relationalMap, TargetModel, TargetField };
}

async function constructModelsForCreationFromOriginField(connection, self, field, attributes) {
  if (!field)
    return;

  if (!field.type.isRelational())
    return;

  const getRelationalModel = (relationalMap, modelName) => {
    let status = relationalMap[modelName];
    return (status) ? status.instance : null;
  };

  let {
    relationalMap,
    TargetModel,
  } = await getRelationalModelStatusForField(connection, self, field);

  let targetModelName   = TargetModel.getModelName();
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
    relationStatus.create   = !thisModelInstance.isPersisted();
    relationStatus.instance = thisModelInstance;

    for (let [ sourceFQField, targetFQField ] of relationStatus.fields.entries()) {
      let targetDef = parseQualifiedName(targetFQField);
      if (Nife.isEmpty(targetDef.fieldNames))
        continue;

      let targetFieldName = targetDef.fieldNames[0];
      let TargetField     = connection.getField(targetFieldName, targetDef.modelName);

      // We never assign to the primaryID
      if (TargetField.primaryKey)
        continue;

      let sourceDef             = parseQualifiedName(sourceFQField);
      let relatedModelInstance  = (modelName === sourceDef.modelName) ? thisModelInstance : getRelationalModel(relationalMap, sourceDef.modelName);
      if (!relatedModelInstance)
        continue;

      // If target field is relational then that
      // means we want to assign the entire model.
      if (TargetField.type.isRelational()) {
        // We don't play around with many relational fields
        if (TargetField.type.isManyRelation())
          continue;

        thisModelInstance[targetDef.fieldNames[0]] = relatedModelInstance;
        continue;
      }

      thisModelInstance[targetDef.fieldNames[0]] = relatedModelInstance[sourceDef.fieldNames[0]];
    }
  }

  return { relationalMap, sortedModelNames };
}

async function createAndSaveAllRelatedModels(connection, self, field, allModelAttributes, options) {
  if (!self)
    return;

  if (!self.isPersisted())
    throw new Error('ModelUtils::createAndSaveAllRelatedModels: Parent model must be persisted before you attempt to save related child models.');

  // Find related models that need to be created
  let TargetModel           = field.type.getTargetModel(connection);
  let targetModelName       = TargetModel.getModelName();
  let parentModelName       = self.getModelName();
  let storedModelMap        = new Map();
  let parentModelSet        = new Set();
  let relatedInfos          = [];

  parentModelSet.add(self);
  storedModelMap.set(parentModelName, parentModelSet);

  for (let i = 0, il = allModelAttributes.length; i < il; i++) {
    let modelAttributes = allModelAttributes[i];
    if (!modelAttributes)
      continue;

    let result = await constructModelsForCreationFromOriginField(connection, self, field, modelAttributes);
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
    return false;

  let fieldSets   = TargetModel.getForeignKeysTargetFieldNames(connection, sourceModelName);
  let hasChanges  = false;

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

    if (targetModelInstance[targetFieldName] !== sourceModelValue) {
      targetModelInstance[targetFieldName] = sourceModelValue;
      hasChanges = true;
    }
  }

  return hasChanges;
}

function assignRelatedModels(model, _relatedModels) {
  if (!model || !_relatedModels)
    return;

  let relatedModels = Nife.toArray(_relatedModels);
  for (let i = 0, il = relatedModels.length; i < il; i++) {
    let relatedModel  = relatedModels[i];
    let modelName     = relatedModel.getModelName();
    let pluralName    = relatedModel.getPluralModelName();
    let relatedScope  = model[pluralName];

    if (!relatedScope) {
      relatedScope = model[pluralName] = [];

      let RelatedModel = relatedModel.getModel();

      model.__assignedRelatedModels.set(modelName, RelatedModel);

      Object.defineProperty(relatedScope, 'Model', {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        RelatedModel,
      });
    }

    if (relatedScope.indexOf(relatedModel) < 0) {
      relatedScope.push(relatedModel);

      let pkFieldName = relatedModel.getPrimaryKeyFieldName();
      let pkValue     = (pkFieldName) ? relatedModel[pkFieldName] : null;

      if (Nife.isNotEmpty(pkValue)) {
        Object.defineProperty(relatedScope, pkValue, {
          writable:     true,
          enumerable:   false,
          configurable: true,
          value:        relatedModel,
        });
      }
    }
  }
}

function getPrimaryKeysForModels(connection, Model, _models, _options) {
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
  let skipRelations         = options.skipRelations;

  for (let i = 0, il = models.length; i < il; i++) {
    let model = models[i];
    if (!model)
      continue;

    let relatedModels = model.__assignedRelatedModels;
    for (let [ relationModelName, RelatedModel ] of relatedModels) {
      if (relationModelName === primaryModelName)
        continue;

      if (providedRelationNames && providedRelationNames.indexOf(relationModelName) < 0)
        continue;

      if (skipRelations && skipRelations.indexOf(relationModelName) >= 0)
        continue;

      let pluralModelName = RelatedModel.getPluralModelName();
      let relatedModels   = model[pluralModelName];
      if (Nife.isEmpty(relatedModels))
        continue;

      let relationPKFieldName = RelatedModel.getPrimaryKeyFieldName();
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
