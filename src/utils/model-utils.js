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
      create:     false,
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

function constructModelsForCreationFromOriginField(connection, modelInstance, originField) {
  // WIP working on this and the above function
  // to get relational model creations to work
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

module.exports = {
  fieldToFullyQualifiedName,
  getRelationalModelStatusForField,
  constructModelsForCreationFromOriginField,
  injectModelMethod,
  isUUID,
  parseQualifiedName,
  sanitizeFieldString,
  setRelationalValues,
  sortModelNamesByCreationOrder,
};
