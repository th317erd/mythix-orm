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

  let relationModelMap = {
    [modelInstance.getModelName()]: { create: false, instance: true, value: modelInstance, modelName: modelInstance.getModelName() },
  };

  const addModelStatus = (modelName, status) => {
    relationModelMap[modelName] = Object.assign(status, { modelName });
  };

  const findModelStatus = (modelName) => {
    return relationModelMap[modelName];
  };

  const hasModelStatus = (modelName) => {
    let status = findModelStatus(modelName);
    return !!status;
  };

  const addRelationalStatus = (type, Model, field, fieldType) => {
    let modelName = Model.getModelName();

    if (fieldType.isRelational() && fieldType.isManyRelation()) {
      if (hasModelStatus(modelName))
        return;

      // we must create
      addModelStatus(modelName, { create: true, instance: false, value: Model });
      return;
    } else {
      // we might already have
      addSingleRelationStatus(type, Model, field);
    }
  };

  const addSingleRelationStatus = (type, Model, field) => {
    let modelName = Model.getModelName();
    if (hasModelStatus(modelName))
      return;

    let model = findLoadedRelationalModel(field);
    if (!model)
      addModelStatus(modelName, { create: true, instance: false, value: Model });
    else
      addModelStatus(modelName, { create: false, instance: true, value: model });
  };

  const findLoadedRelationalModel = (field) => {
    let value = modelInstance[field.fieldName];

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

    addRelationalStatus('source', sourceField.Model, sourceField, sourceFieldType);
    addRelationalStatus('target', targetField.Model, targetField, targetFieldType);
  }, connection);

  return relationModelMap;
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
  injectModelMethod,
  isUUID,
  parseQualifiedName,
  sanitizeFieldString,
  setRelationalValues,
  sortModelNamesByCreationOrder,
};
