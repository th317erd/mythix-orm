'use strict';

const Nife        = require('nife');
const Type        = require('../type');
const ModelUtils  = require('../../utils/model-utils');

const MAX_RECURSE_DEPTH = 20;

class RelationalTypeBase extends Type {
  static isVirtual() {
    return true;
  }

  static isRelational() {
    return true;
  }

  // Model types work by specifying a "target"
  // and a "value provider" (source).
  // These are fully qualified names, meaning
  // they also point to the model as well as the field.
  // If no model is specified, then it always defaults to
  // "this" model. If no field is specified, then it always
  // defaults to "this PK" of the model.
  // Mythix ORM will recursively walk all models and fields
  // defined until it has the full relationships between
  // all fields.
  constructor(...args) {
    super(...args);

    let [
      targetRelation,
      sourceRelation,
    ] = args;

    Object.defineProperties(this, {
      'targetRelation': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        targetRelation,
      },
      'sourceRelation': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        sourceRelation,
      },
    });
  }

  _walkRelationFields(type, callback, _connection) {
    let connection = _connection || this.getConnection();
    if (!connection)
      throw new TypeError(`${this.constructor.name}::_walkRelation: Must have a valid "connection".`);

    const getFieldDefinition = (relation, modelName) => {
      let def = this.getFullyQualifiedName(relation);
      if (!def.modelName)
        def.modelName = (modelName) ? modelName : this.getModel().getModelName();

      return { modelName: def.modelName, fieldNames: def.fieldNames };
    };

    const fieldToArgs = (field) => {
      return {
        field:      field,
        fieldName:  field.fieldName,
        fieldType:  field.type,
        modelName:  field.Model.getModelName(),
        connection,
      };
    };

    let relation        = (type === 'target') ? this.getTargetRelation() : this.getSourceRelation();
    let fieldDefinition = getFieldDefinition(relation);
    if (!fieldDefinition)
      return;

    let field;
    let {
      modelName,
      fieldNames,
    } = fieldDefinition;

    let _stop       = false;
    const stop      = () => (_stop = true);

    let sourceField = this.getField();
    let sourceArgs  = fieldToArgs(sourceField);
    let results     = [];

    for (let i = 0, il = fieldNames.length; i < il; i++) {
      let fieldName = fieldNames[i];
      field = connection.getField(fieldName, modelName);

      if (!field)
        throw new Error(`${this.constructor.name}::_walkRelationFields: Unable to find field ${modelName}:${fieldName}.`);

      let fieldType   = field.type;
      let targetArgs  = fieldToArgs(field);
      let result      = callback({ source: sourceArgs, target: targetArgs, stop });

      if (_stop)
        return results;

      results.push(result);

      if ((i + 1) >= il) {
        if (fieldType.isForeignKey()) {
          let targetField = fieldType.getTargetField();
          result = callback({ source: targetArgs, target: fieldToArgs(targetField) });

          if (_stop)
            return results;

          results.push(result);
        } else if (fieldType.isRelational()) {
          result = fieldType._walkRelationFields(type, callback, connection);

          if (_stop)
            return results;

          results = results.concat(result);
        }
      }

      if (il > 1) {
        sourceArgs = fieldToArgs(field);

        let subDefinition = getFieldDefinition(fieldType.getTargetRelation(), field.Model.getModelName());
        modelName = subDefinition.modelName;
      }
    }

    return results;
  }

  walkTargetRelation(callback, _connection) {
    return this._walkRelationFields('target', callback, _connection);
  }

  walkSourceRelation(callback, _connection) {
    return this._walkRelationFields('source', callback, _connection);
  }

  getTargetModel(options, _connection) {
    let field = this.getTargetField(options, _connection);
    return field.Model;
  }

  getSourceModel(options, _connection) {
    let field = this.getSourceField(options, _connection);
    return field.Model;
  }

  getTargetField(_options, _connection) {
    let options = _options || {};
    let field;

    this.walkTargetRelation(({ target, stop }) => {
      if (options.recursive !== true)
        stop();

      if (options.followForeignKeys === false && target.fieldType.isForeignKey())
        stop();

      field = target.field;
    }, _connection);

    return field;
  }

  getSourceField(_options, _connection) {
    let options = _options || {};
    let field;

    this.walkSourceRelation(({ target, stop }) => {
      if (options.recursive !== true)
        stop();

      if (options.followForeignKeys === false && target.fieldType.isForeignKey())
        stop();

      field = target.field;
    }, _connection);

    return field;
  }

  getTargetRelation() {
    return this.targetRelation;
  }

  getSourceRelation() {
    return this.sourceRelation;
  }

  getFullyQualifiedName(qualifiedName) {
    let def = (Nife.isEmpty(qualifiedName)) ? {} : ModelUtils.parseQualifiedName(qualifiedName);
    if (!def.modelName)
      def.modelName = this.getModel().getModelName();

    if (!def.modelName)
      throw new Error(`${this.constructor.name}::getFullyQualifiedName: Error while attempting to parse qualified name "${qualifiedName}" on field "${this.getModel().getModelName()}.${this.getField().fieldName}": Model not found.`);

    if (Nife.isEmpty(def.fieldNames)) {
      let pkName = this.getModel().getPrimaryKeyFieldName();
      if (Nife.isEmpty(pkName))
        throw new Error(`${this.constructor.name}::getFullyQualifiedName: Error while attempting to parse qualified name "${qualifiedName}" on field "${this.getModel().getModelName()}.${this.getField().fieldName}": No field was specified, and no primary key was found for the model. Without a primary key on the model you MUST specify a field name.`);

      def.fieldNames = [ pkName ];
    }

    return def;
  }

  _getModelRelations(connection, _relations) {
    let relations = _relations || [];
    if (relations.length > MAX_RECURSE_DEPTH)
      throw new Error(`${this.constructor.name}::_getModelRelations: Depth limit exceeded chasing virtual fields.`);

    let fieldRelations = [
      this.getFullyQualifiedName(this.getTargetRelation()),
      this.getFullyQualifiedName(this.getSourceRelation()),
    ];

    for (let j = 0, jl = fieldRelations.length; j < jl; j++) {
      let def         = fieldRelations[j];
      let fieldNames  = def.fieldNames;
      let modelName   = def.modelName;

      for (let i = 0, il = fieldNames.length; i < il; i++) {
        let fieldName = fieldNames[i];
        let field     = connection.getField(fieldName, modelName);
        if (!field)
          throw new Error(`${this.constructor.name}::_getModelRelations: Attempted to fetch "${modelName}.${fieldName}" and failed.`);

        let fieldType = field.type;
        if (fieldType.isRelational()) {
          modelName = fieldType._getModelRelations(
            connection,
            relations,
          );
        } else if (fieldType.isVirtual()) {
          throw new Error(`${this.constructor.name}::_getModelRelations: Encountered an unexpected virtual field "${modelName}.${fieldName}".`);
        } else {
          let sourceModelName = fieldRelations[1].modelName;
          let sourceFieldName = fieldRelations[1].fieldNames[0];
          if (sourceFieldName === fieldName && sourceModelName === modelName)
            continue;

          let sourceField = connection.getField(sourceFieldName, sourceModelName);
          if (!sourceField)
            throw new Error(`${this.constructor.name}::_getModelRelations: Attempted to fetch "${sourceModelName}.${sourceFieldName}" and failed.`);

          let sourceFieldType = sourceField.type;
          if (sourceFieldType.isRelational() || sourceFieldType.isVirtual())
            continue;

          relations.push({
            relationType:     (j === 1) ? 'source' : 'target',
            fieldIndex:       i,
            sourceModelName:  sourceModelName,
            sourceFieldName:  sourceFieldName,
            targetModelName:  modelName,
            targetFieldName:  fieldName,
          });

          break;
        }
      }
    }

    return fieldRelations[0].modelName;
  }

  removeDuplicatesFromRelations(relations) {
    const removeDuplicates = (relations) => {
      let map = {};

      for (let i = 0, il = relations.length; i < il; i++) {
        let part = relations[i];
        let key = `${part.targetModelName}.${part.sourceModelName}.${part.targetFieldName}.${part.sourceFieldName}.${part.relationType}.${part.fieldIndex}`;

        map[key] = part;
      }

      return Array.from(Object.values(map));
    };

    return removeDuplicates(relations);
  }

  getJoinableRelations(_connection) {
    let connection = _connection || this.getConnection();
    if (!connection)
      throw new TypeError(`${this.constructor.name}::getJoinableRelations: Must have a valid "connection" to get field relations.`);

    let relations = [];

    this._getModelRelations(
      connection,
      relations,
    );

    return this.removeDuplicatesFromRelations(relations);
  }

  prepareQuery(modelInstance, field, queryEngine) {
    let connection          = modelInstance.getConnection();
    let type                = field.type;
    let OriginModel         = field.Model;
    let originModelName     = OriginModel.getModelName();
    let ResultingModel      = type.getTargetModel({ recursive: true, followForeignKeys: false }, connection);
    let relations           = type.getJoinableRelations(connection);
    let query               = ResultingModel.where;

    for (let i = 0, il = relations.length; i < il; i++) {
      let relation = relations[i];
      let {
        sourceModelName,
        sourceFieldName,
        targetModelName,
        targetFieldName,
      } = relation;

      let targetModel = connection.getModel(targetModelName);
      query.AND[sourceModelName][sourceFieldName].EQ(targetModel.where[targetFieldName]);

      if (targetModelName === originModelName)
        query = query.AND[targetModelName][targetModelName].EQ(modelInstance[targetFieldName]);
      else if (sourceModelName === originModelName)
        query = query.AND[sourceModelName][sourceFieldName].EQ(modelInstance[sourceFieldName]);
    }

    if (queryEngine)
      query = query.AND(queryEngine);

    return query;
  }

  setRelationalValues(Model, modelInstance, RelatedModel, relatedModelInstance) {
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

  toString() {
    return '';
  }
}

module.exports = RelationalTypeBase;
