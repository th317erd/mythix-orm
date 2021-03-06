'use strict';

const Nife            = require('nife');
const Type            = require('../type');
const ModelUtils      = require('../../utils/model-utils');
const { QueryEngine } = require('../../query-engine');

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
        Model:      field.Model,
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

  prepareQuery(modelInstance, field, _queryEngine, _options) {
    let options     = _options;
    let queryEngine = _queryEngine;

    // Is the queryEngine argument actually the options?
    if (!options && queryEngine && !QueryEngine.isQuery(queryEngine))
      options = queryEngine;

    if (!options)
      options = {};

    let connection        = modelInstance.getConnection();
    let type              = field.type;
    let OriginModel       = field.Model;
    let originModelName   = OriginModel.getModelName();
    let ResultingModel    = type.getTargetModel({ recursive: true, followForeignKeys: false }, connection);
    let relations         = type.getJoinableRelations(connection);
    let query             = ResultingModel.where;
    let allModels         = new Set();
    let includeRelations  = (options.includeRelations === true);

    // console.log('Relations: ', relations);
    // console.log(`Root model -> ${ResultingModel.getModelName()}`);

    for (let i = 0, il = relations.length; i < il; i++) {
      let relation = relations[i];
      let {
        sourceModelName,
        sourceFieldName,
        targetModelName,
        targetFieldName,
      } = relation;

      let sourceModel = connection.getModel(sourceModelName);
      let targetModel = connection.getModel(targetModelName);

      if (!sourceModel)
        throw new Error(`${this.constructor.name}::prepareQuery: Model "${sourceModelName}" is a source relation, but model not found.`);

      if (!targetModel)
        throw new Error(`${this.constructor.name}::prepareQuery: Model ${targetModelName} is a target relation, but model not found.`);

      if (includeRelations) {
        allModels.add(`+${sourceModelName}:*`);
        allModels.add(`+${targetModelName}:*`);
      }

      query.AND[targetModelName][targetFieldName].EQ(sourceModel.where[sourceFieldName]);

      // console.log(`  + .AND.${targetModelName}.${targetFieldName}.EQ(${sourceModelName}.${sourceFieldName})`);

      // eslint-disable-next-line curly
      if (targetModelName === originModelName) {
        query = query.AND[targetModelName][targetFieldName].EQ(modelInstance[targetFieldName]);
        // console.log(`  + .AND.${targetModelName}.${targetFieldName}.EQ(modelInstance.${targetFieldName})`);
      // eslint-disable-next-line curly
      } else if (sourceModelName === originModelName) {
        query = query.AND[sourceModelName][sourceFieldName].EQ(modelInstance[sourceFieldName]);
        // console.log(`  + .AND.${sourceModelName}.${sourceFieldName}.EQ(modelInstance.${sourceFieldName})`);
      }
    }

    if (queryEngine && !QueryEngine.isQuery(queryEngine)) {
      queryEngine = ModelUtils.buildQueryFromModelsAttributes(ResultingModel, queryEngine);
      if (!queryEngine)
        throw new Error(`${this.constructor.name}::prepareQuery: Data provided is insufficient to complete operation.`);
    }

    if (QueryEngine.isQuery(queryEngine))
      query = query.AND.MERGE(queryEngine);

    // If the user requested we include all
    // relations, then adjust the projection
    if (includeRelations)
      query = query.PROJECT(...Array.from(allModels.values()));

    return query;
  }

  toString() {
    return '';
  }
}

module.exports = RelationalTypeBase;
