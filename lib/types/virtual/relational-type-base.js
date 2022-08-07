'use strict';

const Nife            = require('nife');
const Type            = require('../type');
const ModelUtils      = require('../../utils/model-utils');
const { QueryEngine } = require('../../query-engine');

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
  constructor(targetRelation, sourceRelation, _options) {
    let options = _options || {};

    super(targetRelation, sourceRelation, options);

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
      'options': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        options,
      },
    });
  }

  getOptions() {
    return this.options;
  }

  _walkRelationFields(connection, type, callback) {
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

    let relation        = (type === 'target') ? this.getTargetRelation(connection) : this.getSourceRelation(connection);
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
      let result      = callback({ type, source: sourceArgs, target: targetArgs, stop, connection });

      if (_stop)
        return results;

      results.push(result);

      if ((i + 1) >= il) {
        if (fieldType.isForeignKey()) {
          let targetField = fieldType.getTargetField(connection);
          result = callback({ type, source: targetArgs, target: fieldToArgs(targetField), connection });

          if (_stop)
            return results;

          results.push(result);
        } else if (fieldType.isRelational()) {
          result = fieldType._walkRelationFields(connection, type, callback);

          if (_stop)
            return results;

          results = results.concat(result);
        }
      }

      if (il > 1) {
        if (!fieldType.isRelational())
          continue;

        sourceArgs = fieldToArgs(field);

        let subDefinition = getFieldDefinition(fieldType.getTargetRelation(connection), field.Model.getModelName());
        modelName = subDefinition.modelName;
      }
    }

    return results;
  }

  walkTargetRelation(connection, callback) {
    return this._walkRelationFields(connection, 'target', callback);
  }

  walkSourceRelation(connection, callback) {
    return this._walkRelationFields(connection, 'source', callback);
  }

  getTargetModel(connection, options) {
    let field = this.getTargetField(connection, options);
    return field.Model;
  }

  getSourceModel(connection, options) {
    let field = this.getSourceField(connection, options);
    return field.Model;
  }

  getTargetField(connection, _options) {
    let options = _options || {};
    let field;

    this.walkTargetRelation(connection, ({ target, stop }) => {
      if (options.recursive !== true)
        stop();

      if (options.followForeignKeys === false && target.fieldType.isForeignKey())
        stop();

      field = target.field;
    });

    return field;
  }

  getSourceField(connection, _options) {
    let options = _options || {};
    let field;

    this.walkSourceRelation(connection, ({ target, stop }) => {
      if (options.recursive !== true)
        stop();

      if (options.followForeignKeys === false && target.fieldType.isForeignKey())
        stop();

      field = target.field;
    });

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

  _getModelRelations(connection) {
    let relations = [];

    this._walkRelationFields(connection, 'target', ({ source, target }) => {
      let sourceField = (target.fieldType.isRelational()) ? source.fieldType.getSourceField(connection, { recursive: true, followForeignKeys: false }) : source.field;
      let targetField = (target.fieldType.isRelational()) ? source.fieldType.getTargetField(connection, { recursive: true, followForeignKeys: false }) : target.field;

      if (sourceField.type.isVirtual() || targetField.type.isVirtual())
        return;

      relations.push({
        sourceModelName:  sourceField.Model.getModelName(),
        sourceFieldName:  sourceField.fieldName,
        targetModelName:  targetField.Model.getModelName(),
        targetFieldName:  targetField.fieldName,
      });
    });

    this._walkRelationFields(connection, 'source', ({ source, target }) => {
      let sourceField = (source.fieldType.isRelational()) ? source.fieldType.getSourceField(connection, { recursive: true, followForeignKeys: false }) : source.field;
      let targetField = (source.fieldType.isRelational()) ? source.fieldType.getTargetField(connection, { recursive: true, followForeignKeys: false }) : target.field;

      if (sourceField.type.isVirtual() || targetField.type.isVirtual())
        return;

      relations.push({
        sourceModelName:  sourceField.Model.getModelName(),
        sourceFieldName:  sourceField.fieldName,
        targetModelName:  targetField.Model.getModelName(),
        targetFieldName:  targetField.fieldName,
      });
    });

    return relations;
  }

  removeDuplicatesFromRelations(relations) {
    const removeDuplicates = (relations) => {
      let map = {};

      for (let i = 0, il = relations.length; i < il; i++) {
        let part = relations[i];
        let key1 = `${part.targetModelName}.${part.sourceModelName}.${part.targetFieldName}.${part.sourceFieldName}`;
        let key2 = `${part.sourceModelName}.${part.targetModelName}.${part.sourceFieldName}.${part.targetFieldName}`;

        if (!map[key1] && !map[key2])
          map[key1] = part;
      }

      return Array.from(Object.values(map));
    };

    return removeDuplicates(relations);
  }

  getJoinableRelations(connection) {
    if (!connection)
      throw new TypeError(`${this.constructor.name}::getJoinableRelations: Must have a valid "connection" to get field relations.`);

    let relations = this._getModelRelations(connection);
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

    let connection        = modelInstance.getConnection(options.connection);
    let type              = field.type;
    let OriginModel       = field.Model;
    let originModelName   = OriginModel.getModelName();
    let ResultingModel    = type.getTargetModel(connection, { recursive: true, followForeignKeys: false });
    let relations         = type.getJoinableRelations(connection);
    let query             = ResultingModel.where(connection);
    let allModels         = new Set();
    let includeRelations  = (options.includeRelations === true);

    let debug = (true && options.logger);

    if (debug) {
      console.log('Relations: ', relations);
      console.log(`Root model -> ${ResultingModel.getModelName()}`);
    }

    for (let i = relations.length - 1; i >= 0; i--) {
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

      query.AND[targetModelName][targetFieldName].EQ(sourceModel.where(connection)[sourceFieldName]);

      if (debug)
        console.log(`  + .AND.${targetModelName}.${targetFieldName}.EQ(${sourceModelName}.${sourceFieldName})`);

      // eslint-disable-next-line curly
      if (targetModelName === originModelName) {
        query = query.AND[targetModelName][targetFieldName].EQ(modelInstance[targetFieldName]);

        if (debug)
          console.log(`  + .AND.${targetModelName}.${targetFieldName}.EQ(modelInstance.${targetFieldName})`);
      // eslint-disable-next-line curly
      } else if (sourceModelName === originModelName) {
        query = query.AND[sourceModelName][sourceFieldName].EQ(modelInstance[sourceFieldName]);

        if (debug)
          console.log(`  + .AND.${sourceModelName}.${sourceFieldName}.EQ(modelInstance.${sourceFieldName})`);
      }
    }

    if (queryEngine && !QueryEngine.isQuery(queryEngine) && Nife.instanceOf(queryEngine, 'array', 'object') && Nife.isNotEmpty(queryEngine)) {
      queryEngine = ModelUtils.buildQueryFromModelsAttributes(connection, ResultingModel, queryEngine);
      if (!queryEngine)
        throw new Error(`${this.constructor.name}::prepareQuery: Data provided is insufficient to complete operation.`);
    }

    if (QueryEngine.isQuery(queryEngine))
      query = query.AND.MERGE(queryEngine);

    let typeOpts = this.getOptions();
    if (typeOpts && typeOpts.defaultScope === 'function') {
      let defaultScopeQuery = typeOpts.defaultScope.call(this, {
        type:             this,
        model:            modelInstance,
        TargetModel:      ResultingModel,
        targetModelName:  ResultingModel.getModelName(),
        connection,
        query,
      });

      if (QueryEngine.isQuery(defaultScopeQuery) && defaultScopeQuery._getTopContextID() !== query._getTopContextID())
        query = query.AND.MERGE(defaultScopeQuery);
    }

    // If the user requested we include all
    // relations, then adjust the projection
    if (includeRelations)
      query = query.PROJECT(...Array.from(allModels.values()));

    return query;
  }

  toConnectionType() {
    throw new Error(`${this.constructor.name}::toConnectionType: Can not convert relational types to DB types.`);
  }

  toString() {
    return `${this.constructor.name} {}`;
  }
}

module.exports = RelationalTypeBase;
