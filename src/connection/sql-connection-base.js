'use strict';

const Nife            = require('nife');
const SqlString       = require('sqlstring');
const UUID            = require('uuid');
const ConnectionBase  = require('./connection-base');
const SQLLiterals     = require('./sql-literals');
const ModelUtils      = require('../utils/model-utils');
const ModelBase       = require('../model');
const { QueryEngine } = require('../query-engine');

const SAVE_POINT_NAME_CHARS = [ 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P' ];

class SQLConnectionBase extends ConnectionBase {
  static Literals = SQLLiterals;

  static getSQLLiteralClassByName(_name) {
    if (!_name)
      return;

    let name = Nife.capitalize(_name.toLowerCase());

    if (name === 'Literal')
      return SQLLiterals.SQLLiteral;
    else if (name === 'Base')
      return SQLLiterals.SQLLiteralBase;

    return SQLLiterals[`${name}SQLLiteral`];
  }

  static Literal(name, ...args) {
    const LiteralClass = this.getSQLLiteralClassByName(name);
    if (!LiteralClass)
      throw new Error(`${this.constructor.name}::Literal: Unable to locate literal class for literal name "${name}".`);

    let literal = new LiteralClass(...args);
    return literal;
  }

  constructor(_options) {
    super(_options);

    Object.defineProperties(this, {
      'queryGenerator': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
    });
  }

  getQueryGenerator() {
    return this.queryGenerator;
  }

  setQueryGenerator(queryGenerator) {
    this.queryGenerator = queryGenerator;
  }

  prepareArrayValuesForSQL(_array) {
    let array = Nife.arrayFlatten(_array);

    array = array.filter((item) => {
      if (item === null)
        return true;

      if (item instanceof SQLLiterals.SQLLiteralBase)
        return true;

      if (!Nife.instanceOf(item, 'string', 'number', 'bigint', 'boolean'))
        return false;

      return true;
    });

    return Nife.uniq(array);
  }

  escapeSpecialValue(field, value) {
    if (value instanceof SQLLiterals.SQLLiteralBase)
      return value.toString(this);

    if (value === true) {
      return 'TRUE';
    } else if (value === false) {
      return 'FALSE';
    } else if (typeof value === 'bigint') {
      return value.toString();
    } else if (Array.isArray(value)) {
      let arrayValue = this.prepareArrayValuesForSQL(value);
      if (Nife.isEmpty(arrayValue))
        return '';

      return `(${arrayValue.map((item) => this.escape(field, item)).join(',')})`;
    }
  }

  escape(field, value) {
    let result = this.escapeSpecialValue(field, value);
    if (result !== undefined)
      return result;

    return SqlString.escape(value);
  }

  escapeID(value) {
    if (value instanceof SQLLiterals.SQLLiteralBase)
      return value.toString(this.connection);

    return SqlString.escapeId(value).replace(/`/g, '"');
  }

  generateSavePointName() {
    let id = UUID.v4();

    id = id.toUpperCase().replace(/\d/g, (m) => {
      let index = parseInt(m, 10);
      return SAVE_POINT_NAME_CHARS[index];
    }).replace(/-/g, '');

    return `SP${id}`;
  }

  findAllFieldsFromFieldProjectionMap(projectionFieldMap) {
    let fullFieldNames = (Array.isArray(projectionFieldMap)) ? projectionFieldMap : Array.from(projectionFieldMap.keys());

    return fullFieldNames.map((fullFieldName) => {
      let def = ModelUtils.parseQualifiedName(fullFieldName);
      if (!def.modelName || def.fieldNames.length === 0)
        return fullFieldName;

      let field = this.getField(def.fieldNames[0], def.modelName);
      if (!field)
        return fullFieldName;

      return field;
    }).filter(Boolean);
  }

  buildModelDataMapFromSelectResults(queryEngine, result) {
    if (!result)
      return {};

    let rows = result.rows;
    if (Nife.isEmpty(rows))
      return {};

    let context         = queryEngine._getRawQueryContext();
    let fields          = this.findAllFieldsFromFieldProjectionMap(result.columns);
    let rootModelName   = context.rootModelName;
    let modelData       = {};
    let alreadyVisited  = {};

    let fieldInfo = fields.map((field) => {
      let Model       = field.Model;
      let modelName   = Model.getModelName();
      let pkFieldName = Model.getPrimaryKeyFieldName();

      return {
        pkFieldName,
        field,
        Model,
        modelName,
      };
    });

    let modelInfo = fieldInfo.reduce((obj, info) => {
      obj[info.modelName] = info;
      return obj;
    }, {});

    for (let i = 0, il = rows.length; i < il; i++) {
      let row   = rows[i];
      let data  = {};

      // Collect row
      for (let j = 0, jl = fieldInfo.length; j < jl; j++) {
        let {
          field,
          modelName,
        } = fieldInfo[j];

        let dataContext = data[modelName];
        let remoteValue = row[j];

        if (!dataContext)
          dataContext = data[modelName] = {};

        dataContext[field.fieldName] = remoteValue;
      }

      // Remap row
      let modelNames = Object.keys(data).sort((a, b) => {
        if (a === rootModelName)
          return -1;

        if (b === rootModelName)
          return 1;

        if (a === b)
          return 0;

        return (a < b) ? -1 : 1;
      });

      let rootModelData;
      for (let i = 0, il = modelNames.length; i < il; i++) {
        let modelName     = modelNames[i];
        let info          = modelInfo[modelName];
        let models        = modelData[modelName];
        let model         = data[modelName];
        let pkFieldName   = info.pkFieldName;
        let index;

        if (!models)
          models = modelData[modelName] = [];

        // TODO: Use model projection to build relationships
        // This should work if it just happens at the projection
        // level, as that is what will project the fields, and
        // collect the models for these operations.

        if (pkFieldName) {
          let id = model[pkFieldName];

          if (id != null) {
            let idKey = `${modelName}:${pkFieldName}:${id}`;

            if (alreadyVisited[idKey] != null) {
              index = alreadyVisited[idKey];
            } else {
              index = alreadyVisited[idKey] = models.length;
              models.push(model);
            }
          }
        } else {
          models.push(model);
          continue;
        }

        if (i === 0) {
          rootModelData = model;
        } else {
          if (!rootModelData._) {
            Object.defineProperties(rootModelData, {
              '_': {
                writable:     true,
                enumberable:  false,
                configurable: true,
                value:        {},
              },
            });
          }

          if (!rootModelData._[modelName])
            rootModelData._[modelName] = [];

          rootModelData._[modelName].push(index);
        }
      }
    }

    return modelData;
  }

  buildModelsFromModelDataMap(queryEngine, modelDataMap) {
    if (Nife.isEmpty(modelDataMap))
      return [];

    let queryContext  = queryEngine._getRawQueryContext();
    let rootModelName = queryContext.rootModelName;
    let RootModel     = queryContext.rootModel;
    if (!rootModelName || !RootModel)
      throw new Error(`${this.constructor.name}::buildModelsFromModelDataMap: Root model not found.`);

    let rootModelData = modelDataMap[rootModelName];
    if (Nife.isEmpty(rootModelData))
      return [];

    let rootModels = rootModelData.map((data) => {
      let model = new RootModel(data);

      if (model._ && data._) {
        let relationships = data._;
        let modelNames    = Object.keys(relationships);

        for (let i = 0, il = modelNames.length; i < il; i++) {
          let modelName           = modelNames[i];
          let Model               = this.getModel(modelName);
          let pluralModelName     = Model.getPluralName();
          let relationName        = Nife.uncapitalize(pluralModelName);
          let relationshipModels  = model._[relationName];
          let modelIndexes        = relationships[modelName];
          let models              = modelDataMap[modelName];

          if (!relationshipModels)
            relationshipModels = model._[relationName] = [];

          model._[relationName] = relationshipModels.concat(modelIndexes.map((modelIndex) => {
            let modelData = models[modelIndex];
            return new Model(modelData);
          }));
        }
      }

      model.clearDirty();

      return model;
    });

    return rootModels;
  }

  prepareAllModelsForOperation(Model, _models, _options) {
    if (!_models)
      return {};

    if (_models.mythixPreparedModels)
      return _models.mythixPreparedModels;

    if (Array.isArray(_models) && !_models.length)
      return {};

    let options           = _options || {};
    let finalizedModels   = [];
    let dirtyFieldNames   = {};
    let dirtyFields       = [];
    let hasAllFieldNames  = false;
    let totalFieldCount   = Model.getConcreteFieldCount();
    let startIndex        = options.startIndex || 0;
    let models            = Nife.toArray(_models);
    let endIndex          = models.length;

    if (options.endIndex)
      endIndex = Math.min(options.endIndex, endIndex);
    else if (options.batchSize)
      endIndex = Math.min(startIndex + options.batchSize, endIndex);

    // Make sure all data is models,
    // and find all the dirty fields
    for (let i = startIndex; i < endIndex; i++) {
      let model = models[i];
      if (!(model instanceof ModelBase))
        model = new Model(model);

      if (!hasAllFieldNames) {
        Object.assign(dirtyFieldNames, model.changes);

        if (Object.keys(dirtyFieldNames).length >= totalFieldCount)
          hasAllFieldNames = true;
      }

      finalizedModels.push(model);
    }

    let fieldNames = Object.keys(dirtyFieldNames);
    for (let i = 0, il = fieldNames.length; i < il; i++) {
      let fieldName = fieldNames[i];
      let field     = Model.getField(fieldName);
      if (!field)
        continue;

      dirtyFields.push(field);
    }

    let finalResult = { models: finalizedModels, dirtyFields };
    Object.defineProperties(finalResult, {
      'mythixPreparedModels': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        finalResult,
      },
    });

    return finalResult;
  }

  async bulkModelOperation(Model, _models, _options, callback) {
    let models = _models;
    if (!models)
      return;

    if (!Array.isArray(models)) {
      if (!models.mythixPreparedModels)
        models = [ models ].filter(Boolean);
    }

    if (Nife.isEmpty(models))
      return;

    let queryGenerator  = this.getQueryGenerator();
    let options         = Object.assign({}, _options || {});
    let batchSize       = options.batchSize || 500;
    if (batchSize < 1)
      throw new Error(`${this.constructor.name}::bulkModelOperation: "batchSize" can not be less than 1.`);

    let offset        = 0;
    let totalModels   = models.length;
    let finalResults  = [];

    options.endIndex = 0;

    while (options.endIndex < totalModels) {
      options.startIndex = offset;
      options.endIndex = offset + batchSize;

      if (options.endIndex >= totalModels)
        options.endIndex = totalModels;

      let preparedModels  = this.prepareAllModelsForOperation(Model, models, options);
      if (!preparedModels.models || preparedModels.models.length === 0)
        break;

      await callback.call(this, Model, preparedModels, options, queryGenerator);

      let batchModels = preparedModels.models;
      if (batchModels) {
        for (let i = 0, il = batchModels.length; i < il; i++) {
          let batchModel = batchModels[i];
          batchModel.clearDirty();
          finalResults.push(batchModel);
        }
      }

      offset += batchSize;
    }

    return finalResults;
  }

  async insert(Model, models, _options) {
    return await this.bulkModelOperation(Model, models, _options, async (Model, preparedModels, options, queryGenerator) => {
      let sqlStr          = queryGenerator.generateInsertStatement(Model, preparedModels, options);
      let ids             = await this.query(sqlStr, { formatResponse: true });
      // TODO: Assign ids if PK is auto-incrementing
    });
  }

  async update(Model, models, _options, _queryEngine) {
    let queryEngine = _queryEngine;
    let options     = _options;

    if (QueryEngine.isQuery(options)) {
      queryEngine = options;
      options = {};
    } else if (!options) {
      options = {};
    }

    let primaryKeyFieldName = Model.getPrimaryKeyFieldName();
    if (Nife.isEmpty(primaryKeyFieldName) && (!queryEngine || !QueryEngine.isQuery(queryEngine)))
      throw new Error(`${this.constructor.name}::update: Model has no primary key field. You must supply a query when updating models with no primary key.`);

    if (!queryEngine)
      queryEngine = Model.where;

    return await this.bulkModelOperation(Model, models, options, async (Model, preparedModels, options, queryGenerator) => {
      let models = preparedModels.models;
      for (let i = 0, il = models.length; i < il; i++) {
        let model         = models[i];
        let query         = queryEngine.clone();

        if (!_queryEngine) {
          let pkFieldValue = model[primaryKeyFieldName];
          if (!pkFieldValue)
            throw new Error(`${this.constructor.name}::update: Model's primary key is empty. Models being updated must have a valid primary key value unless you specify a query for the update.`);

          query = query[primaryKeyFieldName].EQ(pkFieldValue);
        }

        let sqlStr = queryGenerator.generateUpdateStatement(Model, model, options, query);
        await this.query(sqlStr, { formatResponse: true });
      }
    });
  }

  async destroy(Model, models, _options) {
    let options         = _options;
    let queryGenerator  = this.getQueryGenerator();

    if (QueryEngine.isQuery(models)) {
      let sqlStr          = queryGenerator.generateDeleteStatement(Model, models);
      return await this.query(sqlStr);
    } else if (!options) {
      options = {};
    }

    let primaryKeyFieldName = Model.getPrimaryKeyFieldName();
    if (Nife.isEmpty(primaryKeyFieldName))
      throw new Error(`${this.constructor.name}::destroy: Model has no primary key field. You must supply a query to delete models with no primary key.`);

    return await this.bulkModelOperation(Model, models, options, async (Model, preparedModels, options, queryGenerator) => {
      let models  = preparedModels.models;
      let pkIDs   = [];

      for (let i = 0, il = models.length; i < il; i++) {
        let model         = models[i];
        let pkFieldValue  = model[primaryKeyFieldName];
        if (pkFieldValue != null && Nife.isEmpty(pkFieldValue))
          continue;

        pkIDs.push(pkFieldValue);
      }

      if (Nife.isEmpty(pkIDs))
        return;

      let sqlStr = queryGenerator.generateDeleteStatement(Model, Model.where.id.EQ(pkIDs));
      await this.query(sqlStr, { formatResponse: true });
    });
  }

  async select(_queryEngine, _options) {
    let queryEngine = _queryEngine;
    if (!queryEngine)
      throw new TypeError(`${this.constructor.name}::select: First argument must be a model class or a query.`);

    if (!QueryEngine.isQuery(queryEngine)) {
      if (Object.prototype.hasOwnProperty.call(queryEngine, 'where'))
        queryEngine = queryEngine.where;
      else
        throw new TypeError(`${this.constructor.name}::select: First argument must be a model class or a query.`);
    }

    let options         = _options || {};
    let queryGenerator  = this.getQueryGenerator();
    let sqlStatement    = queryGenerator.generateSelectStatement(queryEngine);
    let result          = await this.query(sqlStatement, { formatResponse: true, logger: options.logger });
    let modelDataMap    = this.buildModelDataMapFromSelectResults(queryEngine, result);
    let models          = this.buildModelsFromModelDataMap(queryEngine, modelDataMap);

    return models;
  }
}

module.exports = SQLConnectionBase;
