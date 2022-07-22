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

  getDefaultOrder(Model, options) {
    let order = Nife.toArray(Model.getDefaultOrder(options));

    order = Nife.arrayFlatten(order).filter(Boolean);

    if (Nife.isEmpty(order))
      return;

    return order;
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

    // Make sure all items are models,
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

  splitModelAndSubModels(Model, primaryModel, _relationMap) {
    const addModelInstance = (modelName, modelInstance) => {
      let relatedModels = relationMap.get(modelName);
      if (!relatedModels) {
        relatedModels = new Set();
        relationMap.set(modelName, relatedModels);
      }

      relatedModels.add(modelInstance);
    };

    const splitSubModels = (Model, model) => {
      if (alreadyVisitedMap.get(model))
        return;

      alreadyVisitedMap.set(model, true);

      let modelName = Model.getModelName();

      Model.iterateFields(({ field, fieldName }) => {
        let fieldType = field.type;
        if (!fieldType.isVirtual())
          return;

        if (!fieldType.exposeToModel())
          return;

        if (!fieldType.isRelational())
          return;

        // We don't deal with many relations
        if (fieldType.isManyRelation())
          return;

        let TargetModel = fieldType.getTargetModel({ recursive: true, followForeignKeys: true });
        if (!TargetModel)
          return;

        // Ignore field types that point to "this" model (Model)
        let targetModelName = TargetModel.getModelName();
        if (targetModelName === modelName)
          return;

        // Ignore field types that point to the primary model
        if (targetModelName === primaryModelName)
          return;

        let value = model[fieldName];
        if (value == null)
          return;

        if (!(value instanceof TargetModel)) {
          value = new TargetModel(value);
          model[fieldName] = value;
        }

        if (!value.isDirty())
          return;

        addModelInstance(targetModelName, value);

        let fieldRelations = ModelUtils.getRelationalModelStatusForField(this, value, field);
        if (fieldRelations) {
          let modelNames = Object.keys(fieldRelations);

          for (let i = 0, il = modelNames.length; i < il; i++) {
            let relatedModelName = modelNames[i];
            if (relatedModelName === modelName || relatedModelName === primaryModelName || relatedModelName === targetModelName)
              continue;

            let RelatedModel = this.getModel(relatedModelName);
            let modelInstance = new RelatedModel();

            addModelInstance(relatedModelName, modelInstance);
          }
        }

        splitSubModels(TargetModel, value, relationMap);
      });
    };

    let alreadyVisitedMap = new Map();
    let relationMap       = _relationMap || new Map();
    let primaryModelName  = Model.getModelName();

    splitSubModels(Model, primaryModel);

    return relationMap;
  }

  prepareAllModelsAndSubModelsForOperation(Model, models, _options) {
    let options                 = Object.assign({}, _options || {}, { startIndex: undefined, endIndex: undefined, batchSize: undefined });
    let primaryModelRelationMap = new Map();
    let groupedModelMap         = new Map();
    let primaryModelName        = Model.getModelName();

    const addModelToGroup = (modelName, modelInstance) => {
      let group = groupedModelMap.get(modelName);
      if (!group) {
        group = new Set();
        groupedModelMap.set(modelName, group);
      }

      group.add(modelInstance);
    };

    // Collect all primary models and sub-models
    for (let i = 0, il = models.length; i < il; i++) {
      let model = models[i];
      if (!(model instanceof ModelBase))
        model = new Model(model);

      let subModels = this.splitModelAndSubModels(Model, model);

      primaryModelRelationMap.set(model, subModels);
      addModelToGroup(primaryModelName, model);

      for (let [ modelName, models ] of subModels.entries()) {
        for (let subModel of models.values())
          addModelToGroup(modelName, subModel);
      }
    }

    // Sort group map by model creation order
    let sortedGroupedModelMap = new Map();
    let sortedModelNames      = ModelUtils.sortModelNamesByCreationOrder(this, Array.from(groupedModelMap.keys()));
    for (let i = 0, il = sortedModelNames.length; i < il; i++) {
      let groupModelName  = sortedModelNames[i];
      let subModels       = Array.from(groupedModelMap.get(groupModelName).values());

      sortedGroupedModelMap.set(groupModelName, subModels);
    }

    // Now copy related field values between all instantiated models
    for (let [ primaryModel, subModelMap ] of primaryModelRelationMap.entries()) {
      for (let [ targetModelName, targetModels ] of subModelMap.entries()) {
        let TargetModel = this.getModel(targetModelName);

        for (let targetModel of targetModels.values()) {
          if (targetModel !== primaryModel) {
            ModelUtils.setRelationalValues(TargetModel, targetModel, Model, primaryModel);
            ModelUtils.setRelationalValues(Model, primaryModel, TargetModel, targetModel);
          }

          for (let [ sourceModelName, sourceModels ] of subModelMap.entries()) {
            if (sourceModelName === targetModelName)
              continue;

            let SourceModel = this.getModel(sourceModelName);
            for (let sourceModel of sourceModels.values()) {
              if (sourceModel === primaryModel)
                continue;

              ModelUtils.setRelationalValues(TargetModel, targetModel, SourceModel, sourceModel);
            }
          }
        }
      }
    }

    return sortedGroupedModelMap;
  }

  async bulkModelOperation(Model, _models, _options, callback, afterOperationCallback) {
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

    const computeBulkModels = async (Model, models, options) => {
      let offset        = 0;
      let totalModels   = models.length;
      let finalResults  = [];

      options.endIndex = 0;

      while (options.endIndex < totalModels) {
        options.startIndex = offset;
        options.endIndex = offset + batchSize;

        if (options.endIndex >= totalModels)
          options.endIndex = totalModels;

        let preparedModels = this.prepareAllModelsForOperation(Model, models, options);
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
    };

    let primaryModelName  = Model.getModelName();
    let groupedModelMap   = this.prepareAllModelsAndSubModelsForOperation(Model, models, options);
    let alreadyStored     = {};
    let dirtyModels       = new Set();
    let primaryResult;

    console.log('GROUP: ', groupedModelMap);

    for (let [ modelName, models ] of groupedModelMap) {
      let GroupModel    = this.getModel(modelName);
      let resultModels  = await computeBulkModels(GroupModel, models, options);

      alreadyStored[modelName] = true;

      if (modelName === primaryModelName)
        primaryResult = resultModels;

      for (let storedModel of resultModels) {
        for (let [ groupModelName, groupModels ] of groupedModelMap) {
          if (groupModelName === modelName)
            continue;

          if (!alreadyStored[groupModelName])
            continue;

          let TargetModel = this.getModel(groupModelName);
          if (!TargetModel.isForeignKeyTargetModel(modelName))
            continue;

          for (let targetModel of groupModels) {
            ModelUtils.setRelationalValues(TargetModel, targetModel, GroupModel, storedModel);

            if (targetModel.isDirty())
              dirtyModels.add(targetModel);
          }
        }
      }
    }

    if (dirtyModels.size > 0 && typeof afterOperationCallback === 'function')
      await afterOperationCallback.call(this, Model, dirtyModels, options, queryGenerator);

    return primaryResult;
  }

  async createTable(Model, options) {
    let queryGenerator  = this.getQueryGenerator();
    let createTableSQL  = queryGenerator.generateCreateTableStatement(Model);

    // Create table
    await this.query(createTableSQL, options);

    // Create indexes and constraints
    let trailingStatements = Nife.toArray(queryGenerator.generateCreateTableStatementOuterTail(Model, options)).filter(Boolean);
    if (Nife.isNotEmpty(trailingStatements)) {
      for (let i = 0, il = trailingStatements.length; i < il; i++) {
        let trailingStatement = trailingStatements[i];
        await this.query(trailingStatement, options);
      }
    }
  }

  async insert(Model, models, _options) {
    return await this.bulkModelOperation(
      Model,
      models,
      _options,
      async (Model, preparedModels, options, queryGenerator) => {
        let sqlStr  = queryGenerator.generateInsertStatement(Model, preparedModels, options);
        console.log('SQL STR: ', sqlStr);
        let ids     = await this.query(sqlStr, { formatResponse: true });
        // TODO: Assign ids if PK is auto-incrementing
      },
      async (PrimaryModel, dirtyModels, options, queryGenerator) => {
        for (let dirtyModel of dirtyModels) {
          let Model   = dirtyModel.getModel();
          let sqlStr  = queryGenerator.generateUpdateStatement(Model, dirtyModel, null, options);
          await this.query(sqlStr, { formatResponse: true });
        }
      },
    );
  }

  // eslint-disable-next-line no-unused-vars
  async upsert(Model, models, _options) {
    throw new Error(`${this.constructor.name}::upsert: This connection type does not support "upsert" operations.`);
  }

  async update(Model, models, _options) {
    let options = _options || {};

    let primaryKeyFieldName = Model.getPrimaryKeyFieldName();
    if (Nife.isEmpty(primaryKeyFieldName))
      throw new Error(`${this.constructor.name}::update: Model has no primary key field.`);

    return await this.bulkModelOperation(Model, models, options, async (Model, preparedModels, options, queryGenerator) => {
      let models = preparedModels.models;
      for (let i = 0, il = models.length; i < il; i++) {
        let model = models[i];
        let query = Model.where;

        let pkFieldValue = model[primaryKeyFieldName];
        if (!pkFieldValue)
          throw new Error(`${this.constructor.name}::update: Model's primary key is empty. Models being updated must have a valid primary key.`);

        query = query[primaryKeyFieldName].EQ(pkFieldValue);

        let sqlStr = queryGenerator.generateUpdateStatement(Model, model, query, options);
        await this.query(sqlStr, { formatResponse: true });
      }
    });
  }

  async updateAll(_queryEngine, model, _options) {
    let queryEngine = this.toQueryEngine(_queryEngine);
    if (!queryEngine)
      throw new Error(`${this.constructor.name}::updateAll: Model class or query is required to update.`);

    let rootModel = queryEngine._getRawQueryContext().rootModel;
    if (!rootModel)
      throw new Error(`${this.constructor.name}::updateAll: Root model not found, and is required to update.`);

    let options         = _options || {};
    let queryGenerator  = this.getQueryGenerator();
    let sqlStr          = queryGenerator.generateUpdateStatement(rootModel, model, queryEngine, options);

    return await this.query(sqlStr, { formatResponse: true, logger: options.logger });
  }

  async destroy(Model, models, _options) {
    let options         = _options;
    let queryGenerator  = this.getQueryGenerator();

    if (QueryEngine.isQuery(models) || (models == null || Array.isArray(models) && Nife.isEmpty(models))) {
      let query = models;
      if (!query)
        query = Model.where.unscoped();

      let sqlStr = queryGenerator.generateDeleteStatement(Model, query);
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

  async destroyAll(_queryEngine, _options) {
    let queryEngine = this.toQueryEngine(_queryEngine);
    if (!queryEngine)
      throw new Error(`${this.constructor.name}::destroyAll: Model class or query is required to destroy.`);

    let rootModel = queryEngine._getRawQueryContext().rootModel;
    if (!rootModel)
      throw new Error(`${this.constructor.name}::updateAll: Root model not found, and is required to destroy.`);

    let options         = _options || {};
    let queryGenerator  = this.getQueryGenerator();
    let sqlStr          = queryGenerator.generateDeleteStatement(rootModel, queryEngine, options);

    return await this.query(sqlStr, { formatResponse: true, logger: options.logger });
  }

  async *select(_queryEngine, _options) {
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
    let batchSize       = options.batchSize || 500;
    let startIndex      = 0;
    let queryGenerator  = this.getQueryGenerator();

    while (true) {
      let query         = queryEngine.clone().LIMIT(batchSize).OFFSET(startIndex);
      let sqlStatement  = queryGenerator.generateSelectStatement(query, options);
      console.log('SQL Statement: ', sqlStatement);
      let result        = await this.query(sqlStatement, { formatResponse: true, logger: options.logger });

      if (!result.rows || result.rows.length === 0)
        break;

      startIndex += result.rows.length;

      if (options.raw === true) {
        yield result;
      } else {
        let modelDataMap  = this.buildModelDataMapFromSelectResults(queryEngine, result);
        let models        = this.buildModelsFromModelDataMap(queryEngine, modelDataMap);

        for (let i = 0, il = models.length; i < il; i++) {
          let model = models[i];
          yield model;
        }
      }

      if (result.rows.length < batchSize)
        break;
    }
  }

  async aggregate(_queryEngine, literal, options) {
    if (!(literal instanceof SQLLiterals.SQLLiteralBase))
      throw new Error(`${this.constructor.name}::aggregate: Second argument must be a SQLLiteral instance.`);

    let queryEngine = this.toQueryEngine(_queryEngine);
    if (!queryEngine)
      throw new TypeError(`${this.constructor.name}::aggregate: First argument must be a model class or a query.`);

    let queryGenerator  = this.getQueryGenerator();
    let query           = queryEngine.clone().PROJECT().PROJECT(literal);
    let sqlStr          = queryGenerator.generateSelectStatement(query, options);

    let result = await this.query(sqlStr, Object.assign({}, options || {}, { formatResponse: true }));
    return result.rows[0][0];
  }

  async average(_queryEngine, _field, options) {
    let queryEngine = this.toQueryEngine(_queryEngine);
    if (!queryEngine)
      throw new TypeError(`${this.constructor.name}::average: First argument must be a model class or a query.`);

    let rootModel = queryEngine._getRawQueryContext().rootModel;
    let field     = ModelUtils.fieldToFullyQualifiedName(_field, rootModel);

    return await this.aggregate(queryEngine, new SQLLiterals.AverageSQLLiteral(field), options);
  }

  async count(_queryEngine, _field, options) {
    let queryEngine = this.toQueryEngine(_queryEngine);
    if (!queryEngine)
      throw new TypeError(`${this.constructor.name}::count: First argument must be a model class or a query.`);

    let rootModel = queryEngine._getRawQueryContext().rootModel;
    let field     = (_field) ? ModelUtils.fieldToFullyQualifiedName(_field, rootModel) : null;

    return await this.aggregate(queryEngine, new SQLLiterals.CountSQLLiteral(field), options);
  }

  async min(_queryEngine, _field, options) {
    let queryEngine = this.toQueryEngine(_queryEngine);
    if (!queryEngine)
      throw new TypeError(`${this.constructor.name}::min: First argument must be a model class or a query.`);

    let rootModel = queryEngine._getRawQueryContext().rootModel;
    let field     = ModelUtils.fieldToFullyQualifiedName(_field, rootModel);

    return await this.aggregate(queryEngine, new SQLLiterals.MinSQLLiteral(field), options);
  }

  async max(_queryEngine, _field, options) {
    let queryEngine = this.toQueryEngine(_queryEngine);
    if (!queryEngine)
      throw new TypeError(`${this.constructor.name}::max: First argument must be a model class or a query.`);

    let rootModel = queryEngine._getRawQueryContext().rootModel;
    let field     = ModelUtils.fieldToFullyQualifiedName(_field, rootModel);

    return await this.aggregate(queryEngine, new SQLLiterals.MaxSQLLiteral(field), options);
  }

  async sum(_queryEngine, _field, options) {
    let queryEngine = this.toQueryEngine(_queryEngine);
    if (!queryEngine)
      throw new TypeError(`${this.constructor.name}::sum: First argument must be a model class or a query.`);

    let rootModel = queryEngine._getRawQueryContext().rootModel;
    let field     = ModelUtils.fieldToFullyQualifiedName(_field, rootModel);

    return await this.aggregate(queryEngine, new SQLLiterals.SumSQLLiteral(field), options);
  }

  async pluck(_queryEngine, ..._fields) {
    let fields = Nife.arrayFlatten(_fields).filter(Boolean);
    if (Nife.isEmpty(fields))
      throw new Error(`${this.constructor.name}::pluck: You must supply "fields" to pluck.`);

    let queryEngine = _queryEngine;
    if (!queryEngine)
      throw new TypeError(`${this.constructor.name}::pluck: First argument must be a model class or a query.`);

    if (!QueryEngine.isQuery(queryEngine)) {
      if (Object.prototype.hasOwnProperty.call(queryEngine, 'where'))
        queryEngine = queryEngine.where;
      else
        throw new TypeError(`${this.constructor.name}::pluck: First argument must be a model class or a query.`);
    }

    let queryGenerator  = this.getQueryGenerator();
    let query           = queryEngine.clone().PROJECT(fields);
    let sqlStr          = queryGenerator.generateSelectStatement(query);
    let result          = await this.query(sqlStr, { formatResponse: true });
    let { rows }        = result;

    if (fields.length === 1 && rows[0] && rows[0].length === 1)
      return rows.map((row) => row[0]);

    return rows;
  }

  async exists(queryEngine, options) {
    let count = await this.count(queryEngine, null, options);
    return (count > 0);
  }
}

module.exports = SQLConnectionBase;
