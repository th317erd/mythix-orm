'use strict';

const EventEmitter        = require('events');
const Nife                = require('nife');
const SqlString           = require('sqlstring');
const { QueryEngine }     = require('../query-engine');
const ModelUtils          = require('../utils/model-utils');
const ModelBase           = require('../model');
const Literals            = require('./literals');
const QueryGeneratorBase  = require('./query-generator-base');
const Types               = require('../types');

const LiteralBase = Literals.LiteralBase;

class ConnectionBase extends EventEmitter {
  static dialect = 'none';

  constructor(_options) {
    super();

    this.setMaxListeners(0);

    let options = Object.assign({
      QueryEngine,
    }, _options || {});

    if (!options.queryGenerator)
      options.queryGenerator = new QueryGeneratorBase(this);

    Object.defineProperties(this, {
      'dialect': {
        enumberable:  false,
        configurable: true,
        get:          () => {
          return this.constructor.dialect;
        },
        set:          () => {
        },
      },
      'models': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        {},
      },
      '_options': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        options,
      },
      'queryGenerator': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        options.queryGenerator,
      },
    });

    this.registerModels(options.models);
  }

  toQueryEngine(_queryEngine) {
    let queryEngine = _queryEngine;
    if (!queryEngine)
      return;

    if (!QueryEngine.isQuery(queryEngine)) {
      if (Object.prototype.hasOwnProperty.call(queryEngine, 'where'))
        queryEngine = queryEngine.where;
      else
        queryEngine = undefined;
    }

    return queryEngine;
  }

  registerModel(_Model, skipModelInitialization) {
    let modelName = _Model.getModelName();
    let Model     = _Model.initializeConnection(_Model, this);

    if (skipModelInitialization !== true)
      Model = _Model.initializeModel(_Model, this);

    this.models[modelName] = Model;

    return Model;
  }

  registerModels(models) {
    if (!models)
      return;

    let keys              = Object.keys(models);
    let initializedModels = [];

    for (let i = 0, il = keys.length; i < il; i++) {
      let key = keys[i];
      let Model = models[key];

      let initializedModel = this.registerModel(Model, true);
      initializedModels.push(initializedModel);
    }

    for (let i = 0, il = initializedModels.length; i < il; i++) {
      let initializedModel = initializedModels[i];
      initializedModel = initializedModel.initializeModel(initializedModel, this);
      this.models[initializedModel.getModelName()] = initializedModel;
    }
  }

  findModelField(finder) {
    let modelMap    = this.getModels();
    let modelNames  = Object.keys(modelMap);
    let results     = [];

    for (let i = 0, il = modelNames.length; i < il; i++) {
      let modelName = modelNames[i];
      let Model     = modelMap[modelName];

      results = results.concat(Model.iterateFields(finder));
    }

    return results;
  }

  getOptions() {
    return this._options;
  }

  getQueryEngineClass() {
    let options = this.getOptions();
    return options.QueryEngine;
  }

  getQueryGenerator() {
    return this.queryGenerator;
  }

  setQueryGenerator(queryGenerator) {
    this.queryGenerator = queryGenerator;
  }

  _escape(value) {
    return SqlString.escape(value);
  }

  escape(field, _value) {
    var value = _value;
    if (value instanceof Literals.LiteralBase)
      return value.toString(this);

    value = field.type.serialize(value, this);

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

    return this._escape(value);
  }

  _escapeID(value) {
    let parts = value.replace(/['"`]/g, '').split(/\.+/g);
    return parts.map((part) => SqlString.escapeId(part).replace(/^`/, '"').replace(/`$/, '"')).join('.');
  }

  escapeID(value) {
    if (value instanceof Literals.LiteralBase)
      return value.toString(this.connection);

    return this._escapeID(value);
  }

  _averageLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._averageLiteralToString(literal);
  }

  _countLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._countLiteralToString(literal);
  }

  _distinctLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._distinctLiteralToString(literal);
  }

  _maxLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._maxLiteralToString(literal);
  }

  _minLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._minLiteralToString(literal);
  }

  _sumLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._sumLiteralToString(literal);
  }

  literalToString(literal) {
    if (literal instanceof Literals.AverageLiteral)
      return this._averageLiteralToString(literal);
    else if (literal instanceof Literals.CountLiteral)
      return this._countLiteralToString(literal);
    else if (literal instanceof Literals.DistinctLiteral)
      return this._distinctLiteralToString(literal);
    else if (literal instanceof Literals.MaxLiteral)
      return this._maxLiteralToString(literal);
    else if (literal instanceof Literals.MinLiteral)
      return this._minLiteralToString(literal);
    else if (literal instanceof Literals.SumLiteral)
      return this._sumLiteralToString(literal);
    else if (literal instanceof Literals.Literal)
      return literal.toString(this);

    throw new Error(`${this.constructor.name}::literalToString: Unsupported literal ${literal}.`);
  }

  _bigintTypeToString(type) {
    return 'BIGINT';
  }

  _blobTypeToString(type) {
    return 'BLOB';
  }

  _booleanTypeToString(type) {
    return 'BOOLEAN';
  }

  _charTypeToString(type) {
    return 'CHAR';
  }

  _dateTypeToString(type) {
    return 'DATE';
  }

  _datetimeTypeToString(type) {
    return 'DATETIME';
  }

  _floatTypeToString(type) {
    return 'FLOAT';
  }

  _integerTypeToString(type) {
    return 'INTEGER';
  }

  _stringTypeToString(type) {
    return `VARCHAR(${type.length})`;
  }

  _textTypeToString(type) {
    return 'TEXT';
  }

  _uuidV1TypeToString(type) {
    return 'VARCHAR(36)';
  }

  _uuidV3TypeToString(type) {
    return 'VARCHAR(36)';
  }

  _uuidV4TypeToString(type) {
    return 'VARCHAR(36)';
  }

  _uuidV5TypeToString(type) {
    return 'VARCHAR(36)';
  }

  _xidTypeToString(type) {
    return 'VARCHAR(20)';
  }

  typeToString(type) {
    if (type instanceof Types.BigIntType)
      return this._bigintTypeToString(type);
    else if (type instanceof Types.BlobType)
      return this._blobTypeToString(type);
    else if (type instanceof Types.BooleanType)
      return this._booleanTypeToString(type);
    else if (type instanceof Types.CharType)
      return this._charTypeToString(type);
    else if (type instanceof Types.DateType)
      return this._dateTypeToString(type);
    else if (type instanceof Types.DateTimeType)
      return this._datetimeTypeToString(type);
    else if (type instanceof Types.FloatType)
      return this._floatTypeToString(type);
    else if (type instanceof Types.IntegerType)
      return this._integerTypeToString(type);
    else if (type instanceof Types.StringType)
      return this._stringTypeToString(type);
    else if (type instanceof Types.TextType)
      return this._textTypeToString(type);
    else if (type instanceof Types.UUIDV1Type)
      return this._uuidV1TypeToString(type);
    else if (type instanceof Types.UUIDV3Type)
      return this._uuidV3TypeToString(type);
    else if (type instanceof Types.UUIDV4Type)
      return this._uuidV4TypeToString(type);
    else if (type instanceof Types.UUIDV5Type)
      return this._uuidV5TypeToString(type);
    else if (type instanceof Types.XIDType)
      return this._xidTypeToString(type);

    throw new Error(`${this.constructor.name}::typeToString: Unsupported type ${type}.`);
  }

  parseQualifiedName(str) {
    return ModelUtils.parseQualifiedName(str);
  }

  getModels() {
    return Object.assign({}, this.models);
  }

  getModel(modelName) {
    if (typeof modelName === 'symbol')
      return;

    let def = this.parseQualifiedName(modelName);
    return this.models[def.modelName];
  }

  getField(fieldName, modelName) {
    let def = this.parseQualifiedName(fieldName);
    if (def.modelName == null)
      def.modelName = modelName;

    let Model = this.getModel(def.modelName);
    if (!Model)
      return;

    return Model.getField(def.fieldNames[0]);
  }

  ensureAllModelsAreInstances(Model, _models, options) {
    if (!_models)
      return [];

    if (_models.mythixPreparedModels)
      return _models.mythixPreparedModels.models;

    let instantiatedModels  = [];
    let startIndex          = options.startIndex || 0;
    let models              = Nife.toArray(_models);
    let endIndex            = models.length;

    if (options.endIndex)
      endIndex = Math.min(options.endIndex, endIndex);
    else if (options.batchSize)
      endIndex = Math.min(startIndex + options.batchSize, endIndex);

    for (let i = startIndex; i < endIndex; i++) {
      let model = models[i];

      if (!(model instanceof ModelBase))
        model = new Model(model);

      instantiatedModels.push(model);
    }

    return instantiatedModels;
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
    let dirtyModels       = [];
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

      if (!(model instanceof ModelBase)) {
        model = new Model(model);
      } else if (model.isPersisted() && options.skipPersisted) {
        if (model.isDirty())
          dirtyModels.push(model);

        continue;
      }

      if (!hasAllFieldNames) {
        Object.assign(
          dirtyFieldNames,
          model._getDirtyFields({
            update: options.isUpdateOperation,
            insert: options.isInsertOperation,
          }),
        );

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

    let finalResult = { models: finalizedModels, dirtyFields, dirtyModels };
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

            let relationStatus = fieldRelations[relatedModelName];
            let RelatedModel = relationStatus.Model;
            let modelInstance = (relationStatus.instance) ? relationStatus.instance : new RelatedModel();
            if (!(modelInstance instanceof RelatedModel))
              modelInstance = new RelatedModel(modelInstance);

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

  // eslint-disable-next-line no-unused-vars
  prepareAllModelsAndSubModelsForOperation(Model, models, _options) {
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

            if (!TargetModel.isForeignKeyTargetModel(sourceModelName))
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

  async bulkModelOperation(Model, _models, _options, beforeCallback, callback, afterCallback, afterOperationCallback) {
    let models = _models;
    if (!models)
      return;

    let inputIsArray = false;
    if (!Array.isArray(models)) {
      if (!models.mythixPreparedModels) {
        if (models instanceof Map || models instanceof Set)
          inputIsArray = true;

        models = Nife.toArray(models).filter(Boolean);
      } else {
        inputIsArray = true;
      }
    } else {
      inputIsArray = true;
    }

    if (Nife.isEmpty(models))
      return (inputIsArray) ? [] : undefined;

    let queryGenerator  = this.getQueryGenerator();
    let options         = Object.assign({}, _options || {});
    let batchSize       = options.batchSize || 500;
    if (batchSize < 1)
      throw new Error(`${this.constructor.name}::bulkModelOperation: "batchSize" can not be less than 1.`);

    const computeBulkModels = async (Model, _models, options) => {
      let models        = Nife.toArray(_models);
      let totalModels   = models.length;
      let offset        = 0;
      let finalResults  = [];
      let dirtyModels   = [];

      options.endIndex = 0;

      while (options.endIndex < totalModels) {
        options.startIndex = offset;
        options.endIndex = offset + batchSize;

        if (options.endIndex >= totalModels)
          options.endIndex = totalModels;

        // We need to run before callbacks first
        // because this might change the fields
        // that are dirty
        let batchModelInstances = this.ensureAllModelsAreInstances(Model, models, options);
        if (typeof beforeCallback === 'function')
          await beforeCallback.call(this, Model, batchModelInstances, options, queryGenerator);

        let preparedModels = this.prepareAllModelsForOperation(Model, batchModelInstances, Object.assign({}, options, { startIndex: 0, endIndex: batchModelInstances.length }));

        await callback.call(this, Model, preparedModels, options, queryGenerator);

        if (options.isInsertOperation)
          dirtyModels = dirtyModels.concat(preparedModels.dirtyModels);

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

      return { results: finalResults, dirtyModels };
    };

    let primaryModelName  = Model.getModelName();
    let groupedModelMap   = this.prepareAllModelsAndSubModelsForOperation(Model, models, options);
    let alreadyStored     = {};
    let allDirtyModels    = new Set();
    let primaryResult;

    // console.log('GROUP: ', groupedModelMap);

    for (let [ modelName, models ] of groupedModelMap) {
      let GroupModel                = this.getModel(modelName);
      let { results, dirtyModels }  = await computeBulkModels(GroupModel, models, options);

      // If prepareAllModelsForOperation found persisted
      // models that were dirty, then add them here
      if (dirtyModels && dirtyModels.length > 0) {
        for (let i = 0, il = dirtyModels.length; i < il; i++)
          allDirtyModels.add(dirtyModels[i]);
      }

      alreadyStored[modelName] = true;

      if (modelName === primaryModelName)
        primaryResult = results;

      for (let storedModel of results) {
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
              allDirtyModels.add(targetModel);
          }
        }
      }

      if (typeof afterCallback === 'function')
        await afterCallback.call(this, GroupModel, results, options, queryGenerator);
    }

    if (allDirtyModels.size > 0 && typeof afterOperationCallback === 'function')
      await afterOperationCallback.call(this, Model, allDirtyModels, options, queryGenerator);

    return (inputIsArray) ? primaryResult : primaryResult[0];
  }

  setPersisted(_models, value) {
    let models = _models;
    if (models.mythixPreparedModels)
      models = models.models;

    if (Nife.isEmpty(models))
      return;

    for (let i = 0, il = models.length; i < il; i++) {
      let model = models[i];
      if (!model || !model._mythixModelInstance)
        continue;

      model._persisted = value;
    }
  }

  async start() {
    throw new Error(`${this.constructor.name}::start: Child class is required to implement "start".`);
  }

  async stop() {
    this.removeAllListeners();
  }
}

module.exports = ConnectionBase;
