'use strict';

const { DateTime }          = require('luxon');
const EventEmitter          = require('events');
const Nife                  = require('nife');
const SqlString             = require('sqlstring');
const { QueryEngine }       = require('../query-engine');
const Utils                 = require('../utils');
const { Model: ModelBase }  = require('../model');
const Literals              = require('./literals');
const QueryGeneratorBase    = require('./query-generator-base');
const Types                 = require('../types');

const LiteralBase = Literals.LiteralBase;

/// ConnectionBase is the base class that
/// all connection classes should inherit from.
///
/// Alias: Connection
class ConnectionBase extends EventEmitter {
  static Literals = Literals;

  static dialect = 'none';

  static _isMythixConnection = true;

  static isConnectionClass(value) {
    if (!value)
      return false;

    if (value.prototype instanceof ConnectionBase)
      return true;

    if (value._isMythixConnection)
      return true;

    return false;
  }

  static isConnection(value) {
    if (!value)
      return false;

    if (value instanceof ConnectionBase)
      return true;

    if (value.constructor && value.constructor._isMythixConnection)
      return true;

    return false;
  }

  static getLiteralClassByName(_name) {
    if (!_name)
      return;

    let name = Nife.capitalize(_name.toLowerCase());

    if (name === 'Literal')
      return Literals.Literal;
    else if (name === 'Base')
      return Literals.LiteralBase;

    return Literals[`${name}Literal`];
  }

  static Literal(name, ...args) {
    const LiteralClass = this.getLiteralClassByName(name);
    if (!LiteralClass)
      throw new Error(`${this.constructor.name}::Literal: Unable to locate literal class for literal name "${name}".`);

    let literal = new LiteralClass(...args);
    return literal;
  }

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
        enumerable:   false,
        configurable: true,
        get:          () => {
          return this.constructor.dialect;
        },
        set:          () => {
        },
      },
      '_models': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        {},
      },
      '_options': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        options,
      },
      '_modelCache': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        new Map(),
      },
      'queryGenerator': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        options.queryGenerator,
      },
    });

    this.registerModels(options.models);
  }

  getLockMode(options) {
    if (!options)
      return { lock: false, read: false, write: false };

    const throwError = () => {
      throw new Error(`${this.constructor.name}::getLockMode: "lock" must be the name of a model (lock: "ModelName"), or an object specifying the model and the lock mode (lock: { modelName: "ModelName", read: true, write: true, dependents: true, noWait: false }).`);
    };

    if (Nife.instanceOf(options, 'string')) {
      let Model = this.getModel(options);
      if (!Model)
        throwError();

      return { lock: true, modelName: options, read: true, write: true };
    } else if (Nife.instanceOf(options, 'object')) {
      let modelName = options.modelName;
      let Model = this.getModel(modelName);
      if (!Model)
        throwError();

      return Object.assign({ lock: true, modelName, read: true, write: true }, options);
    } else {
      throwError();
    }
  }

  getDefaultOrder(Model, options) {
    let order = Model.defaultOrder(options);
    if (!order)
      return;

    order = Nife.arrayFlatten(Nife.toArray(order)).filter((value) => {
      if (!value)
        return false;

      if (!Nife.instanceOf(value, 'string'))
        return false;

      return true;
    });

    if (Nife.isEmpty(order))
      return;

    let modelName = Model.getModelName();
    return order.map((value) => ((value.indexOf(':') < 0) ? `${modelName}:${value}` : value));
  }

  isLimitSupportedInContext(options) {
    return true;
  }

  isOrderSupportedInContext(_options) {
    let options = _options || {};
    if (options.isSubQuery) {
      let subQueryOperator = options.subQueryOperator;
      if (subQueryOperator === 'EXISTS' || subQueryOperator === 'NOT EXISTS')
        return true;

      return 'PROJECTION_ONLY';
    }

    return true;
  }

  _getFromModelCache(Model, key, defaultValue) {
    let cache = this._modelCache.get(Model);
    if (!cache)
      return defaultValue;

    let value = cache.get(key);
    if (value == null)
      return defaultValue;

    return value;
  }

  _setToModelCache(Model, key, value) {
    let cache = this._modelCache.get(Model);
    if (!cache) {
      cache = new Map();
      this._modelCache.set(Model, cache);
    }

    cache.set(key, value);

    return value;
  }

  getOptions() {
    return this._options;
  }

  isStarted() {
    return true;
  }

  toQueryEngine(_queryEngine) {
    let queryEngine = _queryEngine;
    if (!queryEngine)
      return;

    if (!QueryEngine.isQuery(queryEngine)) {
      if (Object.prototype.hasOwnProperty.call(queryEngine, 'where'))
        queryEngine = queryEngine.where(this);
      else
        queryEngine = undefined;
    }

    return queryEngine;
  }

  registerModel(_Model) {
    let Model = _Model.bindConnection(this);

    this._models[Model.getModelName()] = Model;

    return Model;
  }

  registerModels(models) {
    if (!models)
      return;

    let keys = Object.keys(models);

    for (let i = 0, il = keys.length; i < il; i++) {
      let key   = keys[i];
      let Model = models[key];

      this.registerModel(Model);
    }

    return this._models;
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

  parseQualifiedName(str) {
    return Utils.parseQualifiedName(str);
  }

  getModels() {
    return this._models;
  }

  getModel(modelName) {
    if (typeof modelName === 'symbol')
      return;

    let def = this.parseQualifiedName(modelName);
    return this._models[def.modelName];
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
    if (Nife.instanceOf(value, 'string'))
      return `'${value.replace(/'/g, '\\\'')}'`;

    return SqlString.escape(value);
  }

  escape(field, _value, options) {
    var value = _value;
    if (LiteralBase.isLiteral(value))
      return value.toString(this, options);

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

  escapeID(value, options) {
    if (LiteralBase.isLiteral(value))
      return value.toString(this.connection, options);

    return this._escapeID(value);
  }

  _averageLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._averageLiteralToString(literal, options);
  }

  _countLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._countLiteralToString(literal, options);
  }

  _distinctLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._distinctLiteralToString(literal, options);
  }

  _fieldLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._fieldLiteralToString(literal, options);
  }

  _maxLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._maxLiteralToString(literal, options);
  }

  _minLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._minLiteralToString(literal, options);
  }

  _sumLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._sumLiteralToString(literal, options);
  }

  literalToString(literal, options) {
    if (Literals.AverageLiteral.isLiteralType(literal))
      return this._averageLiteralToString(literal, options);
    else if (Literals.CountLiteral.isLiteralType(literal))
      return this._countLiteralToString(literal, options);
    else if (Literals.DistinctLiteral.isLiteralType(literal))
      return this._distinctLiteralToString(literal, options);
    else if (Literals.FieldLiteral.isLiteralType(literal))
      return this._fieldLiteralToString(literal, options);
    else if (Literals.MaxLiteral.isLiteralType(literal))
      return this._maxLiteralToString(literal, options);
    else if (Literals.MinLiteral.isLiteralType(literal))
      return this._minLiteralToString(literal, options);
    else if (Literals.SumLiteral.isLiteralType(literal))
      return this._sumLiteralToString(literal, options);
    else if (Literals.Literal.isLiteralType(literal))
      return literal.toString(this, options);

    throw new Error(`${this.constructor.name}::literalToString: Unsupported literal ${literal}.`);
  }

  // eslint-disable-next-line no-unused-vars
  _bigintTypeToString(type) {
    return 'BIGINT';
  }

  // eslint-disable-next-line no-unused-vars
  _blobTypeToString(type) {
    return 'BLOB';
  }

  // eslint-disable-next-line no-unused-vars
  _booleanTypeToString(type) {
    return 'BOOLEAN';
  }

  // eslint-disable-next-line no-unused-vars
  _charTypeToString(type) {
    return 'CHAR';
  }

  // eslint-disable-next-line no-unused-vars
  _dateTypeToString(type) {
    return 'TIMESTAMP';
  }

  // eslint-disable-next-line no-unused-vars
  _datetimeTypeToString(type) {
    return 'TIMESTAMP';
  }

  // eslint-disable-next-line no-unused-vars
  _numericTypeToString(type) {
    return `NUMERIC(${type.precision}, ${type.scale})`;
  }

  // eslint-disable-next-line no-unused-vars
  _realTypeToString(type) {
    return 'FLOAT';
  }

  // eslint-disable-next-line no-unused-vars
  _integerTypeToString(type) {
    return 'INTEGER';
  }

  // eslint-disable-next-line no-unused-vars
  _stringTypeToString(type) {
    return `VARCHAR(${type.length})`;
  }

  // eslint-disable-next-line no-unused-vars
  _textTypeToString(type) {
    return 'TEXT';
  }

  // eslint-disable-next-line no-unused-vars
  _uuidV1TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  // eslint-disable-next-line no-unused-vars
  _uuidV3TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  // eslint-disable-next-line no-unused-vars
  _uuidV4TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  // eslint-disable-next-line no-unused-vars
  _uuidV5TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  // eslint-disable-next-line no-unused-vars
  _xidTypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  typeToString(type, options) {
    if (Types.BigIntType.isSameType(type))
      return this._bigintTypeToString(type, options);
    else if (Types.BlobType.isSameType(type))
      return this._blobTypeToString(type, options);
    else if (Types.BooleanType.isSameType(type))
      return this._booleanTypeToString(type, options);
    else if (Types.CharType.isSameType(type))
      return this._charTypeToString(type, options);
    else if (Types.DateType.isSameType(type))
      return this._dateTypeToString(type, options);
    else if (Types.DateTimeType.isSameType(type))
      return this._datetimeTypeToString(type, options);
    else if (Types.NumericType.isSameType(type))
      return this._numericTypeToString(type, options);
    else if (Types.RealType.isSameType(type))
      return this._realTypeToString(type, options);
    else if (Types.IntegerType.isSameType(type))
      return this._integerTypeToString(type, options);
    else if (Types.StringType.isSameType(type))
      return this._stringTypeToString(type, options);
    else if (Types.TextType.isSameType(type))
      return this._textTypeToString(type, options);
    else if (Types.UUIDV1Type.isSameType(type))
      return this._uuidV1TypeToString(type, options);
    else if (Types.UUIDV3Type.isSameType(type))
      return this._uuidV3TypeToString(type, options);
    else if (Types.UUIDV4Type.isSameType(type))
      return this._uuidV4TypeToString(type, options);
    else if (Types.UUIDV5Type.isSameType(type))
      return this._uuidV5TypeToString(type, options);
    else if (Types.XIDType.isSameType(type))
      return this._xidTypeToString(type, options);

    throw new Error(`${this.constructor.name}::typeToString: Unsupported type ${type}.`);
  }

  convertDateToDBTime(value, type) {
    if (Nife.instanceOf(value, 'number'))
      return value;
    else if (Nife.instanceOf(value, 'bigint'))
      return Number(value).valueOf();
    else if (DateTime.isDateTime(value))
      return value.toMillis();
    else if (value instanceof Date || (value && value.constructor && value.constructor.name === 'Date'))
      return value.valueOf();
    else if (Nife.instanceOf(value, 'string'))
      return DateTime.fromISO(value).toMillis();

    return value;
  }

  ensureAllModelsAreInstances(Model, _models, options) {
    if (!_models)
      return [];

    if (_models._mythixPreparedModels)
      return _models._mythixPreparedModels.models;

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

      if (!ModelBase.isModel(model))
        model = new Model(model, { connection: this });

      instantiatedModels.push(model);
    }

    return instantiatedModels;
  }

  prepareAllModelsForOperation(Model, _models, _options) {
    if (!_models)
      return {};

    if (_models._mythixPreparedModels)
      return _models._mythixPreparedModels;

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

      if (!ModelBase.isModel(model)) {
        model = new Model(model, { connection: this });
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
      '_mythixPreparedModels': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        finalResult,
      },
    });

    return finalResult;
  }

  splitModelAndSubModels(Model, primaryModel, _relationMap) {
    const addModelInstance = (modelName, self) => {
      let relatedModels = relationMap.get(modelName);
      if (!relatedModels) {
        relatedModels = new Set();
        relationMap.set(modelName, relatedModels);
      }

      relatedModels.add(self);
    };

    const splitSubModels = (Model, model) => {
      if (alreadyVisitedMap.get(model))
        return;

      alreadyVisitedMap.set(model, true);

      let modelName = Model.getModelName();

      Model.iterateFields(({ field, fieldName }) => {
        let fieldType = field.type;
        if (!fieldType.isRelational())
          return;

        // We don't deal with multi-relational fields
        if (fieldType.isManyRelation())
          return;

        let TargetModel     = fieldType.getTargetModel(this);
        let targetModelName = TargetModel.getModelName();
        if (targetModelName === modelName || targetModelName === primaryModelName)
          return;

        let modelInstance = model[fieldName];
        if (modelInstance == null)
          return;

        if (!(modelInstance instanceof TargetModel))
          modelInstance = new TargetModel(modelInstance, { connection: this });

        addModelInstance(targetModelName, modelInstance);

        // Sub model data may have another
        // model to create
        splitSubModels(TargetModel, modelInstance, relationMap);
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

    const addModelToGroup = (modelName, self) => {
      let group = groupedModelMap.get(modelName);
      if (!group) {
        group = new Set();
        groupedModelMap.set(modelName, group);
      }

      group.add(self);
    };

    // Collect all primary models and sub-models
    for (let i = 0, il = models.length; i < il; i++) {
      let model = models[i];
      if (!ModelBase.isModel(model))
        model = new Model(model, { connection: this });

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
    let sortedModelNames      = Utils.sortModelNamesByCreationOrder(this, Array.from(groupedModelMap.keys()));
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
            Utils.setRelationalValues(this, TargetModel, targetModel, Model, primaryModel);
            Utils.setRelationalValues(this, Model, primaryModel, TargetModel, targetModel);
          }

          for (let [ sourceModelName, sourceModels ] of subModelMap.entries()) {
            if (sourceModelName === targetModelName)
              continue;

            if (!TargetModel.isForeignKeyTargetModel(this, sourceModelName))
              continue;

            let SourceModel = this.getModel(sourceModelName);
            for (let sourceModel of sourceModels.values()) {
              if (sourceModel === primaryModel)
                continue;

              Utils.setRelationalValues(this, TargetModel, targetModel, SourceModel, sourceModel);
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
      if (!models._mythixPreparedModels) {
        if (Nife.instanceOf(models, 'map', 'set'))
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
      let models      = Nife.toArray(_models);
      let totalModels = models.length;
      if (totalModels === 0)
        return { results: finalResults, dirtyModels };

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
        if (preparedModels.models.length === 0) {
          // no dirty models in this batch
          offset += batchSize;
          continue;
        }

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

    if (options.isDeleteOperation) {
      // For delete we DO NOT want to collect
      // related models... just delete what we have
      let { results } = await computeBulkModels(Model, models, options);
      return (inputIsArray || !results) ? results : results[0];
    }

    let primaryModelName  = Model.getModelName();
    let groupedModelMap   = this.prepareAllModelsAndSubModelsForOperation(Model, models, options);
    let alreadyStored     = {};
    let allDirtyModels    = new Set();
    let primaryResult;

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
          if (!TargetModel.isForeignKeyTargetModel(this, modelName))
            continue;

          for (let targetModel of groupModels) {
            Utils.setRelationalValues(this, TargetModel, targetModel, GroupModel, storedModel);

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

    return (inputIsArray || !primaryResult) ? primaryResult : primaryResult[0];
  }

  setPersisted(_models, value) {
    let models = _models;
    if (models._mythixPreparedModels)
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

  async runSaveHooks(Model, models, operationHookName, saveHookName, options) {
    const throwError = (error) => {
      throw error;
    };

    let promises  = [];
    let context   = { connection: this, Model, options, self: null };

    for (let i = 0, il = models.length; i < il; i++) {
      let model = models[i];

      context.self = model;

      let promise = model[operationHookName](context);
      if (!Nife.instanceOf(promise, 'promise'))
        promise = Promise.resolve(promise);

      promise = promise.then(async () => await model[saveHookName](context), throwError);
      if (!Nife.instanceOf(promise, 'promise'))
        promise = Promise.resolve(promise);

      promises.push(promise);
    }

    await Promise.all(promises);
  }

  async dropTable(Model, options) {
    throw new Error(`${this.constructor.name}::dropTable: This operation is not supported for this connection type.`);
  }

  async dropTables(_Models, options) {
    if (!_Models)
      return;

    // First we collect all models and put them into a map
    let modelMap = _Models;

    if (Nife.instanceOf(_Models, 'array', 'function')) {
      modelMap = {};

      let Models = Nife.toArray(_Models).filter(Boolean);
      for (let i = 0, il = Models.length; i < il; i++) {
        let Model     = Models[i];
        let modelName = Model.getModelName();

        modelMap[modelName] = Model;
      }
    }

    // Second we sort the model names in creation order,
    // and going in reverse of that order we destroy
    // each table.
    let modelNames        = Object.keys(modelMap);
    let sortedModelNames  = Utils.sortModelNamesByCreationOrder(this, modelNames);
    let results           = [];

    for (let i = sortedModelNames.length - 1; i >= 0; i--) {
      let modelName = sortedModelNames[i];
      let Model     = modelMap[modelName];

      results.push(await this.dropTable(Model, options));
    }

    return results;
  }

  async createTable(Model, options) {
    throw new Error(`${this.constructor.name}::createTable: This operation is not supported for this connection type.`);
  }

  async createTables(_Models, options) {
    if (!_Models)
      return;

    // First we collect all models and put them into a map
    let modelMap = _Models;

    if (Nife.instanceOf(_Models, 'array', 'function')) {
      modelMap = {};

      let Models = Nife.toArray(_Models).filter(Boolean);
      for (let i = 0, il = Models.length; i < il; i++) {
        let Model     = Models[i];
        let modelName = Model.getModelName();

        modelMap[modelName] = Model;
      }
    }

    // Second we sort the model names in creation order,
    // and then create the tables in that order
    let modelNames        = Object.keys(modelMap);
    let sortedModelNames  = Utils.sortModelNamesByCreationOrder(this, modelNames);
    let results           = [];

    for (let i = 0, il = sortedModelNames.length; i < il; i++) {
      let modelName = sortedModelNames[i];
      let Model     = modelMap[modelName];

      results.push(await this.createTable(Model, options));
    }

    return results;
  }

  // Define operations

  async defineTable() {
    throw new Error(`${this.constructor.name}::defineTable: This operation is not supported for this connection type.`);
  }

  async defineConstraints() {
    throw new Error(`${this.constructor.name}::defineConstraints: This operation is not supported for this connection type.`);
  }

  async defineIndexes() {
    throw new Error(`${this.constructor.name}::defineIndexes: This operation is not supported for this connection type.`);
  }

  // Alter operations

  async alterTable(Model, newModelAttributes, options) {
    throw new Error(`${this.constructor.name}::renameTable: This operation is not supported for this connection type.`);
  }

  async dropColumn(Field, options) {
    throw new Error(`${this.constructor.name}::dropColumn: This operation is not supported for this connection type.`);
  }

  async alterColumn(Field, newFieldAttributes, options) {
    throw new Error(`${this.constructor.name}::alterColumn: This operation is not supported for this connection type.`);
  }

  async addColumn(Field, options) {
    throw new Error(`${this.constructor.name}::addColumn: This operation is not supported for this connection type.`);
  }

  async addIndex(Model, indexFields, options) {
    throw new Error(`${this.constructor.name}::addIndex: This operation is not supported for this connection type.`);
  }

  async dropIndex(Model, indexFields, options) {
    throw new Error(`${this.constructor.name}::addIndex: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async insert(Model, models, _options) {
    throw new Error(`${this.constructor.name}::insert: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async upsert(Model, models, _options) {
    throw new Error(`${this.constructor.name}::upsert: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async update(Model, models, _options) {
    throw new Error(`${this.constructor.name}::update: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async updateAll(_queryEngine, model, _options) {
    throw new Error(`${this.constructor.name}::updateAll: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async destroyModels(Model, _models, _options) {
    throw new Error(`${this.constructor.name}::destroyModels: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async destroy(_queryEngineOrModel, modelsOrOptions, _options) {
    throw new Error(`${this.constructor.name}::destroy: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars, require-yield
  async *select(_queryEngine, _options) {
    throw new Error(`${this.constructor.name}::select: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async aggregate(_queryEngine, _literal, options) {
    throw new Error(`${this.constructor.name}::aggregate: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async average(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::average: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async count(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::count: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async min(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::min: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async max(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::max: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async sum(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::sum: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async pluck(_queryEngine, _fields, _options) {
    throw new Error(`${this.constructor.name}::pluck: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async exists(queryEngine, options) {
    throw new Error(`${this.constructor.name}::exists: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async truncate(Model, options) {
    throw new Error(`${this.constructor.name}::truncate: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async query(sql, options) {
    throw new Error(`${this.constructor.name}::transaction: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async transaction(callback, options) {
    throw new Error(`${this.constructor.name}::transaction: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async getDefaultFieldValue(type, context) {
    throw new Error(`${this.constructor.name}::getDefaultFieldValue: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  dirtyFieldHelper(context) {

  }
}

module.exports = ConnectionBase;
