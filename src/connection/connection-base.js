'use strict';

const Nife            = require('nife');
const { QueryEngine } = require('../query-engine');
const ModelUtils      = require('../utils/model-utils');
const ModelBase       = require('../model');

class ConnectionBase {
  static dialect = 'none';

  constructor(_options) {
    let options = Object.assign({
      QueryEngine,
    }, _options || {});

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

    // TODO: Make more efficient by first checking if models are related
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

  async start() {
    throw new Error(`${this.constructor.name}::start: Child class is required to implement "start".`);
  }

  async stop() {
    throw new Error(`${this.constructor.name}::stop: Child class is required to implement "stop".`);
  }
}

module.exports = ConnectionBase;
