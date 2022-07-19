'use strict';

const { QueryEngine } = require('../query-engine');
const Utils           = require('../utils');

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
    return Utils.parseQualifiedName(str);
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

  async start() {
    throw new Error(`${this.constructor.name}::start: Child class is required to implement "start".`);
  }

  async stop() {
    throw new Error(`${this.constructor.name}::stop: Child class is required to implement "stop".`);
  }
}

module.exports = ConnectionBase;
