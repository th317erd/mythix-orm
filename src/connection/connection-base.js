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

  registerModel(_Model) {
    let modelName = _Model.getModelName();
    let Model = _Model.initializeModel(_Model, this);
    // console.log('Registering model: ', Model.getModelName(), Model);
    this.models[modelName] = Model;
  }

  registerModels(models) {
    if (!models)
      return;

    let keys = Object.keys(models);
    for (let i = 0, il = keys.length; i < il; i++) {
      let key = keys[i];
      let Model = models[key];

      this.registerModel(Model);
    }
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

  getModel(modelName) {
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
    throw new Error(`${this.constructor.name}::start: Child class is required to implement "start"`);
  }

  async stop() {
    throw new Error(`${this.constructor.name}::stop: Child class is required to implement "stop"`);
  }
}

module.exports = ConnectionBase;
