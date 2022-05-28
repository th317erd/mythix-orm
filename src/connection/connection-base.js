'use strict';

const Utils = require('../utils');

class ConnectionBase {
  constructor(options) {
    Object.defineProperties(this, {
      'models': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        {},
      },
    });

    this.registerModels(options.models);
  }

  registerModel(_Model) {
    let Model = _Model.initializeModel(_Model, this);
    this.models[Model.getModelName()] = Model;
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
}

module.exports = ConnectionBase;
