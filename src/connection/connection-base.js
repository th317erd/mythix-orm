'use strict';

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

  getModel(modelName) {
    return this.models[modelName];
  }
}

module.exports = ConnectionBase;
