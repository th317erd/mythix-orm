'use strict';

const ProxyClass      = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');
const ModelScope      = require('./model-scope');
const FieldScope      = require('./field-scope');

class QueryEngine extends QueryEngineBase {
  getModelScopeClass() {
    return ModelScope;
  }

  getFieldScopeClass() {
    return FieldScope;
  }

  constructor(_context) {
    if (!_context)
      throw new TypeError('QueryEngine::constructor: "context" required.');

    let context = Object.assign(
      Object.create(_context),
      {
        currentScopeName: 'queryEngine',
        isQueryContext:   true,
        contextID:        QueryEngineBase.generateID(),
      },
    );

    if (!context.queryRoot) {
      let queryRoot = [];

      context.queryRoot = queryRoot;
      context.query = queryRoot;
    }

    context.queryEngineContext = context;

    super(context);

    if (!('and' in context))
      context.and = true;

    if (!('or' in context))
      context.or = false;
  }

  Model(modelName) {
    let model = this.getConnection().getModel(modelName);
    if (!model)
      throw new Error(`QueryEngine::Model: Requested model "${modelName}" not found.`);

    return this._newModelScope(this.currentContext, model);
  }

  unscoped(context) {
    let QueryEngineClass  = this.constructor;
    let currentContext    = context || this.currentContext;
    let queryEngine       = new QueryEngineClass({
      connection: currentContext.connection,
    });

    if (currentContext.rootModelName)
      queryEngine = queryEngine[currentContext.rootModelName];

    return queryEngine;
  }

  [ProxyClass.MISSING](target, prop) {
    if (prop === 'debug') {
      this.currentContext.rootContext.debug = true;
      return this._fetchScope('model', 'queryEngine');
    }

    let model = this.getConnection().getModel(prop);
    if (model) {
      return this._newModelScope(this.currentContext, model).__call(function(fieldName) {
        return this.Field(fieldName);
      });
    }

    return this[prop];
  }
}

module.exports = QueryEngine;
