'use strict';

const ProxyClass      = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');
const ModelScope      = require('./model-scope');
const FieldScope      = require('./field-scope');

class QueryEngine extends QueryEngineBase {
  getModel() {
    return ModelScope;
  }

  getFieldClass() {
    return FieldScope;
  }

  constructor(_context) {
    if (!_context)
      throw new TypeError('QueryEngine::constructor: "context" required.');

    let queryRoot = [];
    let context = Object.assign({ queryRoot, query: queryRoot }, _context, { currentScopeName: 'queryEngine' });
    context.queryEngineContext = context;

    super(context);

    if (!context.connection)
      throw new TypeError('QueryEngine::constructor: "context.connection" is blank, but it must be specified.');

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

  [ProxyClass.MISSING](target, prop) {
    let model = this.getConnection().getModel(prop);
    if (model) {
      return this._newModelScope(this.currentContext, model).__call(function(fieldName) {
        return this.Field(fieldName);
      });
    }
  }
}

module.exports = QueryEngine;