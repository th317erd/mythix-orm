'use strict';

const ProxyClass      = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');
const ModelScope      = require('./model-scope');
const FieldScope      = require('./field-scope');
const Utils           = require('../utils');

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

    super(context);

    context.queryEngineScope = this;
  }

  Model(modelName) {
    let model = this.getConnection().getModel(modelName);
    if (!model)
      throw new Error(`QueryEngine::Model: Requested model "${modelName}" not found.`);

    return this._newModelScope(model);
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

  toString(...args) {
    if (args.length === 0)
      return `${this.constructor.name} {}`;

    let connection      = args[0];
    let queryGenerator  = connection.getQueryGenerator();

    return queryGenerator.generateSelectStatement(this, args[1]);
  }

  toSQL(options) {
    let connection = this.getConnection();
    return this.toString(connection, options);
  }

  all(options) {
    let connection = this.getConnection();
    return connection.select(this, options);
  }

  async first(_limit, options) {
    let limit       = (_limit == null) ? 1 : _limit;
    let connection  = this.getConnection();
    let query       = this.clone().LIMIT(limit);

    let result = await Utils.collect(connection.select(query, options));
    return (_limit == null) ? result[0] : result;
  }

  async last(_limit, options) {
    let limit       = (_limit == null) ? 1 : _limit;
    let connection  = this.getConnection();
    let query       = this.clone().LIMIT(limit);

    let result = await Utils.collect(connection.select(query, Object.assign({}, options || {}, { reverseOrder: true })));
    return (_limit == null) ? result[0] : result.reverse();
  }

  async pluck(...fields) {
    let connection = this.getConnection();
    return await connection.pluck(this, fields);
  }

  async count(field, options) {
    let connection = this.getConnection();
    return await connection.count(this, field, options);
  }

  [ProxyClass.MISSING](target, prop) {
    if (prop === 'debug') {
      this.currentContext.rootContext.debug = true;
      return this._fetchScope('model', 'queryEngine');
    }

    let model = this.getConnection().getModel(prop);
    if (model) {
      return this._newModelScope(model).__call(function(fieldName) {
        return this.Field(fieldName);
      });
    }

    return this[prop];
  }
}

module.exports = QueryEngine;
