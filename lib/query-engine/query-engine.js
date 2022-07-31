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

  MERGE(queryEngine) {
    let sourceQuery = queryEngine._getRawQuery();
    let currentModel = this._getRawQueryContext().Model;

    for (let i = 0, il = sourceQuery.length; i < il; i++) {
      let queryPart     = sourceQuery[i];
      let mergeContext  = Utils.flattenObjectProperties(queryPart, [
        // Skip the following keys
        // as they are provided by the
        // parent queryEngine
        'connection',
        'contextID',
        'fieldContext',
        'isQueryContext',
        'modelContext',
        'partIndex',
        'partParentQuery',
        'queryEngineScope',
        'queryRoot',
        'rootContext',
        'rootModel',
        'rootModelName',
      ]);

      // Skip unneeded duplicate model entries
      if (mergeContext.operator === 'MODEL') {
        if (mergeContext.Model === currentModel)
          continue;

        currentModel = mergeContext.Model;
      }

      this._addToQuery(mergeContext);
    }

    return this;
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

  async update(attributes, options) {
    let connection = this.getConnection();
    return await connection.updateAll(this, attributes, options);
  }

  async destroy(options) {
    let connection = this.getConnection();
    return await connection.destroy(this, options);
  }

  async average(field, options) {
    let connection = this.getConnection();
    return await connection.average(this, field, options);
  }

  async count(field, options) {
    let connection = this.getConnection();
    return await connection.count(this, field, options);
  }

  async min(field, options) {
    let connection = this.getConnection();
    return await connection.min(this, field, options);
  }

  async max(field, options) {
    let connection = this.getConnection();
    return await connection.max(this, field, options);
  }

  async sum(field, options) {
    let connection = this.getConnection();
    return await connection.sum(this, field, options);
  }

  async pluck(...fields) {
    let connection = this.getConnection();
    return await connection.pluck(this, fields);
  }

  async exists(options) {
    let connection = this.getConnection();
    return await connection.exists(this, options);
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
