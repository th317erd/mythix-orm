'use strict';

const Nife            = require('nife');
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
    let context = Object.assign(
      Object.create(_context || {}),
      {
        currentScopeName:        'queryEngine',
        isQueryOperationContext: true,
        contextID:               QueryEngineBase.generateID(),
      },
    );

    super(context);

    context.queryEngineScope = this;
  }

  Model(modelName) {
    let model = this.getModel(modelName);
    if (!model)
      throw new Error(`QueryEngine::Model: Requested model "${modelName}" not found.`);

    return this._newModelScope(model);
  }

  unscoped() {
    let context           = this.getOperationContext();
    let QueryEngineClass  = this.constructor;
    let queryEngine       = new QueryEngineClass({
      connection: this.getConnection(),
    });

    if (context.rootModelName)
      queryEngine = queryEngine[context.rootModelName];

    return queryEngine;
  }

  toString(...args) {
    let connection      = this.getConnection();
    let queryGenerator  = connection.getQueryGenerator();

    return queryGenerator.toConnectionString(this, ...args);
  }

  MERGE(_incomingQueryEngine) {
    let incomingQueryEngine = _incomingQueryEngine;
    if (!incomingQueryEngine)
      return this;

    let thisQueryContext = this.getOperationContext();
    if (!QueryEngine.isQuery(incomingQueryEngine) && Nife.instanceOf(incomingQueryEngine, 'array', 'object', 'map', 'set'))
      incomingQueryEngine = Utils.generateQueryFromFilter(this.getConnection(), thisQueryContext.rootModel, incomingQueryEngine);

    let incomingOperationStack  = incomingQueryEngine.getOperationStack();
    let skippingPrefix          = true;
    let queryEngine             = this.clone();

    for (let i = 0, il = incomingOperationStack.length; i < il; i++) {
      let queryPart = incomingOperationStack[i];

      // For merges, we want to skip the first logical operators
      // found before any other operation.
      // This is because one might do a: Model.where.OR.MERGE(Model.AND.field.EQ()).
      // This query, once merged, would be: Model.where.OR.Model.AND.field.EQ(),
      // which is not what we want... instead we want: Model.where.OR.Model.field.EQ().
      // Since the result we want here is OR merge, not AND merge
      // we skip the first "AND" we encounter, leaving the "OR" as
      // the current logical operator.
      if (skippingPrefix && Object.prototype.hasOwnProperty.call(queryPart, 'logical') && queryPart.logical) {
        if (queryPart.value == null && (queryPart.operator === 'AND' || queryPart.operator === 'OR'))
          continue;
      }

      skippingPrefix = false;

      let value = queryPart.value;
      if (Object.prototype.hasOwnProperty.call(queryPart, 'control') && queryPart.control === true) {
        if (queryPart.operator === 'PROJECT' || queryPart.operator === 'ORDER' || queryPart.operator === 'GROUP_BY')
          value = [ '+' ].concat(value);
      }

      queryEngine = queryEngine[queryPart.queryProp](value, ...(queryPart.queryExtraArgs || []));
    }

    return queryEngine;
  }

  async all(options) {
    let connection = this.getConnection(options && options.connection);

    if (options && options.stream)
      throw new TypeError('QueryEngine::all: "stream" option set to true. For streaming, please use the "cursor" method instead.');

    return await Utils.collect(connection.select(await this.finalizeQuery('read', options), options));
  }

  async *cursor(_options) {
    let options     = _options || {};
    let connection  = this.getConnection(options && options.connection);
    return yield *connection.select(await this.finalizeQuery('read', options), { ...options, stream: true });
  }

  async first(_limit, options) {
    let limit       = (_limit == null) ? 1 : _limit;
    let connection  = this.getConnection(options && options.connection);
    let query       = (await this.finalizeQuery('read', options)).clone().LIMIT(limit);

    let result = await Utils.collect(connection.select(query, options));
    return (_limit == null) ? result[0] : result;
  }

  async last(_limit, options) {
    let limit       = (_limit == null) ? 1 : _limit;
    let connection  = this.getConnection(options && options.connection);
    let query       = (await this.finalizeQuery('read', options)).clone().LIMIT(limit);

    let result = await Utils.collect(connection.select(query, Object.assign({}, options || {}, { reverseOrder: true })));
    return (_limit == null) ? result[0] : result.reverse();
  }

  async updateAll(attributes, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.updateAll(await this.finalizeQuery('update', options), attributes, options);
  }

  async destroy(options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.destroy(await this.finalizeQuery('delete', options), options);
  }

  async average(field, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.average(await this.finalizeQuery('read', options), field, options);
  }

  async count(field, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.count(await this.finalizeQuery('read', options), field, options);
  }

  async min(field, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.min(await this.finalizeQuery('read', options), field, options);
  }

  async max(field, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.max(await this.finalizeQuery('read', options), field, options);
  }

  async sum(field, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.sum(await this.finalizeQuery('read', options), field, options);
  }

  async pluck(fields, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.pluck(await this.finalizeQuery('read', options), fields, options);
  }

  async exists(options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.exists(await this.finalizeQuery('read', options), options);
  }

  async finalizeQuery(crudOperation, options) {
    let connection = this.getConnection();
    if (connection && typeof connection.finalizeQuery === 'function')
      return await connection.finalizeQuery(crudOperation, this, options);

    return this;
  }

  [ProxyClass.MISSING](target, prop) {
    if (prop === 'debug') {
      this.currentContext.rootContext.debug = true;
      return this._fetchScope('model', 'queryEngine');
    }

    let model = this.getModel(prop);
    if (model) {
      return this._newModelScope(model).__call(function(...args) {
        let fieldName = args.find((arg) => (arg && arg.constructor && !arg.constructor._isMythixConnection));
        if (!fieldName)
          return this;

        return this.Field(fieldName);
      });
    }

    return this[prop];
  }
}

module.exports = QueryEngine;
