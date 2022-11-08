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
        currentScopeName: 'queryEngine',
        isQueryContext:   true,
        contextID:        QueryEngineBase.generateID(),
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
    let connection      = this.getConnection();
    let queryGenerator  = connection.getQueryGenerator();

    return queryGenerator.toConnectionString(this, ...args);
  }

  MERGE(_queryEngine) {
    let queryEngine = _queryEngine;
    if (!queryEngine)
      return this;

    let thisQueryContext = this.getOperationContext();
    if (!QueryEngine.isQuery(queryEngine) && Nife.instanceOf(queryEngine, 'array', 'object', 'map', 'set'))
      queryEngine = Utils.generateQueryFromFilter(this.getConnection(), thisQueryContext.rootModel, queryEngine);

    let sourceQuery           = queryEngine.getOperationStack();
    let currentModel          = thisQueryContext.Model;
    let skipLogicalOperators  = true;

    for (let i = 0, il = sourceQuery.length; i < il; i++) {
      let queryPart     = sourceQuery[i];
      let mergeContext  = Utils.objectAssignSpecial(queryPart, null, [
        // Skip the following keys
        // as they are provided by the
        // parent queryEngine
        'connection',
        'fieldContext',
        'isQueryContext',
        'modelContext',
        'queryEngineScope',
        'operationStack',
        'rootContext',
        'rootModel',
        'rootModelName',
        '_queryFinalized',
      ]);

      // For merges, we want to skip the first logical operators
      // found before any other operation.
      // This is because one might do a: Model.where.OR.MERGE(Model.AND.field.EQ()).
      // This query, once merged, would be: Model.where.OR.Model.AND.field.EQ(),
      // which is not what we want... instead we want: Model.where.OR.Model.field.EQ().
      // Since the result we want here is OR merge, not AND merge
      // we skip the first "AND" we encounter, leaving the "OR" as
      // the current logical operator.
      if (skipLogicalOperators && Object.prototype.hasOwnProperty.call(mergeContext, 'logical') && mergeContext.logical) {
        if (mergeContext.value == null && (mergeContext.operator === 'AND' || mergeContext.operator === 'OR'))
          continue;
      }

      if (Object.prototype.hasOwnProperty.call(mergeContext, 'operator')) {
        // Skip unneeded duplicate model entries
        if (mergeContext.operator === 'MODEL') {
          if (mergeContext.Model === currentModel)
            continue;

          currentModel = mergeContext.Model;
        } else if (mergeContext.operator !== 'FIELD') {
          skipLogicalOperators = false;
        }
      }

      this._pushOperationOntoStack(Object.assign({
        Model:      queryPart.Model,
        modelName:  queryPart.modelName,
        Field:      queryPart.Field,
        fieldName:  queryPart.fieldName,
      }, mergeContext));
    }

    return this;
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
    const finalizeQueryForModel = async (query, parent, contextKey, depth) => {
      let operations = query.getOperationStack();

      // Has query already been finalized?
      let lastOperation = operations[operations.length - 1];
      if (lastOperation && Object.prototype.hasOwnProperty.call(lastOperation, '_queryFinalized') && lastOperation._queryFinalized)
        return query;

      let newQuery              = query.clone();
      let alreadyVisitedModels  = new Set();

      for (let i = 0, il = operations.length; i < il; i++) {
        let operation = operations[i];
        if (!Object.prototype.hasOwnProperty.call(operation, 'modelName'))
          continue;

        let modelName = operation.modelName;
        if (alreadyVisitedModels.has(modelName))
          continue;

        alreadyVisitedModels.add(modelName);

        let Model = operation.Model;
        if (typeof Model.finalizeQuery !== 'function')
          continue;

        newQuery = await Model.finalizeQuery({
          type:           crudOperation,
          query:          newQuery,
          queryDepth:     depth,
          operationIndex: i,
          connection,
          Model,
          modelName,
          operation,
          operations,
          parent,
          contextKey,
          options,
        });

        if (parent && contextKey)
          parent[contextKey] = newQuery;
      }

      // Mark as finalized
      newQuery.getOperationContext()._queryFinalized = true;

      return newQuery;
    };

    let connection  = this.getConnection();
    let promises    = [];

    this.walk((query, parent, contextKey, depth) => {
      promises.push(finalizeQueryForModel(query, parent, contextKey, depth));
    });

    if (promises.length > 0)
      await Promise.all(promises);

    return await finalizeQueryForModel(this, null, null, 0);
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
