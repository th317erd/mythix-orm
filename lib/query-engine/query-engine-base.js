'use strict';

const ProxyClass = require('../proxy-class');

let uuid = 1;

function generateID() {
  return uuid++;
}

class QueryEngineBase extends ProxyClass {
  static generateID() {
    return uuid++;
  }

  static isQueryOperationContext(value) {
    return !!(value && value.isQueryOperationContext);
  }

  static isQuery(value) {
    if (!value)
      return false;

    if (value instanceof QueryEngineBase)
      return true;

    if (value._isQueryEngine)
      return true;

    if (typeof value.getOperationContext === 'function')
      return true;

    return false;
  }

  static queryOperationInfo(queryContext) {
    let contextParams = {
      hasCondition: false,
      hasField:     false,
      hasModel:     false,
    };

    if (!queryContext)
      return contextParams;

    if (queryContext.condition)
      contextParams.hasCondition = true;

    if (queryContext.fieldName)
      contextParams.hasField = true;

    if (queryContext.Model)
      contextParams.hasModel = true;

    return contextParams;
  }

  getModelScopeClass() {
    return this.getQueryEngineScope().getModelScopeClass();
  }

  getFieldScopeClass() {
    return this.getQueryEngineScope().getFieldScopeClass();
  }

  getQueryEngineScopeClass() {
    return this.getQueryEngineScope().constructor;
  }

  _inheritContext(context, name, ...args) {
    let newContext = Object.assign(
      Object.create(context),
      { value: undefined },
      ...args,
      {
        contextID: generateID(),
      },
    );

    if (name) {
      newContext[`${name}Context`] = newContext;
      newContext['currentScopeName'] = name;
    }

    return newContext;
  }

  _fetchScope(...scopeNames) {
    let context = this.getOperationContext();

    for (let i = 0, il = scopeNames.length; i < il; i++) {
      let scopeName = scopeNames[i];
      if (scopeName === 'queryEngine') {
        return this._newQueryEngineScope();
      } else if (scopeName === 'model') {
        let Model = context.Model;
        if (!Model)
          continue;

        return this._newModelScope(Model);
      } else if (scopeName === 'field') {
        let Field = context.Field;
        if (!Field)
          continue;

        return this._newFieldScope(context.Field);
      }
    }

    return this;
  }

  _newQueryEngineScope() {
    const QueryEngineClass  = this.getQueryEngineClass();
    let context             = this.currentContext;
    let newContext          = this._inheritContext(context, 'queryEngine');
    let newScope            = new QueryEngineClass(newContext);

    return newScope;
  }

  _newModelScope(Model) {
    let ModelScopeClass = this.getModelScopeClass();
    let modelName       = Model.getModelName();
    let context         = this.currentContext;
    let extra           = {};

    if (!context.rootModel) {
      extra.rootModelName = modelName;
      extra.rootModel = Model;
    }

    // Ensure that the specified model
    // is the same dialect as the query
    let modelConnection = Model._getConnection();
    let dialect;

    if (modelConnection) {
      dialect = modelConnection.dialect;
      if (context.dialect && context.dialect !== dialect)
        throw new Error(`QueryEngine: Model "${Model.getModelName()}" is on a different connection dialect than the current query dialect of "${context.dialect}". You can not match different connection dialects in the same query.`);
    }

    if (!dialect) {
      let connection = this.getConnection();
      if (connection)
        dialect = connection.dialect;
    }

    let newContext  = this._inheritContext(context, 'model', { operator: 'MODEL', queryProp: 'Model', Model, modelName, dialect, value: modelName }, extra);
    let newScope    = new ModelScopeClass(newContext);

    // We shouldn't add this scope if this is
    // already the current model of the scope
    if (context.Model !== Model)
      context.operationStack.push(newContext);

    // But we always need to return the scope
    // for the proxy to work properly
    return newScope;
  }

  _newFieldScope(Field) {
    let fieldName       = Field.fieldName;
    let context         = this.currentContext;
    let FieldScopeClass = this.getFieldScopeClass();
    let extra           = {};

    if (!context.rootField) {
      extra.rootFieldName = fieldName;
      extra.rootField = Field;
    }

    let newContext  = this._inheritContext(context, 'field', { operator: 'FIELD', queryProp: 'Field', fieldName, Field, value: fieldName }, extra);
    let newScope    = new FieldScopeClass(newContext);

    // We shouldn't add this scope if this is
    // already the current field of the scope
    if (context.Field !== Field)
      context.operationStack.push(newContext);

    // But we always need to return the scope
    // for the proxy to work properly
    return newScope;
  }

  constructor(context) {
    super();

    this._isQueryEngine = true;

    if (!context)
      throw new TypeError('QueryEngineBase::constructor: "context" required.');

    if (!context.rootContext) {
      context._isQueryEngine = true;
      context.rootContext = context;
    }

    if (!context.operationStack)
      context.operationStack = [];

    if (!('and' in context))
      context.and = true;

    if (!('or' in context))
      context.or = false;

    // console.log(`Creating new ${this.constructor.name} scope: `, context, Object.getPrototypeOf(context));

    let operationStack = context.operationStack;
    Object.defineProperties(this, {
      operationStack: {
        writable:     false,
        enumerable:   false,
        configurable: true,
        value:        operationStack,
      },
      currentContext: {
        enumerable:   false,
        configurable: true,
        get:          () => {
          let currentContext = operationStack[operationStack.length - 1];
          return currentContext || context;
        },
        set: () => {},
      },
    });
  }

  getQueryID() {
    return this.currentContext.contextID;
  }

  getOperationContext() {
    return this.currentContext;
  }

  getOperationStack() {
    return this.currentContext.operationStack;
  }

  isLastOperationControl() {
    let queryParts  = this.getOperationStack();
    let lastPart    = queryParts[queryParts.length - 1];

    if (Object.prototype.hasOwnProperty.call(lastPart, 'control') && lastPart.control === true)
      return true;

    return false;
  }

  isLastOperationCondition() {
    let queryParts  = this.getOperationStack();
    let lastPart    = queryParts[queryParts.length - 1];

    if (Object.prototype.hasOwnProperty.call(lastPart, 'condition') && lastPart.condition === true)
      return true;

    return false;
  }

  queryHasConditions() {
    let context = this.getOperationContext();
    return context.hasCondition;
  }

  queryHasJoins() {
    let queryParts = this.getOperationStack();
    for (let i = 0, il = queryParts.length; i < il; i++) {
      let queryPart = queryParts[i];
      if (QueryEngineBase.isQuery(queryPart.value) && !queryPart.value.hasCondition)
        return true;
    }

    return false;
  }

  logQueryOperations() {
    let query = this.getOperationStack();
    for (let i = 0, il = query.length; i < il; i++) {
      let queryPart = query[i];
      let operator  = queryPart.operator;

      if (operator === 'MODEL')
        console.log(`MODEL -> ${queryPart.Model.getModelName()}`);
      else if (operator === 'FIELD')
        console.log(`FIELD -> ${queryPart.Field.fieldName}`);
      else
        console.log(`${operator} -> ${queryPart.value}`);
    }
  }

  _pushOperationOntoStack(queryPart, _context) {
    let context   = _context || this.getOperationContext();
    let operationStack = context.operationStack;

    operationStack.push(
      this._inheritContext(
        context,
        null,
        queryPart,
      ),
    );
  }

  getConnection() {
    return this.currentContext.connection;
  }

  getModel(modelName) {
    let connection  = this.getConnection();
    let Model       = (connection && connection.getModel(modelName));

    if (!Model) {
      let models = this.currentContext.models;
      if (models)
        Model = models[modelName];
    }

    return Model;
  }

  getQueryEngineScope() {
    return this.currentContext.queryEngineScope;
  }

  getQueryEngineClass() {
    return this.currentContext.queryEngineScope.constructor;
  }

  clone() {
    return this.map((part) => part)._fetchScope('model');
  }

  filter(callback) {
    const Klass           = this.getQueryEngineScopeClass();
    let context           = this.getOperationContext();
    let parts             = this.getOperationStack();
    let query             = new Klass({
      ...context.rootContext,
      connection:     this.getConnection(),
      operationStack: [],
    });

    for (let i = 0, il = parts.length; i < il; i++) {
      let part = parts[i];
      if (!part)
        continue;

      if (!callback(part, i, parts, query))
        continue;

      query._pushOperationOntoStack({ ...part });
    }

    return query;
  }

  map(callback) {
    const Klass           = this.getQueryEngineScopeClass();
    let context           = this.getOperationContext();
    let parts             = this.getOperationStack();
    let query             = new Klass({
      ...context.rootContext,
      connection:     this.getConnection(),
      operationStack: [],
    });

    for (let i = 0, il = parts.length; i < il; i++) {
      let part = parts[i];
      if (!part)
        continue;

      let newPart = callback({ ...part }, i, parts, query);
      if (!newPart || typeof newPart !== 'object')
        continue;

      query._pushOperationOntoStack(newPart);
    }

    return query;
  }

  walk(callback, _checkContextKeys) {
    const walkQueries = (query, parent, contextKey, depth) => {
      let parts = query.getOperationStack();
      for (let i = 0, il = parts.length; i < il; i++) {
        let part = parts[i];
        if (!part)
          continue;

        for (let j = 0, jl = checkContextKeys.length; j < jl; j++) {
          let contextKey = checkContextKeys[j];
          if (!Object.prototype.hasOwnProperty.call(part, contextKey))
            continue;

          let value = part[contextKey];
          if (value && this.constructor.isQuery(value))
            walkQueries(value, part, contextKey, depth + 1);
        }
      }

      if (parent !== null)
        callback(query, parent, contextKey, depth);
    };

    let checkContextKeys = _checkContextKeys || [ 'value' ];
    walkQueries(this, null, null, 0);
  }
}

module.exports = QueryEngineBase;
