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

  static isQueryContext(value) {
    return !!(value && value.isQueryContext);
  }

  static isQuery(value) {
    if (!value)
      return false;

    if (value instanceof QueryEngineBase)
      return true;

    if (value._isQueryEngine)
      return true;

    if (typeof value._getRawQueryContext === 'function')
      return true;

    return false;
  }

  static queryContextType(queryContext) {
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
    let context = this._getRawQueryContext();

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
    const QueryEngine = this.getQueryEngineClass();
    let context       = this.currentContext;
    let newContext    = this._inheritContext(context, 'queryEngine');
    let newScope      = new QueryEngine(newContext);

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

    let newContext  = this._inheritContext(context, 'model', { operator: 'MODEL', Model, modelName, dialect }, extra);
    let newScope    = new ModelScopeClass(newContext);

    // We shouldn't add this scope if this is
    // already the current model of the scope
    if (context.Model !== Model)
      context.queryRoot.push(newContext);

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

    let newContext  = this._inheritContext(context, 'field', { operator: 'FIELD', fieldName, Field }, extra);
    let newScope    = new FieldScopeClass(newContext);

    // We shouldn't add this scope if this is
    // already the current field of the scope
    if (context.Field !== Field)
      context.queryRoot.push(newContext);

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

    if (!context.queryRoot)
      context.queryRoot = [];

    if (!('and' in context))
      context.and = true;

    if (!('or' in context))
      context.or = false;

    // console.log(`Creating new ${this.constructor.name} scope: `, context, Object.getPrototypeOf(context));

    let queryRoot = context.queryRoot;
    Object.defineProperties(this, {
      queryRoot: {
        writable:     false,
        enumerable:   false,
        configurable: true,
        value:        queryRoot,
      },
      currentContext: {
        enumerable:   false,
        configurable: true,
        get:          () => {
          let currentContext = queryRoot[queryRoot.length - 1];
          return currentContext || context;
        },
        set:          () => {},
      },
    });
  }

  _getTopContextID() {
    return this.currentContext.contextID;
  }

  _getRawQueryContext() {
    return this.currentContext;
  }

  _getRawQuery() {
    return this.currentContext.queryRoot;
  }

  _isLastPartControl() {
    let queryParts  = this._getRawQuery();
    let lastPart    = queryParts[queryParts.length - 1];

    if (Object.prototype.hasOwnProperty.call(lastPart, 'control') && lastPart.control === true)
      return true;

    return false;
  }

  _isLastPartCondition() {
    let queryParts  = this._getRawQuery();
    let lastPart    = queryParts[queryParts.length - 1];

    if (Object.prototype.hasOwnProperty.call(lastPart, 'condition') && lastPart.condition === true)
      return true;

    return false;
  }

  _queryHasConditions() {
    let context = this._getRawQueryContext();
    return context.hasCondition;
  }

  _queryHasJoins() {
    let queryParts = this._getRawQuery();
    for (let i = 0, il = queryParts.length; i < il; i++) {
      let queryPart = queryParts[i];
      if (QueryEngineBase.isQuery(queryPart.value) && !queryPart.value.hasCondition)
        return true;
    }

    return false;
  }

  _debugQuery() {
    let query = this._getRawQuery();
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

  _addToQuery(queryPart, _context) {
    let context   = _context || this._getRawQueryContext();
    let queryRoot = context.queryRoot;

    queryRoot.push(
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
    const Klass           = this.constructor;
    let context           = this._getRawQueryContext();
    let queryRootCopy     = this._getRawQuery().slice();
    let newContext        = Object.assign(Object.create(context), { queryRoot: queryRootCopy });

    queryRootCopy.push(newContext);

    return new Klass(newContext);
  }
}

module.exports = QueryEngineBase;
