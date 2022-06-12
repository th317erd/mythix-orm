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

    return (value instanceof QueryEngineBase);
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
        level:      (context.level || 0) + 1,
        contextID:  generateID(),
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
        let Model = context.Model || context.rootModel;
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
    let extra           = {};
    let modelName       = Model.getModelName();
    let context         = this.currentContext;

    if (!context.rootModelName) {
      extra.rootModelName = modelName;
      extra.rootModel = Model;
    }

    let newContext  = this._inheritContext(context, 'model', { Model, modelName }, extra);
    let newScope    = new ModelScopeClass(newContext);

    this._addToQuery({ operator: 'MODEL' }, newContext);

    return newScope;
  }

  _newFieldScope(Field) {
    let FieldScopeClass = this.getFieldScopeClass();
    let fieldName       = Field.fieldName;
    let context         = this.currentContext;

    let newContext  = this._inheritContext(context, 'field', { Field, fieldName });
    let newScope    = new FieldScopeClass(newContext);

    this._addToQuery({ operator: 'FIELD', fieldName }, newContext);

    return newScope;
  }

  constructor(context) {
    super();

    if (!context)
      throw new TypeError('QueryEngineBase::constructor: "context" required.');

    if (!context.connection)
      throw new TypeError('QueryEngineBase::constructor: "context.connection" is blank, but it must be specified.');

    // context[`${context.currentScopeName || 'queryEngine'}Scope`] = this;
    // context[`${context.currentScopeName || 'queryEngine'}Context`] = context;

    if (!context.rootContext)
      context.rootContext = context;

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
        enumberable:  false,
        configurable: true,
        value:        queryRoot,
      },
      currentContext: {
        enumberable:  false,
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

  _addToQuery(queryPart, _context) {
    let context   = _context || this._getRawQueryContext();
    let queryRoot = context.queryRoot;

    queryRoot.push(
      this._inheritContext(
        context,
        null,
        queryPart,
        {
          partIndex:        queryRoot.length,
          partParentQuery:  queryRoot[queryRoot.length - 1],
        },
      ),
    );
  }

  getConnection() {
    return this.currentContext.connection;
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
