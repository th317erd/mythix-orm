'use strict';

const ProxyClass = require('../proxy-class');

class QueryEngineBase extends ProxyClass {
  getModelScopeClass() {
    return this.getQueryEngineScope().getModelScopeClass();
  }

  getFieldScopeClass() {
    return this.getQueryEngineScope().getFieldScopeClass();
  }

  _inheritContext(context, name, ...args) {
    let newContext = Object.assign(Object.create(context), ...args, { level: (context.level || 0) + 1 });

    if (name) {
      newContext[`${name}Context`] = newContext;
      newContext['currentScopeName'] = name;
    }

    return newContext;
  }

  _fetchContext(name) {
    return this.currentContext[`${name}Context`];
  }

  _fetchScope(scopeName, fallbackScopeName) {
    let context = this._getRawQueryContext();
    let scope = context[`${scopeName}Scope`];
    if (!scope)
      scope = context[`${fallbackScopeName}Scope`];

    return scope;
  }

  _newQueryEngineScope(context, props) {
    const QueryEngine = this.getQueryEngineClass();
    let newContext    = this._inheritContext(context, 'queryEngine', props || {});
    let newScope      = new QueryEngine(newContext);

    return newScope;
  }

  _newModelScope(context, Model, props) {
    let ModelScopeClass = this.getModelScopeClass();
    let extra           = {};
    let modelName       = Model.getModelName();

    if (!context.rootModelName)
      extra.rootModelName = modelName;

    let newContext  = this._inheritContext(context, 'model', { Model, modelName }, props || {}, extra);
    let newScope    = new ModelScopeClass(newContext);

    this._addToQuery({ operator: 'MODEL' }, newContext);

    return newScope;
  }

  _newFieldScope(context, Field, props) {
    let FieldScopeClass = this.getFieldScopeClass();
    let fieldName       = Field.fieldName;

    let newContext  = this._inheritContext(context, 'field', props || {}, { Field, fieldName });
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

    context[`${context.currentScopeName || 'queryEngine'}Scope`] = this;
    context[`${context.currentScopeName || 'queryEngine'}Context`] = context;

    if (!context.rootContext)
      context.rootContext = context;

    // console.log(`Creating new ${this.constructor.name} scope: `, context, Object.getPrototypeOf(context));

    Object.defineProperties(this, {
      ['currentContext']: {
        enumberable:  false,
        configurable: true,
        get:          () => context,
        set:          () => {},
      },
    });
  }

  _addToQuery(queryPart, _context) {
    let currentQuery = (_context || this.currentContext).query;

    currentQuery.push(
      this._inheritContext(
        _context || this.currentContext,
        null,
        queryPart,
        {
          partIndex:        currentQuery.length,
          partParentQuery:  currentQuery,
        },
      ),
    );
  }

  _getRawQueryContext() {
    let rawQuery = this._getRawQuery();
    return rawQuery[rawQuery.length - 1] || this.currentContext;
  }

  _getRawQuery() {
    return this.currentContext.queryRoot;
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
}

module.exports = QueryEngineBase;
