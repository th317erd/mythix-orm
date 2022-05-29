'use strict';

const ProxyClass = require('../proxy-class');

class QueryEngineBase extends ProxyClass {
  getModel() {
    return this.getQueryEngineClass().getModel();
  }

  getFieldClass() {
    return this.getQueryEngineClass().getFieldClass();
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

  _fetchScope(scopeName) {
    return this.currentContext[`${scopeName}Scope`];
  }

  _newModelScope(context, Model) {
    let ModelScopeClass = this.getModel();
    let extra           = {};
    let modelName       = Model.getModelName();

    if (!context.rootModelName)
      extra.rootModelName = modelName;

    let newContext  = this._inheritContext(context, 'model', { Model, modelName }, extra);
    let newScope    = new ModelScopeClass(newContext);

    this._addToQuery({ operator: 'MODEL' }, newContext);

    return newScope;
  }

  _newFieldScope(context, Field) {
    let FieldScopeClass = this.getFieldClass();
    let fieldName       = Field.fieldName;

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

    context[`${context.currentScopeName || 'queryEngine'}Scope`] = this;
    context[`${context.currentScopeName || 'queryEngine'}Context`] = context;

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

  _getRawQuery() {
    return this.currentContext.queryRoot;
  }

  getConnection() {
    return this.currentContext.connection;
  }

  getQueryEngineClass() {
    return this.currentContext.queryEngineScope;
  }
}

module.exports = QueryEngineBase;
