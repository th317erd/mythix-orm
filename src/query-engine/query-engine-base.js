'use strict';

const ProxyClass = require('../proxy-class');

class QueryEngineBase extends ProxyClass {
  getModel() {
    return this.getQueryEngine().getModel();
  }

  getFieldClass() {
    return this.getQueryEngine().getFieldClass();
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
    return new ModelScopeClass(this._inheritContext(context, 'model', { Model, modelName: Model.getModelName() }));
  }

  _newFieldScope(context, Field) {
    let FieldScopeClass = this.getFieldClass();
    return new FieldScopeClass(this._inheritContext(context, 'field', { Field, fieldName: Field.fieldName }));
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

  _addToQuery(queryPart) {
    let currentQuery = this.currentContext.query;

    currentQuery.push(
      this._inheritContext(
        this.currentContext,
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

  getQueryEngine() {
    return this.currentContext.queryEngineScope;
  }
}

module.exports = QueryEngineBase;
