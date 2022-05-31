'use strict';

const ProxyClass = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');

class ModelScope extends QueryEngineBase {
  _getField(fieldName) {
    let Model = this.currentContext.Model;
    return Model.getField(fieldName);
  }

  _getQueryEngineClass() {
    return this.currentContext.queryEngine;
  }

  Field(fieldName) {
    let field = this._getField(fieldName);
    if (!field)
      throw new Error(`QueryEngine::ModelScope::Field: Requested field "${fieldName}" not found.`);

    return this._newFieldScope(this.currentContext, field);
  }

  [ProxyClass.MISSING](target, prop) {
    if (prop === 'where')
      return this._fetchScope('model');

    let field = this._getField(prop);
    if (field)
      return this._newFieldScope(this.currentContext, field);

    let lowerScope = this._fetchScope('queryEngine');
    return lowerScope[prop];
  }

  unscoped() {
    return this.currentContext.queryEngineScope.unscoped(this.currentContext);
  }

  NOT = ProxyClass.autoCall(function() {
    this._addToQuery({ operator: 'NOT', not: !this.currentContext.not });
    return this._fetchScope('model');
  });

  AND = ProxyClass.autoCall(function() {
    this._addToQuery({ operator: 'AND', and: true, or: false });
    return this._fetchScope('model');
  });

  OR = ProxyClass.autoCall(function() {
    this._addToQuery({ operator: 'OR', and: false, or: true });
    return this._fetchScope('model');
  });
}

module.exports = ModelScope;
