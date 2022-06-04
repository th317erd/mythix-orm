'use strict';

const Nife            = require('nife');
const ProxyClass      = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');
const SQLLiteralBase  = require('../connection/sql-literals/sql-literal-base');

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
    this._addToQuery({ logical: true, operator: 'NOT', not: !this.currentContext.not });
    return this._fetchScope('model');
  });

  AND = ProxyClass.autoCall(function(value) {
    this._addToQuery({ logical: true, operator: 'AND', and: true, or: false, value });
    return this._fetchScope('model');
  });

  OR = ProxyClass.autoCall(function(value) {
    this._addToQuery({ logical: true, operator: 'OR', and: false, or: true, value });
    return this._fetchScope('model');
  });

  LIMIT(value) {
    if (typeof value !== 'number' || isNaN(value) || value < 0)
      throw new Error('QueryEngine::ModelScope::LIMIT: Value provided must be a valid positive number, or Infinity.');

    this._addToQuery({ control: true, operator: 'LIMIT', value: Math.round(value) });
    return this._fetchScope('model');
  }

  OFFSET(value) {
    if (typeof value !== 'number' || !isFinite(value) || value < 0)
      throw new Error('QueryEngine::ModelScope::OFFSET: Value provided must be a valid positive number.');

    this._addToQuery({ control: true, operator: 'OFFSET', value: Math.round(value) });
    return this._fetchScope('model');
  }

  ORDER(...args) {
    let values = Nife.arrayFlatten(args);

    values = Nife.toArray(values).filter((value) => {
      if (value == null)
        return false;

      if (!Nife.instanceOf(value, 'string'))
        throw new Error('QueryEngine::ModelScope::ORDER: Invalid value provided. All values provided must be strings. If you want to change the sort order of a given column, add "+" (ASC) or "-" (DESC) to be beginning of the field name. Example: .ORDER("+createdAt"), or .ORDER([ "-name", "+createdAt" ]).');

      return true;
    });

    this._addToQuery({ control: true, operator: 'ORDER', value: values });
    return this._fetchScope('model');
  }

  PROJECT(...args) {
    let values = Nife.arrayFlatten(args);

    values = Nife.toArray(values).filter((value) => {
      if (value == null)
        return false;

      if (value instanceof SQLLiteralBase)
        return true;

      if (!Nife.instanceOf(value, 'string'))
        throw new Error('QueryEngine::ModelScope::PROJECT: Invalid value provided. All values provided must be strings.');

      return true;
    });

    this._addToQuery({ control: true, operator: 'PROJECT', value: values });
    return this._fetchScope('model');
  }
}

module.exports = ModelScope;
