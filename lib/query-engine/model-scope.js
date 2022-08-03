'use strict';

const Nife            = require('nife');
const ProxyClass      = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');
const {
  LiteralBase,
  DistinctLiteral,
} = require('../connection/literals');

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

    return this._newFieldScope(field);
  }

  [ProxyClass.MISSING](target, prop) {
    if (prop === 'where')
      return this._fetchScope('model');

    let field = this._getField(prop);
    if (field)
      return this._newFieldScope(field);

    let lowerScope = this._fetchScope('queryEngine');
    return lowerScope[prop];
  }

  unscoped() {
    return this.currentContext.queryEngineScope.unscoped(this.currentContext);
  }

  toString(...args) {
    if (args.length === 0)
      return `${this.constructor.name} {}`;

    return this.currentContext.queryEngineScope.toString(...args);
  }

  toSQL() {
    return this.currentContext.queryEngineScope.toSQL();
  }

  NOT = ProxyClass.autoCall(function() {
    this._addToQuery({ logical: true, operator: 'NOT', not: !this.currentContext.not });
    return this._fetchScope('model');
  });

  AND = ProxyClass.autoCall(function(value) {
    this._addToQuery({ logical: true, operator: 'AND', and: true, or: false, not: false, value });
    return this._fetchScope('model');
  });

  OR = ProxyClass.autoCall(function(value) {
    this._addToQuery({ logical: true, operator: 'OR', and: false, or: true, not: false, value });
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

    values = Nife.toArray(values).map((value) => {
      if (value == null)
        return;

      // Pass literals directly through
      if (value instanceof LiteralBase)
        return value;

      // Is the projection a model?
      if (value._isModelClass)
        return value;

      // Is the projection a field?
      if (value.Model && value.fieldName)
        return value;

      if (!Nife.instanceOf(value, 'string'))
        throw new Error('QueryEngine::ModelScope::PROJECT: Invalid value provided. All values provided must be strings.');

      return value;
    }).filter(Boolean);

    this._addToQuery({ control: true, operator: 'PROJECT', value: values });
    return this._fetchScope('model');
  }

  DISTINCT = ProxyClass.autoCall(function(fullyQualifiedName) {
    if (arguments.length === 0) {
      let context = this._getRawQueryContext();
      let rootModel = context.rootModel;
      if (!rootModel)
        throw new Error(`${this.constructor.name}::DISTINCT: Attempted to apply DISTINCT to the root model of the query, but no root model was found.`);

      let pkFieldName = rootModel.getPrimaryKeyFieldName();
      if (!pkFieldName)
        throw new Error(`${this.constructor.name}::DISTINCT: Attempted to apply DISTINCT to the root model of the query, but the root model has no primary key. Try directly specifying the DISTINCT field instead.`);

      this.PROJECT(`-${rootModel.getModelName()}:${pkFieldName}`, '+', new DistinctLiteral(`${rootModel.getModelName()}:${pkFieldName}`));
    } else {
      return this.PROJECT(`-${fullyQualifiedName}`, '+', new DistinctLiteral(fullyQualifiedName));
    }
  });

  INNER_JOIN = ProxyClass.autoCall(function() {
    this._addToQuery({ control: true, operator: 'JOIN', value: 'inner', joinType: 'inner', joinOuter: false });
    return this._fetchScope('model');
  });

  LEFT_JOIN = ProxyClass.autoCall(function(outerJoin) {
    this._addToQuery({ control: true, operator: 'JOIN', value: 'left', joinType: 'left', joinOuter: !!outerJoin });
    return this._fetchScope('model');
  });

  RIGHT_JOIN = ProxyClass.autoCall(function(outerJoin) {
    this._addToQuery({ control: true, operator: 'JOIN', value: 'right', joinType: 'right', joinOuter: !!outerJoin });
    return this._fetchScope('model');
  });

  FULL_JOIN = ProxyClass.autoCall(function(outerJoin) {
    this._addToQuery({ control: true, operator: 'JOIN', value: 'full', joinType: 'full', joinOuter: !!outerJoin });
    return this._fetchScope('model');
  });

  CROSS_JOIN = ProxyClass.autoCall(function() {
    this._addToQuery({ control: true, operator: 'JOIN', value: 'cross', joinType: 'cross', joinOuter: false });
    return this._fetchScope('model');
  });

  JOIN(type) {
    if (Nife.isEmpty(type) || !Nife.instanceOf(type, 'string'))
      throw new Error('QueryEngine::ModelScope::JOIN: Invalid value provided. Value must be a valid string specifying JOIN type.');

    this._addToQuery({ control: true, operator: 'JOIN', value: type, joinType: type, joinOuter: false });
    return this._fetchScope('model');
  }
}

module.exports = ModelScope;
