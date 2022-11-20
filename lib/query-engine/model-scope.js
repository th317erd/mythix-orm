'use strict';

const Nife            = require('nife');
const ProxyClass      = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');
const QueryUtils      = require('../utils/query-utils');
const {
  LiteralBase,
  DistinctLiteral,
} = require('../connection/literals');

function applyOrderClause(extraData, ...args) {
  let entities = Nife.arrayFlatten(args);

  entities = Nife.toArray(entities).map((value) => {
    if (value == null)
      return;

    // Pass literals directly through
    if (LiteralBase.isLiteral(value))
      return value;

    // Is the projection a field?
    if (value.Model && value.fieldName)
      return `${value.Model.getModelName()}:${value.fieldName}`;

    if (!Nife.instanceOf(value, 'string'))
      throw new Error('QueryEngine::ModelScope::ORDER: Invalid value provided. All values provided must be strings, fields, or literals. If you want to change the sort order of a given column, add "+" (ASC) or "-" (DESC) to be beginning of the field name. Example: .ORDER("+createdAt"), or .ORDER([ "-name", "+createdAt" ]).');

    return value;
  }).filter(Boolean);

  let context = this.getOperationContext();
  let order = this.margeFields(
    context.order,
    entities,
    extraData,
    { isOrderBy: true },
  );

  this._pushOperationOntoStack({
    control:   true,
    operator:  'ORDER',
    queryProp: 'ORDER',
    value:     entities,
    order,
  });

  return this._fetchScope('model');
}

function wrapOrderClause(func) {
  func.DESC = (...args) => {
    return applyOrderClause.call(this, { direction: '-' }, ...args);
  };

  func.ASC = (...args) => {
    return applyOrderClause.call(this, { direction: '+' }, ...args);
  };

  return func;
}

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
    if (prop === 'where' || prop === '$')
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
    return this.currentContext.queryEngineScope.toString(...args);
  }

  margeFields(currentFields, incomingFields, extraData, options) {
    return QueryUtils.margeFields(this, currentFields, incomingFields, extraData, options);
  }

  NOT = ProxyClass.autoCall(function() {
    this._pushOperationOntoStack({ logical: true, operator: 'NOT', queryProp: 'NOT', not: !this.currentContext.not });
    return this._fetchScope('model');
  });

  AND = ProxyClass.autoCall(function(value) {
    this._pushOperationOntoStack({ logical: true, operator: 'AND', queryProp: 'AND', and: true, or: false, not: false, value });
    return this._fetchScope('model');
  });

  OR = ProxyClass.autoCall(function(value) {
    this._pushOperationOntoStack({ logical: true, operator: 'OR', queryProp: 'OR', and: false, or: true, not: false, value });
    return this._fetchScope('model');
  });

  LIMIT(_value) {
    let value = _value;
    if (typeof value !== 'number' || isNaN(value) || value < 0)
      throw new Error('QueryEngine::ModelScope::LIMIT: Value provided must be a valid positive number, or Infinity.');

    value = Math.round(value);
    this._pushOperationOntoStack({ control: true, operator: 'LIMIT', queryProp: 'LIMIT', value, limit: value });

    return this._fetchScope('model');
  }

  OFFSET(_value) {
    let value = _value;
    if (typeof value !== 'number' || !isFinite(value) || value < 0)
      throw new Error('QueryEngine::ModelScope::OFFSET: Value provided must be a valid positive number.');

    value = Math.round(value);
    this._pushOperationOntoStack({ control: true, operator: 'OFFSET', queryProp: 'OFFSET', value, offset: value });

    return this._fetchScope('model');
  }

  ORDER = wrapOrderClause.call(this, (...args) => {
    return applyOrderClause.call(this, { direction: '+' }, ...args);
  });

  GROUP_BY(...args) {
    let entities = Nife.arrayFlatten(args);

    entities = Nife.toArray(entities).map((value) => {
      if (value == null)
        return;

      // Pass literals directly through
      if (LiteralBase.isLiteral(value))
        return value;

      // Is the projection a field?
      if (value.Model && value.fieldName)
        return `${value.Model.getModelName()}:${value.fieldName}`;

      if (!Nife.instanceOf(value, 'string'))
        throw new Error('QueryEngine::ModelScope::GROUP_BY: Invalid value provided. All values provided must be strings, fields, or literals. If you want to change the sort order of a given column, add "+" (ASC) or "-" (DESC) to be beginning of the field name. Example: .ORDER("+createdAt"), or .ORDER([ "-name", "+createdAt" ]).');

      return true;
    });

    let context = this.getOperationContext();
    let groupBy = this.margeFields(
      context.groupBy,
      entities,
      {},
      { isGroupBy: true },
    );

    this._pushOperationOntoStack({
      control:   true,
      operator:  'GROUP_BY',
      queryProp: 'GROUP_BY',
      value:     entities,
      groupBy,
    });

    return this._fetchScope('model');
  }

  HAVING(query) {
    this._pushOperationOntoStack({ control: true, operator: 'HAVING', queryProp: 'HAVING', value: query, having: query });
    return this._fetchScope('model');
  }

  EXISTS(_query) {
    let query         = _query;
    let queryContext  = (QueryEngineBase.isQuery(query)) ? query.getOperationContext() : null;
    if (!queryContext || !queryContext.hasCondition)
      throw new Error('QueryEngine::ModelScope::EXISTS: Provided value must be a query with conditions.');

    if (!queryContext.projection) {
      let Model   = queryContext.Model;
      let pkField = Model.getPrimaryKeyField();

      if (pkField)
        query = query.clone().PROJECT(pkField);
      else
        throw new Error('QueryEngine::ModelScope::EXISTS: Provided query must have only a single field projected.');
    }

    this._pushOperationOntoStack({
      condition:       true,
      operator:        'EXISTS',
      inverseOperator: 'NOT EXISTS',
      queryProp:       'EXISTS',
      value:           query,
      having:          query,
      hasCondition:    true,
    });

    return this._fetchScope('model');
  }

  PROJECT(...args) {
    let entities = Nife.arrayFlatten(args);

    entities = Nife.toArray(entities).map((value) => {
      if (value == null)
        return;

      // Pass literals directly through
      if (LiteralBase.isLiteral(value))
        return value;

      // Is the projection a model?
      if (value._isMythixModel)
        return value;

      // Is the projection a field?
      if (value.Model && value.fieldName)
        return value;

      if (!Nife.instanceOf(value, 'string'))
        throw new Error('QueryEngine::ModelScope::PROJECT: Invalid value provided. All values provided must be strings.');

      return value;
    }).filter(Boolean);

    let context = this.getOperationContext();
    let projection = this.margeFields(
      context.projection,
      entities,
      {},
      { isProjection: true },
    );

    this._pushOperationOntoStack({
      control:   true,
      operator:  'PROJECT',
      queryProp: 'PROJECT',
      value:     entities,
      projection,
    });

    return this._fetchScope('model');
  }

  DISTINCT = ProxyClass.autoCall(function(fullyQualifiedName) {
    let currentQuery  = this;
    let distinctValue = fullyQualifiedName;

    if (arguments.length === 0) {
      let context = this.getOperationContext();
      let rootModel = context.rootModel;
      if (!rootModel)
        throw new Error(`${this.constructor.name}::DISTINCT: Attempted to apply DISTINCT to the root model of the query, but no root model was found.`);

      let pkFieldName = rootModel.getPrimaryKeyFieldName();
      if (!pkFieldName)
        throw new Error(`${this.constructor.name}::DISTINCT: Attempted to apply DISTINCT to the root model of the query, but the root model has no primary key. Try directly specifying the DISTINCT field instead.`);

      distinctValue = new DistinctLiteral(`${rootModel.getModelName()}:${pkFieldName}`);
      currentQuery = this.PROJECT(`-${rootModel.getModelName()}:${pkFieldName}`);
    } else if (fullyQualifiedName) {
      distinctValue = new DistinctLiteral(fullyQualifiedName);
      currentQuery = this.PROJECT(`-${fullyQualifiedName}`);
    }

    currentQuery._pushOperationOntoStack({ sqlFunc: true, operator: 'DISTINCT', queryProp: 'DISTINCT', value: distinctValue, distinct: distinctValue });
    return this._fetchScope('model');
  });

  INNER_JOIN = ProxyClass.autoCall(function() {
    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'INNER_JOIN', value: 'inner', joinType: 'inner', joinOuter: false });
    return this._fetchScope('model');
  });

  LEFT_JOIN = ProxyClass.autoCall(function(outerJoin) {
    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'LEFT_JOIN', value: 'left', joinType: 'left', joinOuter: !!outerJoin });
    return this._fetchScope('model');
  });

  RIGHT_JOIN = ProxyClass.autoCall(function(outerJoin) {
    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'RIGHT_JOIN', value: 'right', joinType: 'right', joinOuter: !!outerJoin });
    return this._fetchScope('model');
  });

  FULL_JOIN = ProxyClass.autoCall(function(outerJoin) {
    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'FULL_JOIN', value: 'full', joinType: 'full', joinOuter: !!outerJoin });
    return this._fetchScope('model');
  });

  CROSS_JOIN = ProxyClass.autoCall(function() {
    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'CROSS_JOIN', value: 'cross', joinType: 'cross', joinOuter: false });
    return this._fetchScope('model');
  });

  JOIN(type) {
    if (!(Nife.instanceOf(type, 'string') || LiteralBase.isLiteral(type)))
      throw new Error('QueryEngine::ModelScope::JOIN: Invalid value provided. Value must be a valid string or Literal specifying JOIN type.');

    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'JOIN', value: type, joinType: type, joinOuter: false });
    return this._fetchScope('model');
  }
}

module.exports = ModelScope;
