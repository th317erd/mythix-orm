'use strict';

const Nife            = require('nife');
const ProxyClass      = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');

function addOperatorToQuery(name, inverseName, value, extraOptions) {
  if (Nife.instanceOf(value, 'object'))
    throw new Error(`QueryEngine::addOperatorToQuery: ${name}(${value}) makes no sense...`);

  let conditionalParams = {
    condition:       true,
    operator:        name,
    queryProp:       name,
    queryExtraArgs:  (extraOptions) ? [ extraOptions ] : [],
    inverseOperator: inverseName,
    value:           this._fetchOperatorValue(value),
    hasCondition:    true,
  };

  if (extraOptions)
    conditionalParams = Object.assign(conditionalParams, extraOptions);

  this._pushOperationOntoStack(conditionalParams);

  return this._fetchScope('model');
}

function wrapAnyAll(func) {
  const checkQueryProjection = (_query, subType) => {
    let query         = _query;
    let queryContext  = (QueryEngineBase.isQuery(query)) ? query.getOperationContext() : null;
    if (!queryContext || !queryContext.hasCondition)
      throw new Error(`QueryEngine::FieldScope::${subType}: Provided value must be a query with conditions.`);

    if (!queryContext.projection) {
      let Model   = queryContext.Model;
      let pkField = Model.getPrimaryKeyField();

      if (pkField)
        query = query.clone().PROJECT(pkField);
      else
        throw new Error(`QueryEngine::FieldScope::${subType}: Provided query must have only a single field projected.`);
    }

    return query;
  };

  func.ANY = (_query) => {
    return func.call(this, checkQueryProjection(_query, 'ANY'), { subType: 'ANY' });
  };

  func.ALL = (_query) => {
    return func.call(this, checkQueryProjection(_query, 'ANY'), { subType: 'ALL' });
  };

  return func;
}

class FieldScope extends QueryEngineBase {
  NOT = ProxyClass.autoCall(function() {
    this._pushOperationOntoStack({ logical: true, operator: 'NOT', queryProp: 'NOT', not: !this.currentContext.not });
    return this[ProxyClass.PROXY];
  });

  unscoped() {
    return this.currentContext.queryEngineScope.unscoped(this.currentContext);
  }

  toString(...args) {
    return this.currentContext.queryEngineScope.toString(...args);
  }

  _fetchOperatorValue(_value) {
    let value = _value;
    if (!value)
      return value;

    if (value._isMythixModel) {
      // If a model class was supplied, then get the model query engine
      // and the primary key of the model

      let pkFieldName = value.getPrimaryKeyFieldName();
      if (Nife.isEmpty(pkFieldName))
        throw new Error(`${this.constructor.name}::_fetchOperatorValue: Invalid operation: You asked me to match against a model class, but model has no primary key, so I do not know what to match against.`);

      return value.where(this.getConnection())[pkFieldName];
    } else if (value._mythixModelInstance) {
      // If a model instance was supplied
      // then get the model's PK value
      let pkFieldName = value.getPrimaryKeyFieldName();
      if (Nife.isEmpty(pkFieldName))
        throw new Error(`${this.constructor.name}::_fetchOperatorValue: Invalid operation: You asked me to match against a model instance, but model has no primary key, so I do not know what to match against.`);

      return value[pkFieldName];
    }

    return value;
  }

  EQ = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'EQ', 'NEQ', value, options);
  });

  NEQ = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'NEQ', 'EQ', value, options);
  });

  GT = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'GT', 'LTE', value, options);
  });

  GTE = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'GTE', 'LT', value, options);
  });

  LT = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'LT', 'GTE', value, options);
  });

  LTE = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'LTE', 'GT', value, options);
  });

  LIKE(value, options) {
    let caseSensitive = ((options && options.caseSensitive) === true);
    return addOperatorToQuery.call(this, 'LIKE', 'NOT_LIKE', value, { caseSensitive });
  }

  NOT_LIKE(value, options) {
    let caseSensitive = ((options && options.caseSensitive) === true);
    return addOperatorToQuery.call(this, 'NOT_LIKE', 'LIKE', value, { caseSensitive });
  }

  [ProxyClass.MISSING](target, prop) {
    let lowerScope = this._fetchScope('model');
    return lowerScope[prop];
  }
}

module.exports = FieldScope;
