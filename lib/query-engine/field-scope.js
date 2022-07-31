'use strict';

const Nife            = require('nife');
const ProxyClass      = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');

function addOperatorToQuery(name, inverseName, value) {
  this._addToQuery({
    condition:        true,
    operator:         name,
    inverseOperator:  inverseName,
    value:            this._fetchOperatorValue(value),
  });

  return this._fetchScope('model');
}

class FieldScope extends QueryEngineBase {
  NOT = ProxyClass.autoCall(function() {
    this._addToQuery({ logical: true, operator: 'NOT', not: !this.currentContext.not });
    return this[ProxyClass.PROXY];
  });

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

  _fetchOperatorValue(_value) {
    let value = _value;
    if (!value)
      return value;

    if (value.isModelClass) {
      // If a model class was supplied, then get the model query engine
      // and the primary key of the model

      let pkFieldName = value.getPrimaryKeyFieldName();
      if (Nife.isEmpty(pkFieldName))
        throw new Error(`${this.constructor.name}::_fetchOperatorValue: Invalid operation: You asked me to match against a model class, but model has no primary key, so I do not know what to match against.`);

      return value.where[pkFieldName];
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

  EQ(value) {
    return addOperatorToQuery.call(this, 'EQ', 'NEQ', value);
  }

  NEQ(value) {
    return addOperatorToQuery.call(this, 'NEQ', 'EQ', value);
  }

  GT(value) {
    return addOperatorToQuery.call(this, 'GT', 'LTE', value);
  }

  GTE(value) {
    return addOperatorToQuery.call(this, 'GTE', 'LT', value);
  }

  LT(value) {
    return addOperatorToQuery.call(this, 'LT', 'GTE', value);
  }

  LTE(value) {
    return addOperatorToQuery.call(this, 'LTE', 'GT', value);
  }

  [ProxyClass.MISSING](target, prop) {
    let lowerScope = this._fetchScope('model');
    return lowerScope[prop];
  }
}

module.exports = FieldScope;
