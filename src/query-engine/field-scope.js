'use strict';

const ProxyClass = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');

function addOperatorToQuery(name, value) {
  this._addToQuery({ condition: true, operator: name, value: value });
  return this._fetchScope('model');
}

class FieldScope extends QueryEngineBase {
  NOT = ProxyClass.autoCall(function() {
    this._addToQuery({ operator: 'NOT', not: !this.currentContext.not });
    return this[ProxyClass.PROXY];
  });

  unscoped() {
    return this.currentContext.queryEngineScope.unscoped(this.currentContext);
  }

  EQ(value) {
    return addOperatorToQuery.call(this, 'EQ', value);
  }

  NEQ(value) {
    return addOperatorToQuery.call(this, 'NEQ', value);
  }

  GT(value) {
    return addOperatorToQuery.call(this, 'GT', value);
  }

  GTE(value) {
    return addOperatorToQuery.call(this, 'GTE', value);
  }

  LT(value) {
    return addOperatorToQuery.call(this, 'LT', value);
  }

  LTE(value) {
    return addOperatorToQuery.call(this, 'LTE', value);
  }

  [ProxyClass.MISSING](target, prop) {
    let lowerScope = this._fetchScope('model');
    return lowerScope[prop];
  }
}

module.exports = FieldScope;
