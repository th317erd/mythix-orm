'use strict';

const ProxyClass = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');

class FieldScope extends QueryEngineBase {
  NOT = ProxyClass.autoCall(function() {
    this._addToQuery({ operator: 'NOT', not: !this.currentContext.not });
    return this[ProxyClass.PROXY];
  });

  EQ(value) {
    this._addToQuery({ condition: true, operator: 'EQ', value: value });
    return this._fetchScope('model');
  }

  [ProxyClass.MISSING](target, prop) {
    let lowerScope = this._fetchScope('model');
    return lowerScope[prop];
  }
}

module.exports = FieldScope;
