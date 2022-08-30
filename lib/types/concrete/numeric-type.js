'use strict';

const Nife  = require('nife');
const Type  = require('../type');

const DEFAULT_TOTAL_DIGITS    = 20;
const DEFAULT_DECIMAL_PLACES  = 6;

class NumericType extends Type {
  static getDisplayName() {
    return 'NUMERIC';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  constructor(precision, scale) {
    super(precision, scale);

    this.precision = precision || DEFAULT_TOTAL_DIGITS;
    this.scale = scale || DEFAULT_DECIMAL_PLACES;
  }

  castToType({ value }) {
    if (value == null)
      return value;

    let number = parseFloat(('' + value));
    if (!isFinite(number))
      throw new TypeError(`FloatType::castToType: Value provided ("${value}") can not be cast into an floating point number.`);

    return number;
  }

  isValidValue(value) {
    return (Nife.instanceOf(value, 'number') && isFinite(value));
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : 'NUMERIC';
  }
}

module.exports = {
  NUMERIC: Type.wrapConstructor(NumericType),
  NumericType,
};
