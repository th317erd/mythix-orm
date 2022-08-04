'use strict';

const Nife  = require('nife');
const Type  = require('../type');

class FloatType extends Type {
  static getDisplayName() {
    return 'FLOAT';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  constructor(length, precision) {
    super(length, precision);

    this.length = length;
    this.precision = precision;
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
      : 'FLOAT';
  }
}

module.exports = {
  FLOAT: Type.wrapConstructor(FloatType),
  FloatType,
};
