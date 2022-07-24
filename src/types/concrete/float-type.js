'use strict';

const Nife  = require('nife');
const Type  = require('../type');

class FloatType extends Type {
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

  toConnectionType(connection) {
    switch (connection.dialect) {
      case 'sqlite':
        return 'REAL';
      default:
        return this.toString();
    }
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
