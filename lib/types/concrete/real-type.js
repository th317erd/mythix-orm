'use strict';

const Nife  = require('nife');
const Type  = require('../type');

class RealType extends Type {
  static getDisplayName() {
    return 'REAL';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  constructor(length, scale) {
    super(length, scale);

    this.length = length;
    this.scale = scale;
  }

  castToType({ value }) {
    if (value == null)
      return value;

    let number = parseFloat(('' + value));
    if (!isFinite(number))
      throw new TypeError(`RealType::castToType: Value provided ("${value}") can not be cast into an floating point number.`);

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
  REAL: Type.wrapConstructor(RealType),
  RealType,
};
