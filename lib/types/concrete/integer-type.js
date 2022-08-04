'use strict';

const Nife                = require('nife');
const Type                = require('../type');
const { AUTO_INCREMENT }  = require('../helpers/default-helpers');

class IntegerType extends Type {
  static Default = {
    AUTO_INCREMENT,
  };

  static getDisplayName() {
    return 'INTEGER';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  constructor(length) {
    super(length);

    this.length = length || null;
  }

  castToType({ value }) {
    if (value == null)
      return value;

    let number = parseFloat(('' + value).replace(/[^\d.e-]/g, ''));
    if (!isFinite(number))
      throw new TypeError(`IntegerType::castToType: Value provided ("${value}") can not be cast into an integer.`);

    return Math.round(number);
  }

  isValidValue(value) {
    return (Nife.instanceOf(value, 'number') && isFinite(value));
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : 'INTEGER';
  }
}

module.exports = {
  INTEGER: Type.wrapConstructor(IntegerType),
  IntegerType,
};
