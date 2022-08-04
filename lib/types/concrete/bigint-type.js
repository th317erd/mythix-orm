'use strict';

const Nife                = require('nife');
const Type                = require('../type');
const { AUTO_INCREMENT }  = require('../helpers/default-helpers');

class BigIntType extends Type {
  static Default = {
    AUTO_INCREMENT,
  };

  static getDisplayName() {
    return 'BIGINT';
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

    return BigInt(value);
  }

  isValidValue(value) {
    return (Nife.instanceOf(value, 'number', 'bigint') && isFinite(value));
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : `BIGINT${(this.length != null) ? `(${this.length})` : ''}`;
  }
}

module.exports = {
  BIGINT: Type.wrapConstructor(BigIntType),
  BigIntType,
};
