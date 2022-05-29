'use strict';

const Type                = require('../type');
const { AUTO_INCREMENT }  = require('../../helpers/default-helpers');

class BigIntType extends Type {
  static Default = {
    AUTO_INCREMENT,
  };

  static castToType({ value }) {
    if (value == null)
      return value;

    return BigInt(value);
  }

  constructor(length) {
    super(length);

    this.length = length || null;
  }

  toConnectionType(connection) {
    switch (connection.dialect) {
      case 'sqlite':
        return 'BIGINT';
      default:
        return this.toString();
    }
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
