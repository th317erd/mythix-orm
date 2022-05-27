'use strict';

const Type = require('./type');

class BigIntType extends Type {
  static castToType({ value }) {
    if (value == null)
      return value;

    return BigInt(value);
  }

  constructor(length) {
    super(length);

    this.length = length || null;
  }

  toString(connection) {
    return (connection)
      ? connection.typeToString(this)
      : `BIGINT${(this.length != null) ? `(${this.length})` : ''}`;
  }
}

module.exports = {
  BIGINT: Type.wrapConstructor(BigIntType),
  BigIntType,
};
