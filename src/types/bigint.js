'use strict';

const Type = require('./type');

class BigIntType extends Type {
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
