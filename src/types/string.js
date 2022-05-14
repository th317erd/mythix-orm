'use strict';

const Type = require('./type');

const DEFAULT_STRING_LENGTH = 256;

class StringType extends Type {
  constructor(length) {
    super(length);

    this.length = length || DEFAULT_STRING_LENGTH;
  }

  toString(connection) {
    return (connection) ? connection.typeToString(this) : `VARCHAR(${this.length})`;
  }
}

module.exports = {
  STRING: Type.wrapConstructor(StringType),
  StringType,
};
