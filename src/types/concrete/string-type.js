'use strict';

const Type = require('../type');

const DEFAULT_STRING_LENGTH = 256;

class StringType extends Type {
  castToType({ value }) {
    if (value == null)
      return value;

    return ('' + value);
  }

  constructor(length) {
    super(length);

    this.length = length || DEFAULT_STRING_LENGTH;
  }

  toConnectionType(connection) {
    switch (connection.dialect) {
      case 'sqlite':
        return this.toString();
      default:
        return this.toString();
    }
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : `VARCHAR(${this.length})`;
  }
}

module.exports = {
  STRING: Type.wrapConstructor(StringType),
  StringType,
};
