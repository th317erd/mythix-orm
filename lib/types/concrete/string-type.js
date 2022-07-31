'use strict';

const Nife  = require('nife');
const Type  = require('../type');

const DEFAULT_STRING_LENGTH = 256;

class StringType extends Type {
  static getDisplayName() {
    return 'STRING';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  constructor(length) {
    super(length);

    this.length = length || DEFAULT_STRING_LENGTH;
  }

  castToType({ value }) {
    if (value == null)
      return value;

    return ('' + value);
  }

  isValidValue(value) {
    return Nife.instanceOf(value, 'string');
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
