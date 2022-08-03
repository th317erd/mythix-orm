'use strict';

const Nife  = require('nife');
const Type  = require('../type');

class CharType extends Type {
  static getDisplayName() {
    return 'CHAR';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  castToType({ value }) {
    if (value == null)
      return value;

    if (!Nife.instanceOf('string') || value.length !== 1)
      throw new TypeError(`CharType::castToType: Value provided ("${value}") can not be cast into a char. Please provide a string that is one character wide.`);

    return value.charAt(0);
  }

  isValidValue(value) {
    return (Nife.instanceOf(value, 'string') && value.length === 1);
  }

  toConnectionType(connection) {
    // Note the deliberate lack of arguments
    // for "toString" so we don't infinitely recurse...
    switch (connection.dialect) {
      case 'sqlite':
        return 'CHAR';
      default:
        return this.toString();
    }
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : 'CHAR';
  }
}

module.exports = {
  CHAR: Type.wrapConstructor(CharType),
  CharType,
};
