'use strict';

const Nife  = require('nife');
const Type  = require('../type');

const DEFAULT_STRING_LENGTH = 65565;

class TextType extends Type {
  static getDisplayName() {
    return 'TEXT';
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

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : 'TEXT';
  }
}

module.exports = {
  TEXT: Type.wrapConstructor(TextType),
  TextType,
};
