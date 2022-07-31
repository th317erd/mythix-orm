'use strict';

const Nife  = require('nife');
const Type  = require('../type');

class BooleanType extends Type {
  static getDisplayName() {
    return 'BOOLEAN';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  castToType({ value }) {
    if (value == null)
      return value;

    if (value === 'true')
      return true;

    if (value === 'false')
      return false;

    return !!value;
  }

  isValidValue(value) {
    return (value === true || value === false);
  }

  toConnectionType(connection) {
    switch (connection.dialect) {
      case 'sqlite':
        return 'BOOLEAN';
      default:
        return this.toString();
    }
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : 'BOOLEAN';
  }
}

module.exports = {
  BOOLEAN: Type.wrapConstructor(BooleanType),
  BooleanType,
};
