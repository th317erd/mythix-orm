'use strict';

const Type = require('../type');

class BooleanType extends Type {
  static castToType({ value }) {
    if (value == null)
      return value;

    if (value === 'true')
      return true;

    if (value === 'false')
      return false;

    return !!value;
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
