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

  toString(connection) {
    return (connection) ? connection.typeToString(this) : 'BOOLEAN';
  }
}

module.exports = {
  BOOLEAN: Type.wrapConstructor(BooleanType),
  BooleanType,
};
