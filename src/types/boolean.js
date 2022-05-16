'use strict';

const Type = require('./type');

class BooleanType extends Type {
  static castToType(value) {
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
