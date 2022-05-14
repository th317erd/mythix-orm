'use strict';

const Type = require('./type');

class IntegerType extends Type {
  toString(connection) {
    return (connection) ? connection.typeToString(this) : 'INTEGER';
  }
}

module.exports = {
  INTEGER: Type.wrapConstructor(IntegerType),
  IntegerType,
};
