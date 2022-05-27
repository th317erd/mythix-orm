'use strict';

const Type = require('./type');

class IntegerType extends Type {
  static castToType({ value }) {
    if (value == null)
      return value;

    let number = parseFloat(('' + value).replace(/[^\d.e-]/g, ''));
    if (!isFinite(number))
      throw new TypeError(`IntegerType::castToType: Value provided ("${value}") can not be cast into an integer.`);

    return Math.round(number);
  }

  toString(connection) {
    return (connection) ? connection.typeToString(this) : 'INTEGER';
  }
}

module.exports = {
  INTEGER: Type.wrapConstructor(IntegerType),
  IntegerType,
};
