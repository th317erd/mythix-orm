'use strict';

const UUID                  = require('uuid');
const Type                  = require('../type');
const { defaultValueFlags } = require('../../helpers/default-helpers');

class UUIDV1Type extends Type {
  static Default = {
    UUIDV1: defaultValueFlags(function() {
      return UUID.v1();
    }),
  };

  static castToType({ value }) {
    if (value == null)
      return value;

    if (!UUID.validate(value))
      throw new TypeError(`UUIDV1Type::castToType: Provided value "${value}" is not a valid UUID.`);

    return value;
  }

  toString(connection) {
    return (connection) ? connection.typeToString(this) : 'VARCHAR(36)';
  }
}

module.exports = {
  UUIDV1: Type.wrapConstructor(UUIDV1Type),
  UUIDV1Type,
};
