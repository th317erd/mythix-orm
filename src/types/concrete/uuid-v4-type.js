'use strict';

const UUID                  = require('uuid');
const Type                  = require('../type');
const { defaultValueFlags } = require('../../helpers/default-helpers');

const UUIDV4 = defaultValueFlags(function() {
  return UUID.v4();
});

class UUIDV4Type extends Type {
  static Default = {
    UUIDV4: UUIDV4,
  };

  castToType({ value }) {
    if (value == null)
      return value;

    if (!UUID.validate(value))
      throw new TypeError(`UUIDV4Type::castToType: Provided value "${value}" is not a valid UUID.`);

    return value;
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
      : 'VARCHAR(36)';
  }
}

module.exports = {
  UUIDV4: Type.wrapConstructor(UUIDV4Type),
  UUIDV4Type,
};
