'use strict';

/* global Buffer */

const Type = require('../type');

class BlobType extends Type {
  static getDisplayName() {
    return 'BLOB';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  constructor(length) {
    super(length);

    this.length = length || null;
  }

  castToType({ value }) {
    if (value == null)
      return value;

    if (!Buffer.isBuffer(value))
      throw new TypeError(`BlobType::castToType: Value provided ("${value}") can not be cast into a Buffer. Please provide a Buffer directly.`);

    return value;
  }

  isValidValue(value) {
    return Buffer.isBuffer(value);
  }

  toConnectionType(connection) {
    switch (connection.dialect) {
      case 'sqlite':
        return 'BLOB';
      default:
        return this.toString();
    }
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : 'BLOB';
  }
}

module.exports = {
  BLOB: Type.wrapConstructor(BlobType),
  BlobType,
};
