'use strict';

const Nife                = require('nife');
const Type                = require('../type');
const { AUTO_INCREMENT }  = require('../helpers/default-helpers');

class BigIntType extends Type {
  static Default = {
    AUTO_INCREMENT,
  };

  static getDisplayName() {
    return 'BIGINT';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  constructor(length, _options) {
    let options = _options || {};

    super(length, options);

    this.length = length || null;

    Object.defineProperties(this, {
      '_options': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        options,
      },
    });
  }

  castToType({ value }) {
    if (value == null)
      return value;

    let castValue = BigInt(value);
    if (this._options.strict === true)
      return castValue;

    return Number(castValue).valueOf();
  }

  isValidValue(value) {
    return (Nife.instanceOf(value, 'number', 'bigint') && isFinite(value));
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : `BIGINT${(this.length != null) ? `(${this.length})` : ''}`;
  }
}

module.exports = {
  BIGINT: Type.wrapConstructor(BigIntType),
  BigIntType,
};
