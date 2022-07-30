'use strict';

const XID                   = require('xid-js');
const Type                  = require('../type');
const { defaultValueFlags } = require('../helpers/default-helpers');

class XIDType extends Type {
  static Default = {
    XID: defaultValueFlags(function() {
      return XID.next();
    }),
  };

  castToType({ value }) {
    if (value == null)
      return value;

    if (!this.isValidValue(value))
      throw new TypeError(`XIDType::castToType: Provided value "${value}" is not a valid XID.`);

    return value;
  }

  isValidValue(value) {
    return (/^[0-9abcdefghjkmnpqrstvwxyz]{20}$/).test(value);
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
      : 'VARCHAR(20)';
  }
}

module.exports = {
  XID: Type.wrapConstructor(XIDType),
  XIDType,
};
