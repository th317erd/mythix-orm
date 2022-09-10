'use strict';

const Nife                  = require('nife');
const XID                   = require('xid-js');
const UUIDBaseType          = require('./uuid-base');
const { defaultValueFlags } = require('../helpers/default-helpers');

class XIDType extends UUIDBaseType {
  static Default = {
    XID: defaultValueFlags(function(context) {
      let field   = context.field;
      let type    = field.type;
      let prefix  = type.getPrefix();

      return `${prefix}${XID.next()}`;
    }),
  };

  static getDisplayName() {
    return 'XID';
  }

  castToType(args) {
    let { value } = args;
    if (value == null)
      return value;

    if (!this.isValidValue(value, args))
      throw new TypeError(`XIDType::castToType: Provided value "${value}" is not a valid XID.`);

    return this.addPrefix(value);
  }

  isValidValue(value) {
    if (!Nife.instanceOf(value, 'string'))
      return false;

    let { id } = this.stripPrefix(value);
    if (!id)
      return false;

    return (/^[0-9abcdefghjkmnpqrstvwxyz]{20}$/).test(id);
  }

  getBaseLength() {
    // eslint-disable-next-line no-magic-numbers
    return 20;
  }
}

module.exports = {
  XID: UUIDBaseType.wrapConstructor(XIDType),
  XIDType,
};
