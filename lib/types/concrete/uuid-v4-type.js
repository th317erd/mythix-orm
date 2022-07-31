'use strict';

const UUID                  = require('uuid');
const Type                  = require('../type');
const { defaultValueFlags } = require('../helpers/default-helpers');

class UUIDV4Type extends Type {
  static Default = {
    UUIDV4: function(options) {
      // Are we fetching, or constructing?
      // If we are fetching, then just return a UUID
      if (options && options._fetchDefaultValue)
        return UUID.v4();

      return defaultValueFlags(function(context) {
        let opts = options;
        if (typeof opts === 'function')
          opts = opts.call(this, context);

        if (!opts)
          return UUID.v4();
        else
          return UUID.v4(opts, opts.buffer, opts.offset);
      });
    },
  };

  static getDisplayName() {
    return 'UUIDV4';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  castToType({ value }) {
    if (value == null)
      return value;

    if (!this.isValidValue(value))
      throw new TypeError(`UUIDV4Type::castToType: Provided value "${value}" is not a valid UUID.`);

    return value;
  }

  isValidValue(value) {
    return UUID.validate(value);
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
