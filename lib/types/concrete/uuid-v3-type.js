'use strict';

const UUID                  = require('uuid');
const Type                  = require('../type');
const { defaultValueFlags } = require('../helpers/default-helpers');

class UUIDV3Type extends Type {
  static Default = {
    UUIDV3: function(options) {
      return defaultValueFlags(function(context) {
        if (!options)
          throw new Error('UUIDV3Type::Default::UUIDV3: "options" argument is required. Did you forget to call the default provider? The correct usage pattern is: "defaultValue: Types.UUIDV3.Default.UUIDV3(Object|Function)".');

        let opts = options;
        if (typeof opts === 'function')
          opts = opts.call(this, context);

        return UUID.v3(opts.name, opts.namespace, opts.buffer, opts.offset);
      });
    },
  };

  static getDisplayName() {
    return 'UUIDV3';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  castToType({ value }) {
    if (value == null)
      return value;

    if (!this.isValidValue(value))
      throw new TypeError(`UUIDV3Type::castToType: Provided value "${value}" is not a valid UUID.`);

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
  UUIDV3: Type.wrapConstructor(UUIDV3Type),
  UUIDV3Type,
};
