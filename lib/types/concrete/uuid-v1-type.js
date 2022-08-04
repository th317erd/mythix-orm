'use strict';

const UUID                  = require('uuid');
const Type                  = require('../type');
const { defaultValueFlags } = require('../helpers/default-helpers');

class UUIDV1Type extends Type {
  static Default = {
    UUIDV1: function(options) {
      return defaultValueFlags(function(context) {
        if (!options)
          throw new Error('UUIDV1Type::Default::UUIDV1: "options" argument is required. Did you forget to call the default provider? The correct usage pattern is: "defaultValue: Types.UUIDV1.Default.UUIDV1(Object|Function)".');

        let opts = options;
        if (typeof opts === 'function')
          opts = opts.call(this, context);

        return UUID.v1(opts, opts.buffer, opts.offset);
      });
    },
  };

  static getDisplayName() {
    return 'UUIDV1';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  castToType({ value }) {
    if (value == null)
      return value;

    if (!this.isValidValue(value))
      throw new TypeError(`UUIDV1Type::castToType: Provided value "${value}" is not a valid UUID.`);

    return value;
  }

  isValidValue(value) {
    return UUID.validate(value);
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : 'VARCHAR(36)';
  }
}

module.exports = {
  UUIDV1: Type.wrapConstructor(UUIDV1Type),
  UUIDV1Type,
};
