'use strict';

const UUID                  = require('uuid');
const Type                  = require('../type');
const { defaultValueFlags } = require('../helpers/default-helpers');

class UUIDV5Type extends Type {
  static Default = {
    UUIDV5: function(options) {
      return defaultValueFlags(function(context) {
        if (!options)
          throw new Error('UUIDV5Type::Default::UUIDV5: "options" argument is required. Did you forget to call the default provider? The correct usage pattern is: "defaultValue: Types.UUIDV5.Default.UUIDV5(Object|Function)".');

        let opts = options;
        if (typeof opts === 'function')
          opts = opts.call(this, context);

        return UUID.v3(opts.name, opts.namespace, opts.buffer, opts.offset);
      });
    },
  };

  static getDisplayName() {
    return 'UUIDV5';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  castToType(args) {
    let { value } = args;
    if (value == null)
      return value;

    if (!this.isValidValue(value, args))
      throw new TypeError(`UUIDV5Type::castToType: Provided value "${value}" is not a valid UUID.`);

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
  UUIDV5: Type.wrapConstructor(UUIDV5Type),
  UUIDV5Type,
};
