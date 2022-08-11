'use strict';

const Nife                  = require('nife');
const UUID                  = require('uuid');
const UUIDBaseType          = require('./uuid-base');
const { defaultValueFlags } = require('../helpers/default-helpers');

let CALLABLE_PROP_NAMES = [ 'random', 'buffer', 'offset' ];

class UUIDV4Type extends UUIDBaseType {
  static Default = {
    UUIDV4: defaultValueFlags(function(context) {
      let field     = context.field;
      let type      = field.type;
      let options   = type.getOptions();
      let uuidArgs  = type.getArgsForUUID(options);

      // Will throw an error if something is wrong
      type.validateOptions(uuidArgs);

      let prefix = options.prefix || '';
      return `${prefix}${UUID.v4(...uuidArgs)}`;
    }),
  };

  static getDisplayName() {
    return 'UUIDV4';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  getArgsForUUID(options) {
    let uuidOptions = {};

    for (let i = 0, il = CALLABLE_PROP_NAMES.length; i < il; i++) {
      let propName  = CALLABLE_PROP_NAMES[i];
      let value     = options[propName];

      if (typeof value === 'function')
        value = value(this);

      if (value !== undefined)
        uuidOptions[propName] = value;
    }

    let args = [];
    if (Nife.isNotEmpty(uuidOptions))
      args.push(uuidOptions);

    if (uuidOptions.buffer) {
      args.push(uuidOptions.buffer);
      args.push(uuidOptions.offset || 0);
    }

    return args;
  }

  validateOptions() {
    // No validation required for V4
  }

  castToType(args) {
    let { value } = args;
    if (value == null)
      return value;

    if (!this.isValidValue(value, args))
      throw new TypeError(`UUIDV4Type::castToType: Provided value "${value}" is not a valid UUID.`);

    return this.addPrefix(value);
  }

  isValidValue(value) {
    if (!Nife.instanceOf(value, 'string'))
      return false;

    let { id } = this.stripPrefix(value);
    if (!id)
      return false;

    return UUID.validate(id);
  }
}

module.exports = {
  UUIDV4: UUIDBaseType.wrapConstructor(UUIDV4Type),
  UUIDV4Type,
};
