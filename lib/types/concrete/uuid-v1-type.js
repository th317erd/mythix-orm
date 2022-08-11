'use strict';

const Nife                  = require('nife');
const UUID                  = require('uuid');
const UUIDBaseType          = require('./uuid-base');
const { defaultValueFlags } = require('../helpers/default-helpers');

let CALLABLE_PROP_NAMES = [ 'node', 'clockseq', 'msecs', 'nsecs', 'random', 'buffer', 'offset' ];

class UUIDV1Type extends UUIDBaseType {
  static Default = {
    UUIDV1: defaultValueFlags(function(context) {
      let field     = context.field;
      let type      = field.type;
      let options   = type.getOptions();
      let uuidArgs  = type.getArgsForUUID(options);

      // Will throw an error if something is wrong
      type.validateOptions(uuidArgs);

      let prefix = options.prefix || '';
      return `${prefix}${UUID.v1(...uuidArgs)}`;
    }),
  };

  static getDisplayName() {
    return 'UUIDV1';
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

  validateOptions(uuidArgs) {
    let uuidOptions = uuidArgs[0];

    if (Nife.isEmpty(uuidOptions))
      throw new Error('UUIDV1Type::Default::UUIDV1: "options" argument is required for the specified type. Try "type: Types.UUIDV1({ ...options })" for your type instead.');

    if (uuidOptions.clockseq == null)
      throw new Error('UUIDV1Type::Default::UUIDV1: "options.clockseq" option is required for the specified type. Try "type: Types.UUIDV1({ clockseq: Function | Number })" for your type instead.');

    if (uuidOptions.rng == null && uuidOptions.random == null)
      throw new Error('UUIDV1Type::Default::UUIDV1: One of "options.rng" or "option.random" options are required for the specified type. Try "type: Types.UUIDV1({ rng: () => Array[16] })" or "type: Types.UUIDV1({ random: Function | Array[16] })" for your type instead.');
  }

  castToType(args) {
    let { value } = args;
    if (value == null)
      return value;

    if (!this.isValidValue(value, args))
      throw new TypeError(`UUIDV1Type::castToType: Provided value "${value}" is not a valid UUID.`);

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
  UUIDV1: UUIDBaseType.wrapConstructor(UUIDV1Type),
  UUIDV1Type,
};
