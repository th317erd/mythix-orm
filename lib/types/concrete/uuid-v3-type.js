'use strict';

const Nife                  = require('nife');
const UUID                  = require('uuid');
const UUIDBaseType          = require('./uuid-base');
const { defaultValueFlags } = require('../helpers/default-helpers');

let CALLABLE_PROP_NAMES = [ 'name', 'namespace', 'buffer', 'offset' ];

class UUIDV3Type extends UUIDBaseType {
  static Default = {
    UUIDV3: defaultValueFlags(function(context) {
      let field     = context.field;
      let type      = field.type;
      let options   = type.getOptions();
      let uuidArgs  = type.getArgsForUUID(options);

      // Will throw an error if something is wrong
      type.validateOptions(uuidArgs);

      let prefix = options.prefix || '';
      return `${prefix}${UUID.v3(...uuidArgs)}`;
    }),
  };

  static getDisplayName() {
    return 'UUIDV3';
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
    if (Nife.isNotEmpty(uuidOptions.name))
      args.push(uuidOptions.name);

    if (Nife.isNotEmpty(uuidOptions.namespace))
      args.push(uuidOptions.namespace);

    if (uuidOptions.buffer) {
      if (args.length < 1)
        args.push(null);

      if (args.length < 2)
        args.push(null);

      args.push(uuidOptions.buffer);
      args.push(uuidOptions.offset || 0);
    }

    return args;
  }

  validateOptions(uuidArgs) {
    let [ name, namespace ] = uuidArgs;

    if (Nife.isEmpty(this.options))
      throw new Error('UUIDV3Type::Default::UUIDV3: "options" argument is required for the specified type. Try "type: Types.UUIDV3({ ...options })" for your type instead.');

    if (Nife.isEmpty(name))
      throw new Error('UUIDV3Type::Default::UUIDV3: "options.name" option is required for the specified type. Try "type: Types.UUIDV3({ name: Function | String | Array })" for your type instead.');

    if (Nife.isEmpty(namespace))
      throw new Error('UUIDV3Type::Default::UUIDV3: "options.namespace" option is required for the specified type. Try "type: Types.UUIDV3({ namespace: Function | String | Array[16] })" for your type instead.');
  }

  castToType(args) {
    let { value } = args;
    if (value == null)
      return value;

    if (!this.isValidValue(value, args))
      throw new TypeError(`UUIDV3Type::castToType: Provided value "${value}" is not a valid UUID.`);

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
  UUIDV3: UUIDBaseType.wrapConstructor(UUIDV3Type),
  UUIDV3Type,
};
