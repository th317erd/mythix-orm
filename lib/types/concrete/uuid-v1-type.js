'use strict';

const Nife                  = require('nife');
const UUID                  = require('uuid');
const UUIDBaseType          = require('./uuid-base');
const { defaultValueFlags } = require('../helpers/default-helpers');

let CALLABLE_PROP_NAMES = [ 'node', 'clockseq', 'msecs', 'nsecs', 'random', 'buffer', 'offset' ];

/// `UUID` (version 1) type.
///
/// This represents a string based UUID,
/// using UUID version 1. The underlying
/// database type is the same as <see>StringType</see>,
/// which is usually a "VARCHAR" type.
///
/// An optional "prefix" can be specified for this
/// type, which will allow you to prefix your ids.
///
/// This type will automatically decide its own "length"
/// based on the length of a UUIDV1, plus the length of
/// any prefix you provide.
///
/// See the [uuid](https://www.npmjs.com/package/uuid)
/// module to properly understand the options for this
/// field type.
///
/// Example:
///   class UUIDs extends Model {
///     static fields = {
///       uuid1: Types.UUIDV1({
///         prefix: 'USER_',
///         node: ...,
///         clockseq: ...,
///         msecs: ...,
///         nsecs: ...,
///         random: ...,
///         rng: ...,
///         buffer: ...,
///         offset: ...,
///       }),
///       uuid2: new Types.UUIDV1Type({
///         prefix: 'USER_',
///         ...,
///       }),
///       uuidWithDefault: {
///         type: Types.UUIDV1({ ... }),
///         defaultValue: Types.UUIDV1.Default.UUIDV1,
///       },
///     };
///   }
///
/// Properties:
///   Default: object = { UUIDV1 }
///     `UUIDV1` is a method that can be used as the `defaultValue` of a <see>Field</see>
///     to have this field auto-generate UUIDs for new models.
///
/// See: Type
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

  /// Get the "display" name for this type.
  ///
  /// This method is called from <see>Model.toString</see>
  /// when stringifying the model for representation.
  ///
  /// Note:
  ///   This is also an instance method that can be called from
  ///   an instance of the type.
  ///
  /// Return: string
  ///   Return the string value `'UUIDV1'`
  static getDisplayName() {
    return 'UUIDV1';
  }

  /// This is an internal method that is used by the
  /// type. It prepares the arguments needed for the
  /// [uuid](https://www.npmjs.com/package/uuid) module
  /// based on the options provided to the type when
  /// created. It will return arguments that can be
  /// properly passed to [uuid.v1](https://www.npmjs.com/package/uuid#uuidv1options-buffer-offset).
  ///
  /// Return: Array<any>
  ///   An array of arguments to pass to `uuid.v1`
  ///
  /// Arguments:
  ///   options: object
  ///     The "options" object provided to the type when
  ///     it was initially created.
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

  /// This is an internal method that is used by the
  /// type. It validates the arguments that will be passed
  /// to the [uuid.v1](https://www.npmjs.com/package/uuid#uuidv1options-buffer-offset) method.
  /// If incorrect options were provided to the type
  /// when the type was specified, then this will throw
  /// a validation exception.
  ///
  /// Note:
  ///   The provided "options" are not validated until
  ///   the first UUID is generated for the first time.
  ///
  /// Return: undefined
  ///
  /// Arguments:
  ///   uuidArgs: Array<any>
  ///     The arguments that will be passed to [uuid.v1](https://www.npmjs.com/package/uuid#uuidv1options-buffer-offset).
  ///     If an issue is detected than an exception will be thrown.
  validateOptions(uuidArgs) {
    let uuidOptions = uuidArgs[0];

    if (Nife.isEmpty(uuidOptions))
      throw new Error('UUIDV1Type::Default::UUIDV1: "options" argument is required for the specified type. Try "type: Types.UUIDV1({ ...options })" for your type instead.');

    if (uuidOptions.clockseq == null)
      throw new Error('UUIDV1Type::Default::UUIDV1: "options.clockseq" option is required for the specified type. Try "type: Types.UUIDV1({ clockseq: Function | Number })" for your type instead.');

    if (uuidOptions.rng == null && uuidOptions.random == null)
      throw new Error('UUIDV1Type::Default::UUIDV1: One of "options.rng" or "option.random" options are required for the specified type. Try "type: Types.UUIDV1({ rng: () => Array[16] })" or "type: Types.UUIDV1({ random: Function | Array[16] })" for your type instead.');
  }

  /// Cast provided value to underlying type.
  ///
  /// This will cast the incoming value to the
  /// underlying type of this field, a `UUIDV1`
  /// string primitive. A `null` or `undefined`
  /// value will simply be returned.
  ///
  /// The provided value will be given to [uuid.validate](https://www.npmjs.com/package/uuid#uuidvalidatestr)
  /// to validate that it is a correct UUIDV1. If the provided value
  /// is not a valid UUIDV1, then this method will throw an exception.
  ///
  /// If your UUID is prefixed with the `prefix`
  /// option, then that will be stripped off first
  /// before the provided value is handed off to
  /// [uuid.validate](https://www.npmjs.com/package/uuid#uuidvalidatestr)
  /// for validation. The prefix will be added back after validation.
  ///
  /// Note:
  ///   If a valid UUID is given without a prefix, then the
  ///   specified prefix (if any) will simply be added to
  ///   the returned result.
  ///
  /// See <see>Type.castToType</see> for a more
  /// detailed description.
  ///
  /// Return: string | null | undefined
  ///   Return the incoming `value`, cast to this
  ///   type. `null` and `undefined` are simply
  ///   returned without casting.
  ///
  /// Arguments:
  ///   context: <see name="CastToTypeContext">Type.castToType</see>
  castToType(args) {
    let { value } = args;
    if (value == null)
      return value;

    if (!this.isValidValue(value, args))
      throw new TypeError(`UUIDV1Type::castToType: Provided value "${value}" is not a valid UUID.`);

    return this.addPrefix(value);
  }

  /// Check if the provided value is valid.
  ///
  /// This will check if the provided value is
  /// a valid UUIDV1, ignoring any prefix.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check.
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
