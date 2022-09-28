'use strict';

const Nife                  = require('nife');
const XID                   = require('xid-js');
const UUIDBaseType          = require('./uuid-base');
const { defaultValueFlags } = require('../helpers/default-helpers');

/// `XID` type.
///
/// This represents a string based XID. The underlying
/// database type is the same as <see>StringType</see>,
/// which is usually a "VARCHAR" type.
///
/// An optional "prefix" can be specified for this
/// type, which will allow you to prefix your ids.
///
/// This type will automatically decide its own "length"
/// based on the length of an XID, plus the length of
/// any prefix you provide.
///
/// This type uses the [xid-js](https://www.npmjs.com/package/xid-js)
/// module to generate XIDs.
///
/// Example:
///   class XIDs extends Model {
///     static fields = {
///       xid1: Types.XID({
///         prefix: 'USER_',
///       }),
///       xid2: new Types.XIDType({
///         prefix: 'USER_',
///       }),
///       xidWithDefault: {
///         type: Types.XID({ ... }),
///         defaultValue: Types.XID.Default.XID,
///       },
///     };
///   }
///
/// Properties:
///   Default: object = { XID }
///     `XID` is a method that can be used as the `defaultValue` of a <see>Field</see>
///     to have this field auto-generate XIDs for new models.
///
/// See: Type
class XIDType extends UUIDBaseType {
  static Default = {
    XID: defaultValueFlags(function(context) {
      let field   = context.field;
      let type    = field.type;
      let prefix  = type.getPrefix();

      return `${prefix}${XID.next()}`;
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
  ///   Return the string value `'XID'`
  static getDisplayName() {
    return 'XID';
  }

  /// Cast provided value to underlying type.
  ///
  /// This will cast the incoming value to the
  /// underlying type of this field, a `XID`
  /// string primitive. A `null` or `undefined`
  /// value will simply be returned.
  ///
  /// The provided value will be validated to ensure
  /// it is a valid base32 XID value.
  ///
  /// If your XID is prefixed with the `prefix`
  /// option, then that will be stripped off first
  /// before the provided value is validated.
  /// The prefix will be added back after validation.
  ///
  /// Note:
  ///   If a valid XID is given without a prefix, then the
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
      throw new TypeError(`XIDType::castToType: Provided value "${value}" is not a valid XID.`);

    return this.addPrefix(value);
  }

  /// Check if the provided value is valid.
  ///
  /// This will check if the provided value is
  /// a valid XID, ignoring any prefix.
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
