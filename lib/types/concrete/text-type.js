'use strict';

const Nife  = require('nife');
const Type  = require('../type');

const DEFAULT_TEXT_LENGTH = 65565;

/// `TEXT` type.
///
/// This represents a "text" of any size for the
/// underlying database driver, usually a "TEXT" type.
///
/// If no "length" is specified when you create the type,
/// then a default length of `65565` is assumed.
///
/// In most databases, a "TEXT" type is used for storing
/// large text blobs. Most databases will store this type
/// outside of the engine in a separate file space, and
/// so this type can be a little slower than other types,
/// however the tradeoff is that you can store a large
/// amount of text.
///
/// Example:
///   class TextBlobs extends Model {
///     static fields = {
///       text1: Types.TEXT(65535),
///       text2: Types.TEXT, // default length = 65535
///       text3: new Types.TextType(65535),
///     };
///   }
///
/// See: Type
class TextType extends Type {
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
  ///   Return the string value `'TEXT'`
  static getDisplayName() {
    return 'TEXT';
  }

  /// Construct a new `TEXT` type.
  ///
  /// The `length` argument specifies
  /// the number of characters to use for
  /// this string in the database.
  ///
  /// Return: StringType
  ///
  /// Arguments:
  ///   length?: number
  ///     How many characters to use in the underlying database to store the value.
  constructor(length) {
    super(length);

    this.length = length || DEFAULT_TEXT_LENGTH;
  }

  /// Cast provided value to underlying type.
  ///
  /// This will cast the incoming value to the
  /// underlying type of this field, a `string`
  /// primitive. A `null` or `undefined`
  /// value will simply be returned.
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
  castToType({ value }) {
    if (value == null)
      return value;

    return ('' + value);
  }

  /// Check if the provided value is valid.
  ///
  /// This will check if the provided value is
  /// a `string` (or `String`) instance.
  /// Both an empty string, and a string that is nothing
  /// except whitespace are considered "valid".
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check.
  isValidValue(value) {
    return Nife.instanceOf(value, 'string');
  }

  /// Stringify the type itself.
  ///
  /// If a `connection` argument is provided, then this
  /// will go through the connection to generate the type
  /// for the underlying database. If no connection is
  /// provided, then a "standard" SQL type will be returned
  /// for this type instead. The "standard" type returned
  /// when no `connection` is provided is `'TEXT'`.
  ///
  /// Return: string
  ///
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection. If provided, send this
  ///     type through <see>Type.toConnectionType</see>
  ///     to have the connection itself generate the underlying
  ///     type for the database. If `connection` is not provided,
  ///     then this will simply return a "standard" generic matching
  ///     SQL type.
  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : 'TEXT';
  }
}

module.exports = {
  TEXT: Type.wrapConstructor(TextType),
  TextType,
};
