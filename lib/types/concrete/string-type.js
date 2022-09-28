'use strict';

const Nife  = require('nife');
const Type  = require('../type');

const DEFAULT_STRING_LENGTH = 256;

/// `STRING` type.
///
/// This represents a "string" of any size for the
/// underlying database driver, usually a "VARCHAR" type.
///
/// If no "length" is specified when you create the type,
/// then a default length of `256` is assumed.
///
/// Example:
///   class Strings extends Model {
///     static fields = {
///       string1: Types.STRING(16),
///       string2: Types.STRING, // default length = 256
///       string3: new Types.StringType(64),
///     };
///   }
///
/// See: Type
class StringType extends Type {
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
  ///   Return the string value `'STRING'`
  static getDisplayName() {
    return 'STRING';
  }

  /// Construct a new `STRING` type.
  ///
  /// The `length` argument specifies
  /// the number of characters to use for this string
  /// in the database.
  ///
  /// Return: StringType
  ///
  /// Arguments:
  ///   length?: number
  ///     How many characters to use in the underlying database to store the value.
  constructor(length) {
    super(length);

    this.length = length || DEFAULT_STRING_LENGTH;
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
  /// when no `connection` is provided is `'VARCHAR'`.
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
      : `VARCHAR(${this.length})`;
  }
}

module.exports = {
  STRING: Type.wrapConstructor(StringType),
  StringType,
};
