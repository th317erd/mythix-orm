'use strict';

const Nife  = require('nife');
const Type  = require('../type');

/// `CHAR` type.
///
/// This represents a "char" type for the underlying database,
/// which is a single character.
///
/// Example:
///   class Booleans extends Model {
///     static fields = {
///       char1: Types.CHAR,
///       char2: new Types.CharType(),
///     };
///   }
///
/// See: Type
class CharType extends Type {
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
  ///   Return the string value `'CHAR'`
  static getDisplayName() {
    return 'CHAR';
  }

  /// Cast provided value to underlying type.
  ///
  /// This will cast the incoming value to the
  /// underlying type of this field, a `string`
  /// primitive with a length of one character.
  /// If the provided value is not a `string` or
  /// a `String`, or if the string provided has
  /// a character length greater than one, then
  /// an exception will be thrown.
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

    if (!Nife.instanceOf('string') || value.length !== 1)
      throw new TypeError(`CharType::castToType: Value provided ("${value}") can not be cast into a char. Please provide a string that is one character wide.`);

    return value.charAt(0);
  }

  /// Check if the provided value is valid.
  ///
  /// This will check if the provided value is
  /// a `string` or a `String` with a length
  /// of one character. If it is, then this will
  /// return `true`, otherwise it will return `false`.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check.
  isValidValue(value) {
    return (Nife.instanceOf(value, 'string') && value.length === 1);
  }

  /// Stringify the type itself.
  ///
  /// If a `connection` argument is provided, then this
  /// will go through the connection to generate the type
  /// for the underlying database. If no connection is
  /// provided, then a "standard" SQL type will be returned
  /// for this type instead. The "standard" type returned
  /// when no `connection` is provided is `'CHAR'`.
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
      : 'CHAR';
  }
}

module.exports = {
  CHAR: Type.wrapConstructor(CharType),
  CharType,
};
