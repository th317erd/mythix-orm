'use strict';

const Type = require('../type');

/// `BOOLEAN` type.
///
/// This represents a "boolean" type for the underlying database.
///
/// Example:
///   class Booleans extends Model {
///     static fields = {
///       boolean1: Types.BOOLEAN,
///       boolean2: new Types.BooleanType(),
///     };
///   }
///
/// See: Type
class BooleanType extends Type {
  /// Get the "display" name for this type.
  ///
  /// This method is called from <see>Model.toString</see>
  /// when stringifying the model for representation.
  ///
  /// Note:
  ///   This is also an instance method that can be called from
  ///   an instance of the type.
  /// Return: string
  ///   Return the string value `'BOOLEAN'`
  static getDisplayName() {
    return 'BOOLEAN';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  /// Cast provided value to underlying type.
  ///
  /// This will cast the incoming value to the
  /// underlying type of this field, a `boolean`
  /// primitive. It has some special edge-cases
  /// to be aware of. If a string value matching
  /// `'true'` or `'TRUE'` is given, then this
  /// will result in `true`. If a value matching
  /// `'false'` or `'FALSE'` is provided, then
  /// the result will be `false`. Otherwise,
  /// the result of `!!value` is returned.
  ///
  /// See <see>Type.castToType</see> for a more
  /// detailed description.
  ///
  /// Return: boolean | null | undefined
  ///   Return the incoming `value`, cast to this
  ///   type. `null` and `undefined` are simply
  ///   returned without casting.
  /// Arguments:
  ///   context: <see name="CastToTypeContext">Type.castToType</see>
  castToType({ value }) {
    if (value == null)
      return value;

    if (value === 'true' || value === 'TRUE')
      return true;

    if (value === 'false' || value === 'FALSE')
      return false;

    return !!value;
  }

  /// Check if the provided value is valid.
  ///
  /// This will check if the provided value is
  /// either `true` or `false` exactly, as a primitive.
  /// If `typeof value === 'boolean'` then this method
  /// will return `true`, otherwise it will return `false`.
  ///
  /// Return: boolean
  /// Arguments:
  ///   value: any
  ///     The value to check.
  isValidValue(value) {
    return (value === true || value === false);
  }

  /// Stringify the type itself.
  ///
  /// If a `connection` argument is provided, then this
  /// will go through the connection to generate the type
  /// for the underlying database. If no connection is
  /// provided, then a "standard" SQL type will be returned
  /// for this type instead. The "standard" type returned
  /// when no `connection` is provided is `'BOOLEAN'`.
  ///
  /// Return: string
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
      : 'BOOLEAN';
  }
}

module.exports = {
  BOOLEAN: Type.wrapConstructor(BooleanType),
  BooleanType,
};
