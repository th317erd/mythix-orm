'use strict';

const Nife  = require('nife');
const Type  = require('../type');

const DEFAULT_TOTAL_DIGITS    = 20;
const DEFAULT_DECIMAL_PLACES  = 6;

/// `NUMERIC` type.
/// Also known as `NUMBER`, or `DECIMAL` in some databases.
///
/// This represents a "real" number of high precision and
/// accuracy in the underlying database driver. Unlike the
/// <see>RealType</see>, which suffers from precision and
/// rounding errors, this will be precise to very large
/// and very small values in most databases.
///
/// The `precision`, and `scale` arguments are named after
/// PostgreSQL, and need some explaining, as their names do
/// not match their intent very well. `precision` is the total
/// number of digits a number can have. For example, the number
/// 123.456 has a `precision` of `6`, because it contains six
/// digits. The `scale` specifies the number of digits that
/// are desired *after* the decimal place. A scale of `0` would
/// essentially mean that you want an integer (no decimal places),
/// whereas a `scale` of `2` would mean you want two decimal places.
/// It is important to call out that different database drivers
/// may interpret and use these values differently.
///
/// Example:
///   class Numeric extends Model {
///     static fields = {
///       numeric1: Types.NUMERIC(20, 6),
///       numeric2: Types.NUMERIC,
///       numeric3: new Types.NumericType(10, 3),
///     };
///   }
///
/// See: Type
class NumericType extends Type {
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
  ///   Return the string value `'NUMERIC'`
  static getDisplayName() {
    return 'NUMERIC';
  }

  /// Construct a new `NUMERIC` type.
  ///
  /// The `precision` argument specifies the
  /// total number of digits (including decimal places)
  /// allowed for each number. The `scale` argument
  /// specifies the number of digits allowed after the decimal
  /// place. A `scale` of `0` is valid, meaning you want no
  /// digits after the decimal point.
  ///
  /// Return: NumericType
  ///
  /// Arguments:
  ///   precision?: number = 20
  ///     The total number of allowed digits in the number. This
  ///     includes decimal places. So a number of 123.456 would
  ///     need a `precision` of `6`, since it contains six total
  ///     digits.
  ///   scale?: number = 6
  ///     How many digits are allowed after the decimal point.
  ///     A value of `2` would allow two digits after the decimal
  ///     point. A value of `0` would allow no digits after the decimal
  ///     point, essentially turning this into an integer.
  constructor(precision, scale) {
    super(precision, scale);

    this.precision = precision || DEFAULT_TOTAL_DIGITS;
    this.scale = scale || DEFAULT_DECIMAL_PLACES;
  }

  /// Cast provided value to underlying type.
  ///
  /// This will cast the incoming value to the
  /// underlying type of this field, a `number`
  /// primitive. A `null` or `undefined`
  /// value will simply be returned.
  ///
  /// See <see>Type.castToType</see> for a more
  /// detailed description.
  ///
  /// Return: number | null | undefined
  ///   Return the incoming `value`, cast to this
  ///   type. `null` and `undefined` are simply
  ///   returned without casting.
  ///
  /// Arguments:
  ///   context: <see name="CastToTypeContext">Type.castToType</see>
  castToType({ value }) {
    if (value == null)
      return value;

    let number = parseFloat(('' + value));
    if (!isFinite(number))
      throw new TypeError(`NumericType::castToType: Value provided ("${value}") can not be cast into a floating point number.`);

    return number;
  }

  /// Check if the provided value is valid.
  ///
  /// This will check if the provided value is
  /// a `number` instance, and if it is finite.
  /// If both conditions are true, then it will
  /// return `true`.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check.
  isValidValue(value) {
    return (Nife.instanceOf(value, 'number') && isFinite(value));
  }

  /// Stringify the type itself.
  ///
  /// If a `connection` argument is provided, then this
  /// will go through the connection to generate the type
  /// for the underlying database. If no connection is
  /// provided, then a "standard" SQL type will be returned
  /// for this type instead. The "standard" type returned
  /// when no `connection` is provided is `'NUMERIC'`.
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
      : 'NUMERIC';
  }
}

module.exports = {
  NUMERIC: Type.wrapConstructor(NumericType),
  NumericType,
};
