'use strict';

const Nife  = require('nife');
const Type  = require('../type');

/// `REAL` type.
/// Also known as `FLOAT`, or `DOUBLE` in some databases.
///
/// This represents a "real" number of low precision and
/// accuracy in the underlying database driver. Unlike the
/// <see>NumericType</see>, which is high precision, this
/// type can suffer from rounding errors, and won't be as
/// precise <see>NumericType</see>.
///
/// The `length` argument is the total number of bytes needed
/// to store the underlying value. If `4`, then this will generally
/// be a `FLOAT` or equivalent type in the underlying database driver.
/// If `8`, then it will generally be a `DOUBLE` or equivalent type.
/// The `scale` argument specifies the number of digits that
/// are desired *after* the decimal place. A scale of `0` would
/// essentially mean that you want an integer (no decimal places),
/// whereas a `scale` of `2` would mean you want two decimal places.
/// It is important to call out that different database drivers
/// may interpret and use these values differently.
///
/// Example:
///   class Real extends Model {
///     static fields = {
///       real1: Types.REAL(4, 6),  // FLOAT
///       real2: Types.REAL(8, 6),  // DOUBLE
///       real3: Types.REAL,        // FLOAT (default)
///     };
///   }
///
/// See: Type
class RealType extends Type {
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
  ///   Return the string value `'REAL'`
  static getDisplayName() {
    return 'REAL';
  }

  /// Construct a new `REAL` type.
  ///
  /// The `length` argument specifies the total number of
  /// bytes needed to store this value (default = `4`).
  /// The `scale` argument specifies the number of digits
  /// desired after the decimal place. A `scale` of `0` is valid,
  /// meaning you want no digits after the decimal point.
  /// Note that this is dependant on database driver support.
  ///
  /// Return: RealType
  ///
  /// Arguments:
  ///   length?: number = 4
  ///     The number of bytes needed to store the value. Use `4`
  ///     to end up with a `FLOAT` on most database drivers, or
  ///     `8` for a `DOUBLE`.
  ///   scale?: number = 6
  ///     How many digits are desired after the decimal point.
  ///     A value of `2` would allow two digits after the decimal
  ///     point. A value of `0` would allow no digits after the decimal
  ///     point, essentially turning this into an integer. This
  ///     depends on the underlying database to support it.
  constructor(length, scale) {
    super(length, scale);

    this.length = length;
    this.scale = scale;
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
      throw new TypeError(`RealType::castToType: Value provided ("${value}") can not be cast into an floating point number.`);

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
  /// when no `connection` is provided is `'FLOAT'`.
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
      : 'FLOAT';
  }
}

module.exports = {
  REAL: Type.wrapConstructor(RealType),
  RealType,
};
