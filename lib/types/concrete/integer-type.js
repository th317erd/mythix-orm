'use strict';

const Nife                = require('nife');
const Type                = require('../type');
const { AUTO_INCREMENT }  = require('../helpers/default-helpers');

/// `INTEGER` type.
///
/// This represents an "integer" of any size (less than 64bits)
/// for the underlying database driver.
///
/// The `INTEGER` type is used for specifying all integer types.
/// Its `length` argument is used to specify the number of bytes
/// used in the backing storage of the field value. Each database
/// driver will interpret this `length` differently. For example,
/// a MySQL driver may choose to use a `TINYINT`, `SMALLINT`, a
/// `MEDIUMINT` based on the number of bytes specified.
///
/// Example:
///   class Integers extends Model {
///     static fields = {
///       integer1: Types.INTEGER(4),
///       integer2: Types.INTEGER,
///       integer3: new Types.BigIntType(1),
///       autoIncrementing: {
///         type: Types.INTEGER,
///         defaultValue: Types.INTEGER.Default.AUTO_INCREMENT,
///       },
///     };
///   }
///
/// Properties:
///   Default: object = { AUTO_INCREMENT }
///     `AUTO_INCREMENT` is a method that can be used as the `defaultValue` of a <see>Field</see>
///     to have this field auto-increment in the underlying database.
/// See: Type
class IntegerType extends Type {
  static Default = {
    AUTO_INCREMENT,
  };

  /// Get the "display" name for this type.
  ///
  /// This method is called from <see>Model.toString</see>
  /// when stringifying the model for representation.
  ///
  /// Note:
  ///   This is also an instance method that can be called from
  ///   an instance of the type.
  /// Return: string
  ///   Return the string value `'INTEGER'`
  static getDisplayName() {
    return 'INTEGER';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  /// Construct a new `BIGINT` type.
  ///
  /// The `length` argument--as on all
  /// integer types in Mythix ORM--specifies
  /// the number of bytes to use for this type
  /// in the database. Each underlying database
  /// driver will interpret this value in a way that
  /// makes sense to the database.
  ///
  /// Return: IntegerType
  /// Arguments:
  ///   length?: number
  ///     How many bytes to use in the underlying database to store the value.
  ///     Depending on the database driver, a length of `0` may be interpreted
  ///     as a single bit.
  constructor(length) {
    super(length);

    this.length = length || null;
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
  /// Arguments:
  ///   context: <see name="CastToTypeContext">Type.castToType</see>
  castToType({ value }) {
    if (value == null)
      return value;

    let number = parseFloat(('' + value).replace(/[^\d.e-]/g, ''));
    if (!isFinite(number))
      throw new TypeError(`IntegerType::castToType: Value provided ("${value}") can not be cast into an integer.`);

    return Math.round(number);
  }

  /// Check if the provided value is valid.
  ///
  /// This will check if the provided value is
  /// a `number` instance, and if it is finite.
  /// If both conditions are true, then it will
  /// return `true`.
  ///
  /// Return: boolean
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
  /// when no `connection` is provided is `'INTEGER'`.
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
      : 'INTEGER';
  }
}

module.exports = {
  INTEGER: Type.wrapConstructor(IntegerType),
  IntegerType,
};
