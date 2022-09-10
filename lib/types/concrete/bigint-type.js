'use strict';

const Nife                = require('nife');
const Type                = require('../type');
const { AUTO_INCREMENT }  = require('../helpers/default-helpers');

/// `BIGINT` type.
///
/// This represents a "big integer", or a 64+ bit integer
/// in any underlying database.
///
/// By default, `BIGINT` will use the `number` primitive
/// to store values in JavaScript client-side. This
/// choice was made because the `number` primitive is
/// more commonly dealt with, and its max storage
/// capacity is the number `9,007,199,254,740,991`, which
/// is generally far above and beyond what most databases
/// will ever reach. However, you can pass the
/// `{ strict: true }` argument to the constructor when
/// defining the type, and in this case it will instead
/// use JavaScript's [BigInt](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt)
/// type to store this value client-side.
///
/// Example:
///   class Numbers extends Model {
///     static fields = {
///       number1: Types.BIGINT(8, { strict: true }),
///       number2: new Types.BigIntType(8, { strict: true }),
///       autoIncrementing: {
///         type: Types.BIGINT,
///         defaultValue: Types.BIGINT.Default.AUTO_INCREMENT,
///       },
///     };
///   }
///
/// Note:
///   SQLite currently doesn't support 64bit auto-incrementing
///   IDs (except on the primary key itself). For this reason,
///   Mythix ORM supports emulation of BIGINT auto-incrementing
///   IDs in its [mythix-orm-sqlite](https://www.npmjs.com/package/mythix-orm-sqlite)
///   driver. You can read more about it in the documentation.
///
/// Properties:
///   Default: object = { AUTO_INCREMENT }
///     `AUTO_INCREMENT` is a method that can be used as the `defaultValue` of a <see>Field</see>
///     to have this field auto-increment in the underlying database.
///
/// See: Type
class BigIntType extends Type {
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
  ///
  /// Return: string
  ///   Return the string value `'BIGINT'`
  static getDisplayName() {
    return 'BIGINT';
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
  /// The "options" argument can be used to specify
  /// if this BIGINT type should be in "strict" mode.
  /// "strict" mode simply means that the client-side
  /// storage for the field's value (in JavaScript)
  /// will actually be a [BigInt](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt) type.
  /// By default, the underlying field value is stored
  /// as a `number` primitive.
  ///
  /// Return: BigIntType
  ///
  /// Arguments:
  ///   length?: number = 8
  ///     How many bytes to use in the underlying database to store the value.
  ///   options?: object
  ///     Only possible option is `{ strict: true }`, which will change the
  ///     underlying field to use [BigInt](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt)
  ///     for value storage.
  constructor(length, _options) {
    let options = _options || {};

    super(length, options);

    this.length = length || null;

    Object.defineProperties(this, {
      '_options': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        options,
      },
    });
  }

  /// Cast provided value to underlying type.
  ///
  /// This will cast the incoming value to the
  /// underlying type of this field, either a
  /// `number` primitive, or a `BigInt` if in
  /// "strict" mode. A `null` or `undefined`
  /// value will simply be returned.
  ///
  /// See <see>Type.castToType</see> for a more
  /// detailed description.
  ///
  /// Return: number | BigInt | null | undefined
  ///   Return the incoming `value`, cast to this
  ///   type. `null` and `undefined` are simply
  ///   returned without casting.
  ///
  /// Arguments:
  ///   context: <see name="CastToTypeContext">Type.castToType</see>
  castToType({ value }) {
    if (value == null)
      return value;

    let castValue = BigInt(value);
    if (this._options.strict === true)
      return castValue;

    return Number(castValue).valueOf();
  }

  /// Check if the provided value is valid.
  ///
  /// This will check if the provided value is
  /// a `number` or a BigInt instance, and if
  /// it is finite. If both conditions are true,
  /// then it will return `true`.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check.
  isValidValue(value) {
    return (Nife.instanceOf(value, 'number', 'bigint') && isFinite(value));
  }

  /// Stringify the type itself.
  ///
  /// If a `connection` argument is provided, then this
  /// will go through the connection to generate the type
  /// for the underlying database. If no connection is
  /// provided, then a "standard" SQL type will be returned
  /// for this type instead. The "standard" type returned
  /// when no `connection` is provided is `'BIGINT'` or
  /// `'BIGINT(${this.length})'` if a `length` is provided to
  /// the type.
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
      : `BIGINT${(this.length != null) ? `(${this.length})` : ''}`;
  }
}

module.exports = {
  BIGINT: Type.wrapConstructor(BigIntType),
  BigIntType,
};
