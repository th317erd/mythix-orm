'use strict';

/* global Buffer */

const Type = require('../type');

/// `BLOB` type.
///
/// This represents a "blob", or random binary data.
///
/// BlobType expects that the value you provide to the
/// field will be a `Buffer` instance. If it isn't it
/// will throw an error when `castToType` is called.
/// `null` and `undefined` are also acceptable values
/// for a BlobType field.
///
/// Example:
///   class Blobs extends Model {
///     static fields = {
///       blob1: Types.BLOB(1024),
///       blob2: new Types.BlobType(1024),
///     };
///   }
///
/// See: Type
class BlobType extends Type {
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
  ///   Return the string value `'BLOB'`
  static getDisplayName() {
    return 'BLOB';
  }

  /// Construct a new `BLOB` type.
  ///
  /// The `length` argument is the number
  /// of bytes to use in the database to
  /// store the data.
  ///
  /// Return: BlobType
  ///
  /// Arguments:
  ///   length?: number
  ///     How many bytes to use in the underlying database to store the value.
  constructor(length) {
    super(length);

    this.length = length || null;
  }

  /// Cast provided value to underlying type.
  ///
  /// Unlike most type casters, this doesn't
  /// do any casting. You must provide either
  /// a `Buffer`, `null`, or `undefined` as
  /// the value to your blob field. If any
  /// other type of value is provided then
  /// an exception will be thrown.
  ///
  /// See <see>Type.castToType</see> for a more
  /// detailed description.
  ///
  /// Return: Buffer | null | undefined
  ///   Return the incoming `value`, cast to this
  ///   type. `null` and `undefined` are simply
  ///   returned without casting.
  ///
  /// Arguments:
  ///   context: <see name="CastToTypeContext">Type.castToType</see>
  castToType({ value }) {
    if (value == null)
      return value;

    if (!Buffer.isBuffer(value))
      throw new TypeError(`BlobType::castToType: Value provided ("${value}") can not be cast into a Buffer. Please provide a Buffer directly.`);

    return value;
  }

  /// Check if the provided value is valid.
  ///
  /// This will check if the provided value is
  /// a `Buffer`. If it is, it will return `true`,
  /// otherwise it will return `false`.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check.
  isValidValue(value) {
    return Buffer.isBuffer(value);
  }

  /// Stringify the type itself.
  ///
  /// If a `connection` argument is provided, then this
  /// will go through the connection to generate the type
  /// for the underlying database. If no connection is
  /// provided, then a "standard" SQL type will be returned
  /// for this type instead. The "standard" type returned
  /// when no `connection` is provided is `'BLOB'`.
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
      : 'BLOB';
  }
}

module.exports = {
  BLOB: Type.wrapConstructor(BlobType),
  BlobType,
};
