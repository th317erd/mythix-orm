'use strict';

const Nife              = require('nife');
const moment            = require('moment');
const Type              = require('../type');
const { DATETIME_NOW }  = require('../helpers/default-helpers');

moment.suppressDeprecationWarnings = true;

/// `DATETIME` type.
///
/// This represents a "date time", a date with a time
/// in the underlying database.
///
/// Client-side storage for this field will be backed by
/// a [moment](https://momentjs.com/docs/) instance.
///
/// You can optionally provide a `format` argument when constructing
/// this type. **This is a client-side format only**. It doesn't
/// specify the format for storing the value in the database. Instead
/// it is used to parse provided date strings, and when serializing the
/// field (for example via <see>Model.toJSON</see>).
///
/// Example:
///   class DatesWithTimes extends Model {
///     static fields = {
///       dateTime1: Types.DATETIME('YYYY-MM-DD HH:mm:ss'),
///       dateTime2: new Types.DateTimeType(),
///     };
///   }
///
/// Properties:
///   Default: object = { NOW }
///     `NOW` is a method that can be used as the `defaultValue` of a <see>Field</see>
///     to have this field use "NOW" time. There are a number of different ways
///     to use this `defaultValue` method. If you use just `Types.DATETIME.Default.NOW`,
///     then it will use the underlying databases 'NOW' function, setting the field
///     to remote database time. Available also are the following:<br>
///     1. `Types.DATETIME.Default.NOW` = Remote database 'NOW' time, set on insert only.<br>
///     2. `Types.DATETIME.Default.NOW.UPDATE` = Remote database 'NOW' time, set on insert and update.<br>
///     3. `Types.DATETIME.Default.NOW.LOCAL` = Local (client-side) 'NOW' time, set on insert only.<br>
///     4. `Types.DATETIME.Default.NOW.LOCAL.UPDATE` = Local (client-side) 'NOW' time, set on insert and update.<br>
///     *Note: It is highly recommended that you **do not** use `LOCAL` time unless you know exactly what you are doing, as clock-drift between machines may become an issue.*
///
/// See: Type
class DateTimeType extends Type {
  static Default = {
    NOW: DATETIME_NOW,
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
  ///   Return the string value `'DATETIME'`
  static getDisplayName() {
    return 'DATETIME';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  /// Construct a new `DATETIME` type.
  ///
  /// The `format` argument will specify the
  /// date format for this field. See the
  /// [moment](https://momentjs.com/docs/#/displaying/format/) docs
  /// for a reference on this format.
  ///
  /// This format **is not** the format that will be used
  /// to store the `DATETIME` field in the database. Instead, this
  /// is a client-side only format, to define how to parse
  /// provided date strings, and to format on serialize
  /// when calling <see>Model.toJSON</see>.
  ///
  /// The `length` argument is used by some database drivers
  /// to modify the precision of the underlying timestamp/datetime value.
  ///
  /// Return: DateTimeType
  ///
  /// Arguments:
  ///   format?: string
  ///     Specify the client-side formatting for this date field.
  ///     This will be used for parsing date strings, and for serializing
  ///     the field.
  ///   length?: number
  ///     Used by some database drivers to specify the precision of the time
  ///     when stored in the database.
  constructor(format, length) {
    super(format, length);

    this.format = format;
    this.length = length || null;
  }

  /// Cast provided value to underlying type.
  ///
  /// This will cast the incoming value to the
  /// underlying type of this field, a [moment](https://momentjs.com/docs/)
  /// instance. If the provided value results in
  /// an invalid date, then an exception will be thrown.
  ///
  /// See <see>Type.castToType</see> for a more
  /// detailed description.
  ///
  /// Return: moment | null | undefined
  ///   Return the incoming `value`, cast to this
  ///   type. `null` and `undefined` are simply
  ///   returned without casting.
  ///
  /// Arguments:
  ///   context: <see name="CastToTypeContext">Type.castToType</see>
  castToType({ value, connection }) {
    if (value == null)
      return value;

    let dateTime = this.deserialize(value, connection);
    if (!dateTime.isValid())
      throw new TypeError(`DateTimeType::castToType: Value provided ("${value}") can not be cast into a date.`);

    return dateTime;
  }

  /// Check if the provided value is valid.
  ///
  /// This will check if the provided value is
  /// a valid date. It does so by calling
  /// `moment(value).isValid()`. If this check
  /// results in `true`, then this method will
  /// return `true`, otherwise it will return `false`.
  ///
  /// See the [moment](https://momentjs.com/docs/) library documentation for more information.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check.
  isValidValue(value) {
    return moment(value).isValid();
  }

  /// Stringify the type itself.
  ///
  /// If a `connection` argument is provided, then this
  /// will go through the connection to generate the type
  /// for the underlying database. If no connection is
  /// provided, then a "standard" SQL type will be returned
  /// for this type instead. The "standard" type returned
  /// when no `connection` is provided is `'DATETIME'`.
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
      : 'DATETIME';
  }

  /// Serialize the field value.
  ///
  /// This will be called whenever the field's value
  /// needs to be serialized. If a `connection` argument
  /// is provided, then this method will assume that the
  /// connection is serializing it for storage in the database.
  /// In this case, <see>connection.convertDateToDBTime</see> is
  /// called and provided the `value` for the connection to turn
  /// the date into a proper value for the underlying database.
  ///
  /// If no `connection` is provided, then the date will be serialized
  /// according to the provided `format` specified by the user. If
  /// no format was specified by the user, then it will be serialized
  /// to [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) format.
  ///
  /// Return: string | null | undefined
  ///
  /// Arguments:
  ///   value: moment
  ///     The date value to serialize.
  ///   connection?: <see>Connection</see>
  ///     A connection instance, which if provided, will
  ///     proxy serialization to the underlying database connection
  ///     via the <see>connection.convertDateToDBTime</see> method.
  serialize(_value, connection) {
    let value = _value;
    if (value == null)
      return (connection) ? null : value;

    if (!moment.isMoment(value))
      value = this.deserialize(value);

    if (connection)
      return connection.convertDateToDBTime(value);

    if (this.format)
      return value.format(this.format);

    return value.toISOString();
  }

  /// Deserialize the field value.
  ///
  /// This method is used to deserialize a provided
  /// field value. If the value provided is a `moment`
  /// instance, then simply return it. Otherwise,
  /// if a string is provided, convert it to a `moment`
  /// instance, using the `format` provided (if any).
  ///
  /// The `connection` argument is not used by this
  /// method, but is provided if the user wishes to
  /// overload this method.
  ///
  /// If `null` or `undefined` are provided as a value
  /// then that value will simply be returned as-is.
  ///
  /// Note:
  ///   This method is called by `castToType` to cast
  ///   incoming values into `moment` instances.
  ///
  /// Return: moment
  ///
  /// Arguments:
  ///   value: string | moment | null | undefined
  ///     The value to deserialize.
  ///   connection?: <see>Connection</see>
  ///     An optional connection that might be provided
  ///     depending on how and where this method is called from.
  ///     This method does nothing with the `connection`. It is
  ///     simply provided for if the user wishes to overload this
  ///     method.
  // eslint-disable-next-line no-unused-vars
  deserialize(_value, connection) {
    let value = _value;
    if (value == null)
      return value;

    if (Nife.instanceOf(value, 'string') && this.format) {
      value = moment(_value, this.format);

      if (!value.isValid())
        value = moment(_value);
    } else {
      value = moment(_value);
    }

    if (!value.isValid())
      throw new TypeError(`DateType::deserialize: Value provided ("${_value}") can not be cast into a date.`);

    return value;
  }
}

module.exports = {
  DATETIME:   Type.wrapConstructor(DateTimeType),
  DateTimeType,
};
