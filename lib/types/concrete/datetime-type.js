'use strict';

const Nife              = require('nife');
const moment            = require('moment');
const Type              = require('../type');
const { DATETIME_NOW }  = require('../helpers/default-helpers');

moment.suppressDeprecationWarnings = true;

class DateTimeType extends Type {
  static Default = {
    NOW: DATETIME_NOW,
  };

  static getDisplayName() {
    return 'DATETIME';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  constructor(length, format) {
    super(length, format);

    this.length = length || null;
    this.format = format;
  }

  castToType({ value }) {
    if (value == null)
      return value;

    let dateTime = this.deserialize(value);
    if (!dateTime.isValid())
      throw new TypeError(`DateTimeType::castToType: Value provided ("${value}") can not be cast into a date.`);

    return dateTime;
  }

  isValidValue(value) {
    return moment(value).isValid();
  }

  toConnectionType(connection) {
    switch (connection.dialect) {
      case 'sqlite':
        return this.toString();
      case 'mysql':
        if (this.length)
          return `DATETIME(${this.length})`;
        else
          return 'DATETIME';
      default:
        return this.toString();
    }
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : 'DATETIME';
  }

  serialize(_value, connection) {
    let value = _value;
    if (value == null)
      return (connection) ? null : value;

    if (!(value instanceof moment))
      value = this.deserialize(value);

    if (connection)
      return value.toDate();

    if (this.format)
      return value.format(this.format);

    return value.toISOString();
  }

  deserialize(_value) {
    let value = _value;
    if (value == null)
      return value;

    if (Nife.instanceOf(value, 'string') && this.format) {
      value = moment.utc(_value, this.format);

      if (!value.isValid())
        value = moment.utc(_value);
    } else {
      value = moment.utc(_value);
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
