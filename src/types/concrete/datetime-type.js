'use strict';

const moment            = require('moment');
const Type              = require('../type');
const { DATETIME_NOW }  = require('../../helpers/default-helpers');

moment.suppressDeprecationWarnings = true;

class DateTimeType extends Type {
  static Default = {
    NOW: DATETIME_NOW,
  };

  static castToType({ value }) {
    if (value == null)
      return value;

    let dateTime = moment.utc(value);
    if (!dateTime.isValid())
      throw new TypeError(`DateTimeType::castToType: Value provided ("${value}") can not be cast into a date.`);

    return dateTime.toDate();
  }

  constructor(length) {
    super(length);

    this.length = length || null;
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
}

module.exports = {
  DATETIME:   Type.wrapConstructor(DateTimeType),
  DateTimeType,
};
