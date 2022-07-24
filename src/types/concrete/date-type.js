'use strict';

const Nife          = require('nife');
const moment        = require('moment');
const Type          = require('../type');
const { DATE_NOW }  = require('../helpers/default-helpers');

moment.suppressDeprecationWarnings = true;

class DateType extends Type {
  static Default = {
    NOW: DATE_NOW,
  };

  castToType({ value }) {
    if (value == null)
      return value;

    let dateTime = moment.utc(value);
    if (!dateTime.isValid())
      throw new TypeError(`DateType::castToType: Value provided ("${value}") can not be cast into a date.`);

    return dateTime.startOf('day').toDate();
  }

  isValidValue(value) {
    return moment(value).isValid();
  }

  toConnectionType(connection) {
    switch (connection.dialect) {
      case 'sqlite':
        return this.toString();
      default:
        return this.toString();
    }
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : 'DATE';
  }
}

module.exports = {
  DATE: Type.wrapConstructor(DateType),
  DateType,
};
