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

  constructor(format) {
    super(format);

    this.format = format;
  }

  castToType({ value }) {
    if (value == null)
      return value;

    let dateTime = this.deserialize(value);
    if (!dateTime.isValid())
      throw new TypeError(`DateType::castToType: Value provided ("${value}") can not be cast into a date.`);

    return dateTime.startOf('day');
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

  serialize(value, connection) {
    if (value == null)
      return (connection) ? null : value;

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
  DATE: Type.wrapConstructor(DateType),
  DateType,
};
