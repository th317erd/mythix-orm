'use strict';

const { Model, Types } = require('../../../lib');

class Time extends Model {
  static fields = {
    'id': {
      type:         Types.XID,
      defaultValue: Types.XID.Default.XID,
      allowNull:    false,
      primaryKey:   true,
    },
    'datetime': {
      type:         Types.DATETIME,
      defaultValue: Types.DATETIME.Default.NOW,
      index:        true,
    },
    'datetimeLocal': {
      type:         Types.DATETIME,
      defaultValue: Types.DATETIME.Default.NOW.LOCAL,
      index:        true,
    },
    'date': {
      type:         Types.DATE,
      defaultValue: Types.DATE.Default.NOW,
      index:        true,
    },
    'dateLocal': {
      type:         Types.DATE,
      defaultValue: Types.DATE.Default.NOW.LOCAL,
      index:        true,
    },
  };
}

module.exports = Time;
