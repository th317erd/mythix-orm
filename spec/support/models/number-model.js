'use strict';

const { Model, Types } = require('../../../lib');

class Number extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'numberInt': {
      type:         Types.INTEGER,
      index:        true,
    },
    'numberFloat': {
      type:         Types.FLOAT,
      index:        true,
    },
  };
}

module.exports = Number;
