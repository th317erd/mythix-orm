'use strict';

const { Model, Types } = require('../../../lib');

class ValidationTest extends Model {
  static fields = {
    'id': {
      type:         Types.XID,
      defaultValue: Types.XID.Default.XID,
      allowNull:    false,
      primaryKey:   true,
    },
    'number': {
      type:     Types.STRING,
      index:    true,
      validate: async (value) => {
        if (!(/^\d+$/).test(value))
          throw new Error('Number expected');
      },
    },
    'boolean': {
      type:     Types.STRING,
      index:    true,
      validate: async (value) => {
        if (!(/^(true|false)$/).test(value))
          throw new Error('Boolean expected');
      },
    },
    'date': {
      type:     Types.STRING,
      index:    true,
      validate: async (value) => {
        if (!(/^\d{4}-\d{2}-\d{2}$/).test(value))
          throw new Error('Invalid date');
      },
    },
  };
}

module.exports = ValidationTest;
