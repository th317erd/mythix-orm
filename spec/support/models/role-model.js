'use strict';

const { Model, Types } = require('../../../src');

class Role extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'name': {
      type:       Types.STRING(64),
      allowNull:  false,
      index:      true,
    },
    'user': {
      type:       Types.Model('UserRole:user', 'UserRole:role'),
    },
  };
}

module.exports = Role;
