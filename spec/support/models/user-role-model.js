'use strict';

const { Model, Types } = require('../../../src');

class UserRole extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'roleID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    'userID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    'role': {
      type:       Types.Model('Role:id', 'roleID'),
    },
    'user': {
      type:       Types.Model('User:id', 'userID'),
    },
  };
}

module.exports = UserRole;
