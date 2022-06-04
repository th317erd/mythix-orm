'use strict';

const { Model, Types } = require('../../../src');

class User extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'firstName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'lastName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'primaryRoleID': {
      type:       Types.UUIDV4,
      allowNull:  false,
    },
    'roles': {
      type:       Types.Models('UserRole:role', 'UserRole:user'),
    },
    'userThing': {
      type:       Types.Model('UserThing:user'),
    },
    'userThingRole': {
      type:       Types.Model('Role', 'userThing.role'),
    },
    'primaryRole': {
      type:       Types.Model('Role', 'primaryRoleID'),
    },
  };
}

module.exports = User;
