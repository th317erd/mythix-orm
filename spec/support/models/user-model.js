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
      allowNull:  true,
    },
    'roles': {
      type:       Types.Models('Role', 'userRoles.role'),
    },
    'userRoles': {
      type:       Types.Models('UserRole:userID'),
    },
    'userThing': {
      type:       Types.Model('UserThing:userID'),
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
