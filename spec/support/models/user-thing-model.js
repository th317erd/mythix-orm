'use strict';

const { Model, Types } = require('../../../src');

class UserThing extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'userID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    'roleThingID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    'roleThing': {
      type:       Types.Model('RoleThing', 'roleThingID'),
    },
    'role': {
      type:       Types.Model('Role', 'roleThing.role'),
    },
    'user': {
      type:       Types.Model('User:id', 'userID'),
    },
  };
}

module.exports = UserThing;
