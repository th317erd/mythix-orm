'use strict';

const { Model, Types } = require('../../../src');

class RoleThing extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'roleID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    'userThing': {
      type:       Types.Model('UserThing', 'UserThing:roleThing'),
    },
    'role': {
      type:       Types.Model('Role:id', 'roleID'),
    },
    'user': {
      type:       Types.Model('User', 'UserThing:user'),
    },
  };
}

module.exports = RoleThing;
