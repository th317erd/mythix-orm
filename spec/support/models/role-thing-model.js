'use strict';

const { Model, Types } = require('../../../lib');

class RoleThing extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'roleID': {
      type:       Types.FOREIGN_KEY('Role:id', { onDelete: 'CASCADE', onUpdate: 'CASCADE' }),
      allowNull:  true,
      index:      true,
    },
    'userThing': {
      type:       Types.Model('UserThing:roleThingID'),
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
