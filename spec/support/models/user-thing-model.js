'use strict';

const { Model, Types } = require('../../../lib');

class UserThing extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'userID': {
      type:         Types.FOREIGN_KEY('User:id', { onDelete: 'CASCADE', onUpdate: 'CASCADE' }),
      allowNull:    false,
      index:        true,
    },
    'roleThingID': {
      type:         Types.FOREIGN_KEY('RoleThing:id', { onDelete: 'CASCADE', onUpdate: 'CASCADE' }),
      allowNull:    false,
      index:        true,
    },
    'roleThing': {
      type:         Types.Model(({ RoleThing, userQuery, self }) => {
        return RoleThing.$.id.EQ(self.roleThingID).MERGE(userQuery);
      }),
    },
    'role': {
      type:         Types.Model(({ Role, RoleThing, userQuery, self }) => {
        return Role
          .$.id
            .EQ(RoleThing.$.roleID)
          .RoleThing.id
            .EQ(self.roleThingID)
          .MERGE(userQuery);
      }),
    },
    'user': {
      type:         Types.Model(({ User, userQuery, self }) => {
        return User.$.id.EQ(self.userID).MERGE(userQuery);
      }),
    },
  };
}

module.exports = UserThing;
