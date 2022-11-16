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
      type:      Types.FOREIGN_KEY('Role:id', { onDelete: 'CASCADE', onUpdate: 'CASCADE' }),
      allowNull: true,
      index:     true,
    },
    'userThing': {
      type: Types.Model('UserThing', ({ self }, { UserThing }, userQuery) => {
        return UserThing.$.roleThingID.EQ(self.id).MERGE(userQuery);
      }),
    },
    'role': {
      type: Types.Model('Role', ({ self }, { Role }, userQuery) => {
        return Role.$.id.EQ(self.roleID).MERGE(userQuery);
      }),
    },
    'user': {
      type: Types.Model('User', ({ self }, { UserThing, User }, userQuery) => {
        return User
          .$.id
            .EQ(UserThing.userID)
          .UserThing.roleThingID
            .EQ(self.id)
          .MERGE(userQuery);
      }),
    },
  };
}

module.exports = RoleThing;
