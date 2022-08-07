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
      type:         Types.FOREIGN_KEY('Role:id', { onDelete: 'CASCADE', onUpdate: 'CASCADE' }),
      allowNull:    true,
      index:        true,
    },
    'userThing': {
      type:         Types.Model(({ UserThing, userQuery, self }) => {
        return UserThing.$.roleThingID.EQ(self.id).MERGE(userQuery);
      }),
    },
    'role': {
      type:         Types.Model(({ Role, userQuery, self }) => {
        return Role.$.id.EQ(self.roleID).MERGE(userQuery);
      }),
    },
    'user': {
      type:         Types.Model(({ UserThing, User, userQuery, self }) => {
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
