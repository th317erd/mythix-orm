'use strict';

const { Model, Types } = require('../../../lib');

class User extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'firstName': {
      type:         Types.STRING(64),
      allowNull:    true,
      index:        true,
    },
    'lastName': {
      type:         Types.STRING(64),
      allowNull:    true,
      index:        true,
    },
    'primaryRoleID': {
      type:         Types.FOREIGN_KEY('Role:id', { onDelete: 'SET NULL', onUpdate: 'SET NULL' }),
      allowNull:    true,
    },
    'roles': {
      type:         Types.Models('Role', ({ self }, { Role, UserRole }, userQuery) => {
        return Role
          .$.id
            .EQ(UserRole.$.roleID)
          .UserRole.userID
            .EQ(self.id)
          .MERGE(userQuery);
      }),
    },
    'userRoles': {
      type:         Types.Models('UserRole', ({ self }, { UserRole }, userQuery) => {
        return UserRole.$.userID.EQ(self.id).MERGE(userQuery);
      }),
    },
    'userThing': {
      type:         Types.Model('UserThing', ({ self }, { UserThing }, userQuery) => {
        return UserThing
          .$.userID
            .EQ(self.id)
          .MERGE(userQuery);
      }),
    },
    'userThingRole': {
      type:         Types.Model('Role', ({ self }, { Role, UserThing, RoleThing }, userQuery) => {
        return Role
          .$.id
            .EQ(RoleThing.$.roleID)
          .RoleThing.id
            .EQ(UserThing.$.roleThingID)
          .UserThing.userID
            .EQ(self.id)
          .MERGE(userQuery);
      }),
    },
    'primaryRole': {
      type:         Types.Model('Role', ({ self }, { Role }, userQuery) => {
        return Role.$.id.EQ(self.primaryRoleID).MERGE(userQuery);
      }),
    },
  };
}

module.exports = User;
