'use strict';

const { Model, Types } = require('../../../lib');

class UserRole extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'roleID': {
      type:         Types.FOREIGN_KEY('Role:id', { onDelete: 'CASCADE', onUpdate: 'CASCADE' }),
      allowNull:    false,
      index:        true,
    },
    'userID': {
      type:         Types.FOREIGN_KEY('User:id', { onDelete: 'CASCADE', onUpdate: 'CASCADE' }),
      allowNull:    false,
      index:        true,
    },
    'role': {
      type:         Types.Model(({ Role, userQuery, self }) => {
        return Role.$.id.EQ(self.roleID).MERGE(userQuery);
      }),
    },
    'user': {
      type:         Types.Model(({ User, userQuery, self }) => {
        return User.$.id.EQ(self.userID).MERGE(userQuery);
      }),
    },
  };
}

module.exports = UserRole;
