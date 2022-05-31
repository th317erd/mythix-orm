'use strict';

const { Types } = require('../../../src');
const User      = require('./user-model');

class ExtendedUser extends User {
  static fields = User.cloneFields({
    'id': {
      type:         Types.INTEGER,
      defaultValue: Types.INTEGER.Default.AUTO_INCREMENT,
      primaryKey:   true,
      allowNull:    false,
      unique:       true, // should be ignored when create table query is generated
    },
    'createdAt': {
      type:         Types.DATETIME,
      defaultValue: Types.DATETIME.Default.NOW,
      allowNull:    false,
    },
    'email': {
      type:         Types.STRING(256),
      allowNull:    false,
      unique:       true,
    },
    'primaryRole': {
      type:         Types.STRING(256),
      defaultValue: () => {
        return 'user';
      },
      allowNull:    false,
    },
    'playerType': {
      type:         Types.STRING(256),
      defaultValue: 'wizard',
      allowNull:    false,
    },
  });
}

module.exports = ExtendedUser;
