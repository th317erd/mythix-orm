/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const Types                 = require('../../../src/types');
const { SQLiteConnection }  = require('../../../src/connection/sqlite-connection');
const {
  Role,
  User,
} = require('../../support/models');

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

describe('SQLiteQueryGenerator', () => {
  let connection;

  beforeEach(() => {
    connection = new SQLiteConnection({
      models: [
        User,
        Role,
        ExtendedUser,
      ],
    });
  });

  describe('generatorCreateTableStatement', () => {
    it('can generate a create table statement #1', async () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(await queryGenerator.generatorCreateTableStatement(User)).toEqual('CREATE TABLE IF NOT EXISTS "users" (  "id" VARCHAR(36) PRIMARY KEY,\n  "firstName" VARCHAR(64),\n  "lastName" VARCHAR(64),\n  "primaryRoleID" VARCHAR(36) NOT NULL\n);');
    });

    it('can generate a create table statement #2', async () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(await queryGenerator.generatorCreateTableStatement(ExtendedUser)).toEqual('CREATE TABLE IF NOT EXISTS "extended_users" (  "id" INTEGER PRIMARY KEY AUTOINCREMENT,\n  "createdAt" DATETIME NOT NULL DEFAULT (datetime(\'now\')),\n  "email" VARCHAR(256) UNIQUE NOT NULL,\n  "firstName" VARCHAR(64),\n  "lastName" VARCHAR(64),\n  "playerType" VARCHAR(256) NOT NULL DEFAULT \'wizard\',\n  "primaryRole" VARCHAR(256) NOT NULL DEFAULT \'user\',\n  "primaryRoleID" VARCHAR(36) NOT NULL\n);');
    });
  });
});
