/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../src/connection/sqlite-connection');
const {
  Role,
  User,
} = require('../../support/models');

describe('SQLiteQueryGenerator', () => {
  let connection;

  beforeEach(() => {
    connection = new SQLiteConnection({
      models: [
        User,
        Role,
      ],
    });
  });

  describe('generatorCreateTableStatement', () => {
    it('can generate a create table statement', async () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(await queryGenerator.generatorCreateTableStatement(User)).toEqual('CREATE TABLE IF NOT EXISTS "users" (  "id" VARCHAR(36) PRIMARY KEY,\n  "firstName" VARCHAR(64),\n  "lastName" VARCHAR(64),\n  "primaryRoleID" VARCHAR(36) NOT NULL\n);');
    });
  });
});
