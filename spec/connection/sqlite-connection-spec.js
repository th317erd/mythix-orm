/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../src/connection/sqlite-connection');
const {
  Role,
  User,
} = require('../support/models');

describe('SQLiteConnection', () => {
  let connection;

  beforeEach(() => {
    connection = new SQLiteConnection({
      models: [
        User,
        Role,
      ],
    });
  });

  describe('escape', () => {
    it('can escape a string value', () => {
      expect(connection.escape('test "hello";')).toEqual('\'test \\"hello\\";\'');
    });

    it('can escape a integer value', () => {
      expect(connection.escape(10)).toEqual('10');
      expect(connection.escape(-10)).toEqual('-10');
    });

    it('can escape a number value', () => {
      expect(connection.escape(10.345)).toEqual('10.345');
      expect(connection.escape(-10.345)).toEqual('-10.345');
    });

    it('can escape a boolean value', () => {
      expect(connection.escape(true)).toEqual('TRUE');
      expect(connection.escape(false)).toEqual('FALSE');
    });
  });

  describe('escapeID', () => {
    it('can escape a string value', () => {
      expect(connection.escapeID('test.derp')).toEqual('"test"."derp"');
    });
  });

  describe('generatorCreateTableStatement', () => {
    it('can generate a create table statement', async () => {
      expect(await connection.generatorCreateTableStatement(User)).toEqual('CREATE TABLE IF NOT EXISTS "users" (  "id" VARCHAR(36) PRIMARY KEY,\n  "firstName" VARCHAR(64),\n  "lastName" VARCHAR(64),\n  "primaryRoleID" VARCHAR(36) NOT NULL\n);');
    });
  });
});
