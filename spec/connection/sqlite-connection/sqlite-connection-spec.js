/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../src/connection/sqlite-connection');
const { SQLLiteral } = require('../../../src/connection/sql-literals');
const {
  Role,
  User,
} = require('../../support/models');

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

  describe('getSQLLiteralClassByName', () => {
    it('can return literal class', () => {
      expect(SQLiteConnection.getSQLLiteralClassByName('distinct')).toBe(SQLiteConnection.Literals.DistinctSQLLiteral);
      expect(SQLiteConnection.getSQLLiteralClassByName('DISTINCT')).toBe(SQLiteConnection.Literals.DistinctSQLLiteral);
      expect(SQLiteConnection.getSQLLiteralClassByName('Distinct')).toBe(SQLiteConnection.Literals.DistinctSQLLiteral);
      expect(SQLiteConnection.getSQLLiteralClassByName('literal')).toBe(SQLiteConnection.Literals.SQLLiteral);
      expect(SQLiteConnection.getSQLLiteralClassByName('LITERAL')).toBe(SQLiteConnection.Literals.SQLLiteral);
      expect(SQLiteConnection.getSQLLiteralClassByName('base')).toBe(SQLiteConnection.Literals.SQLLiteralBase);
    });
  });

  describe('Literal', () => {
    it('can instantiate a SQL literal', () => {
      expect(SQLiteConnection.Literal('distinct', 'User:firstName')).toBeInstanceOf(SQLiteConnection.Literals.DistinctSQLLiteral);
    });

    it('can stringify a literal to SQL', () => {
      let literal = SQLiteConnection.Literal('distinct', 'User:firstName');
      expect(literal.toString(connection)).toEqual('DISTINCT("users"."firstName")');
    });

    it('will stringify to class name if no connection given', () => {
      let literal = SQLiteConnection.Literal('distinct', 'User:firstName');
      expect(literal.toString()).toEqual('DistinctSQLLiteral {}');
    });
  });

  describe('escape', () => {
    it('can escape a string value', () => {
      expect(connection.escape(User.fields.id, 'test "hello";')).toEqual('\'test \\"hello\\";\'');
    });

    it('can escape a integer value', () => {
      expect(connection.escape(User.fields.id, 10)).toEqual('10');
      expect(connection.escape(User.fields.id, -10)).toEqual('-10');
    });

    it('can escape a number value', () => {
      expect(connection.escape(User.fields.id, 10.345)).toEqual('10.345');
      expect(connection.escape(User.fields.id, -10.345)).toEqual('-10.345');
    });

    it('can escape a boolean value', () => {
      expect(connection.escape(User.fields.id, true)).toEqual('TRUE');
      expect(connection.escape(User.fields.id, false)).toEqual('FALSE');
    });

    it('should not escape a literal value', () => {
      expect(connection.escape(User.fields.id, new SQLLiteral('!$#%'))).toEqual('!$#%');
    });
  });

  describe('escapeID', () => {
    it('can escape a string value', () => {
      expect(connection.escapeID('test.derp')).toEqual('"test"."derp"');
    });

    it('should not escape a literal value', () => {
      expect(connection.escapeID(new SQLLiteral('!$#%'))).toEqual('!$#%');
    });
  });

  describe('dialect', () => {
    it('can return dialect', () => {
      expect(SQLiteConnection.dialect).toEqual('sqlite');
      expect(connection.dialect).toEqual('sqlite');
    });
  });

  describe('start', () => {
    it('can initiate a :memory: DB connection', async () => {
      expect(connection.db).toBe(null);
      await connection.start();
      expect(connection.db).not.toBe(null);
    });
  });

  describe('stop', () => {
    it('can shutdown a DB connection', async () => {
      expect(connection.db).toBe(null);
      await connection.start();
      expect(connection.db).not.toBe(null);

      await connection.stop();
      expect(connection.db).toBe(null);
    });
  });
});
