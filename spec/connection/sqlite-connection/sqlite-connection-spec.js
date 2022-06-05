/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach, beforeAll, spyOn, fail */

const { SQLiteConnection } = require('../../../src/connection/sqlite-connection');
const { SQLLiteral } = require('../../../src/connection/sql-literals');
const {
  Role,
  User,
} = require('../../support/models');

describe('SQLiteConnection', () => {
  describe('connection management', () => {
    let connection;

    beforeEach(async () => {
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
        expect(literal.toString(connection)).toEqual('DISTINCT "users"."firstName" AS "User"."firstName"');
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

    describe('generateSavePointName', () => {
      it('can generate a save point name', async () => {
        expect(connection.generateSavePointName()).toMatch(/SP[A-P]{32}/);
      });
    });
  });

  describe('database operations', () => {
    let connection;

    beforeAll(async () => {
      connection = new SQLiteConnection({
        models: [
          User,
          Role,
        ],
      });

      await connection.start();
    });

    describe('exec', () => {
      it('should be able to call exec', async () => {
        let result = await connection.exec('SELECT 1+1');
        expect(result.name).toEqual(':memory:');
      });
    });

    describe('query', () => {
      it('should be able to query the database', async () => {
        let result = await connection.query('SELECT 1+1');
        expect(result).toEqual([ [ 2 ] ]);
      });
    });

    describe('transaction', () => {
      it('should be able to create a transaction', async () => {
        let statements = [];

        const originalQuery = connection.query;

        spyOn(connection, 'query').and.callFake((...args) => {
          statements.push(args);
          return originalQuery.apply(connection, args);
        });

        await connection.transaction(async () => {
          await connection.query('SELECT 1+1');
        });

        expect(statements.length).toEqual(3);
        expect(statements[0][0]).toEqual('BEGIN');
        expect(statements[1][0]).toEqual('SELECT 1+1');
        expect(statements[2][0]).toEqual('COMMIT');
      });

      it('should be able to have transactions inside transactions', async () => {
        let statements = [];

        const originalQuery = connection.query;

        spyOn(connection, 'query').and.callFake((...args) => {
          statements.push(args);
          return originalQuery.apply(connection, args);
        });

        await connection.transaction(async (connection) => {
          await connection.transaction(async (connection) => {
            await connection.query('SELECT 1+1');
          });
        });

        expect(statements.length).toEqual(5);
        expect(statements[0][0]).toEqual('BEGIN');
        expect(statements[1][0]).toMatch(/SAVEPOINT SP[A-P]{32}/);
        expect(statements[2][0]).toEqual('SELECT 1+1');
        expect(statements[3][0]).toMatch('RELEASE SAVEPOINT SP[A-P]{32}');
        expect(statements[4][0]).toEqual('COMMIT');
      });

      it('should rollback if an error is thrown', async () => {
        let statements = [];

        const originalQuery = connection.query;

        spyOn(connection, 'query').and.callFake((...args) => {
          statements.push(args);
          return originalQuery.apply(connection, args);
        });

        try {
          await connection.transaction(async () => {
            await connection.query('DERP 1+1');
          });

          fail('unreachable');
        } catch (error) {
          expect(error.message).toEqual('near "DERP": syntax error');
          expect(statements.length).toEqual(3);
          expect(statements[0][0]).toEqual('BEGIN');
          expect(statements[1][0]).toEqual('DERP 1+1');
          expect(statements[2][0]).toEqual('ROLLBACK');
        }
      });

      it('should rollback if an error is thrown in a sub transaction', async () => {
        let statements = [];

        const originalQuery = connection.query;

        spyOn(connection, 'query').and.callFake((...args) => {
          statements.push(args);
          return originalQuery.apply(connection, args);
        });

        try {
          await connection.transaction(async (connection) => {
            await connection.transaction(async (connection) => {
              await connection.query('DERP 1+1');
            });
          });

          fail('unreachable');
        } catch (error) {
          expect(error.message).toEqual('near "DERP": syntax error');
          expect(statements.length).toEqual(5);
          expect(statements[0][0]).toEqual('BEGIN');
          expect(statements[1][0]).toMatch(/SAVEPOINT SP[A-P]{32}/);
          expect(statements[2][0]).toEqual('DERP 1+1');
          expect(statements[3][0]).toMatch('ROLLBACK TO SAVEPOINT SP[A-P]{32}');
          expect(statements[4][0]).toEqual('ROLLBACK');
        }
      });
    });
  });
});
