/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll, afterEach, beforeAll, spyOn, fail */

const {
  createConnection,
  truncateTables,
} = require('./sqlite-connection-helper');

describe('SQLiteConnection', () => {
  let connection;

  beforeAll(async () => {
    let setup = await createConnection();
    connection = setup.connection;
  });

  afterEach(async () => {
    await truncateTables(connection);
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
      expect(result.rows).toBeInstanceOf(Array);
      expect(result.rows.length).toEqual(1);
      expect(result.rows[0]).toEqual([ 2 ]);
      expect(result.columns).toBeInstanceOf(Array);
      expect(result.columns.length).toEqual(1);
      expect(result.columns[0]).toEqual('1+1');
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
