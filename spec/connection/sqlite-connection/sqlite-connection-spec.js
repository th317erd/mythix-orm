/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach, beforeAll, spyOn, fail */

const UUID = require('uuid');
const { SQLiteConnection } = require('../../../src/connection/sqlite-connection');
const { SQLLiteral } = require('../../../src/connection/sql-literals');

describe('SQLiteConnection', () => {
  describe('connection management', () => {
    let connection;
    let User;
    let Role;

    beforeEach(async () => {
      connection = new SQLiteConnection({
        models: require('../../support/models'),
      });

      let models = connection.getModels();

      User = models.User;
      Role = models.Role;
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
        expect(literal.toString(connection)).toEqual('DISTINCT "users"."firstName" AS "User:firstName"');
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

    describe('parseFieldProjection', () => {
      it('can parse a field projection and turn it into a field definition', async () => {
        let queryGenerator  = connection.getQueryGenerator();
        expect(queryGenerator.parseFieldProjection('"users"."id" AS "User:id"')).toEqual('User:id');
        expect(queryGenerator.parseFieldProjection('"users"."firstName" AS "User:firstName"')).toEqual('User:firstName');
      });

      it('can parse a field projection when it is a literal', async () => {
        let queryGenerator  = connection.getQueryGenerator();
        expect(queryGenerator.parseFieldProjection('DISTINCT "users"."id" AS "User:id"')).toEqual('User:id');
      });

      it('can parse a field projection when it is a non-standard format', async () => {
        let queryGenerator  = connection.getQueryGenerator();
        expect(queryGenerator.parseFieldProjection('COUNT("users"."id") AS "User:id"')).toEqual('User:id');
        expect(queryGenerator.parseFieldProjection('COUNT("users"."id")')).toEqual('User:id');
      });
    });

    describe('projectionToFieldMap', () => {
      it('can parse projection and turn it into a field map', async () => {
        let queryGenerator  = connection.getQueryGenerator();
        let sqlStatement    = queryGenerator.generateSelectStatement(User.where.firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('firstName'));

        let result = queryGenerator.parseFieldProjectionToFieldMap(sqlStatement);
        expect(Array.from(result.keys())).toEqual([
          'User:firstName',
          'User:id',
          'User:lastName',
          'User:primaryRoleID',
        ]);

        expect(Array.from(result.values())).toEqual([
          '"users"."firstName" AS "User:firstName"',
          '"users"."id" AS "User:id"',
          '"users"."lastName" AS "User:lastName"',
          '"users"."primaryRoleID" AS "User:primaryRoleID"',
        ]);
      });
    });

    describe('findAllFieldsFromFieldProjectionMap', () => {
      it('will return all fields from projection map', async () => {
        let queryGenerator      = connection.getQueryGenerator();
        let sqlStatement        = queryGenerator.generateSelectStatement(User.where.id.EQ(Role.where.id).firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('User:firstName'));
        let projectionFieldMap  = queryGenerator.parseFieldProjectionToFieldMap(sqlStatement);

        expect(connection.findAllFieldsFromFieldProjectionMap(projectionFieldMap)).toEqual([
          Role.fields.id,
          Role.fields.name,
          User.fields.firstName,
          User.fields.id,
          User.fields.lastName,
          User.fields.primaryRoleID,
        ]);
      });

      it('will return the raw projection field as a string if field can not be found', async () => {
        let queryGenerator      = connection.getQueryGenerator();
        let sqlStatement        = queryGenerator.generateSelectStatement(User.where.id.EQ(Role.where.id).firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('User:firstName').PROJECT('*', new SQLLiteral('COUNT(*)')));
        let projectionFieldMap  = queryGenerator.parseFieldProjectionToFieldMap(sqlStatement);

        expect(connection.findAllFieldsFromFieldProjectionMap(projectionFieldMap)).toEqual([
          'COUNT(*)',
          Role.fields.id,
          Role.fields.name,
          User.fields.firstName,
          User.fields.id,
          User.fields.lastName,
          User.fields.primaryRoleID,
        ]);
      });
    });
  });

  describe('database operations', () => {
    let connection;
    let User;
    let Role;

    const createTable = async (connection, Model) => {
      let queryGenerator  = connection.getQueryGenerator();
      let sqlStr          = queryGenerator.generateCreateTableStatement(Model);

      return await connection.query(sqlStr);
    };

    beforeAll(async () => {
      connection = new SQLiteConnection({
        models: require('../../support/models'),
      });

      let models = connection.getModels();

      User = models.User;
      Role = models.Role;

      await connection.start();

      await createTable(connection, User);
      await createTable(connection, Role);
    });

    beforeEach(async () => {
      // Truncate
      await connection.query('DELETE FROM "users"');
      await connection.query('DELETE FROM "roles"');
    });

    describe('insert query', () => {
      it('should be able to insert a model', async () => {
        let queryGenerator  = connection.getQueryGenerator();
        let sqlStr          = queryGenerator.generateInsertStatement(
          User,
          [
            new User({ firstName: 'Test', lastName: 'User', primaryRoleID: UUID.v4() }),
          ],
        );
        let result          = await connection.query(sqlStr);
        expect(result).toEqual({ changes: 1, lastInsertRowid: 1 });
        expect(connection.formatInsertResponse(sqlStr, result)).toEqual([ 1 ]);
      });

      it('should be able to insert multiple models', async () => {
        let queryGenerator  = connection.getQueryGenerator();
        let sqlStr          = queryGenerator.generateInsertStatement(
          User,
          [
            new User({ firstName: 'Test', lastName: 'User', primaryRoleID: UUID.v4() }),
            new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: UUID.v4() }),
          ],
        );

        let result = await connection.query(sqlStr);
        expect(result).toEqual({ changes: 2, lastInsertRowid: 2 });
        expect(connection.formatInsertResponse(sqlStr, result)).toEqual([ 1, 2 ]);
      });

      it('should be able to request that response be formatted', async () => {
        let queryGenerator  = connection.getQueryGenerator();
        let sqlStr          = queryGenerator.generateInsertStatement(
          User,
          [
            new User({ firstName: 'Test', lastName: 'User', primaryRoleID: UUID.v4() }),
            new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: UUID.v4() }),
          ],
        );

        let result = await connection.query(sqlStr, { formatResponse: true });
        expect(result).toEqual([ 1, 2 ]);
      });
    });

    describe('select query', () => {
      const insertSomeRows = async () => {
        let queryGenerator  = connection.getQueryGenerator();
        let sqlStr          = queryGenerator.generateInsertStatement(
          User,
          [
            new User({ id: '4430db4c-8967-41d9-807c-40811fcee60a', firstName: 'Test', lastName: 'User', primaryRoleID: 'edf06e37-fdd3-4e96-b1fc-dcaff256d24a' }),
            new User({ id: '33144fb7-cffe-454e-8d45-9c585bc89fc6', firstName: 'Mary', lastName: 'Anne', primaryRoleID: '81fe6880-af54-489d-a9dc-facfa98059ab' }),
            new User({ id: 'c69da6dc-189b-43e9-9b98-c9e0ba1d85eb', firstName: 'First', lastName: null, primaryRoleID: 'f1635dbe-1f74-4000-b6af-e9dd92b0025d' }),
          ],
        );

        return await connection.query(sqlStr);
      };

      it('should be able to select rows', async () => {
        await insertSomeRows();

        let queryGenerator  = connection.getQueryGenerator();
        let sqlStatement    = queryGenerator.generateSelectStatement(User.where.firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('firstName'));
        let result          = await connection.query(sqlStatement);

        expect(result).toBeInstanceOf(Array);
        expect(result.length).toEqual(2);
        expect(result[0]).toBeInstanceOf(Array);
        expect(result[0].length).toEqual(4);
        expect(result[1]).toBeInstanceOf(Array);
        expect(result[1].length).toEqual(4);
        expect(result[0][0]).toEqual('First');
        expect(result[0][1]).toMatch(/[a-f0-9-]{36}/);
        expect(result[0][2]).toBe(null);
        expect(result[0][3]).toMatch(/[a-f0-9-]{36}/);
        expect(result[1][0]).toEqual('Mary');
        expect(result[1][1]).toMatch(/[a-f0-9-]{36}/);
        expect(result[1][2]).toEqual('Anne');
        expect(result[1][3]).toMatch(/[a-f0-9-]{36}/);
      });

      it('should map selected rows to model map', async () => {
        await insertSomeRows();

        let queryGenerator  = connection.getQueryGenerator();
        let sqlStatement    = queryGenerator.generateSelectStatement(User.where.firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('firstName'));
        let result          = connection.formatSelectResponse(sqlStatement, await connection.query(sqlStatement));

        expect(result.columns).toEqual([
          'User:firstName',
          'User:id',
          'User:lastName',
          'User:primaryRoleID',
        ]);

        expect(result.rows).toBeInstanceOf(Array);
        expect(result.rows.length).toEqual(2);
      });

      it('should be able to request that response be formatted', async () => {
        await insertSomeRows();

        let queryGenerator  = connection.getQueryGenerator();
        let sqlStatement    = queryGenerator.generateSelectStatement(User.where.firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('firstName'));
        let result          = await connection.query(sqlStatement, { formatResponse: true });

        expect(result.columns).toEqual([
          'User:firstName',
          'User:id',
          'User:lastName',
          'User:primaryRoleID',
        ]);

        expect(result.rows).toBeInstanceOf(Array);
        expect(result.rows.length).toEqual(2);
      });

      it('should be able to generate a model data map from query result', async () => {
        await insertSomeRows();

        let queryGenerator  = connection.getQueryGenerator();
        let sqlStatement    = queryGenerator.generateSelectStatement(User.where.firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('User:firstName'));
        let result          = await connection.query(sqlStatement, { formatResponse: true, logger: console });

        expect(connection.buildModelDataMapFromSelectResults(result)).toEqual({
          User: [
            {
              id:             'c69da6dc-189b-43e9-9b98-c9e0ba1d85eb',
              firstName:      'First',
              lastName:       null,
              primaryRoleID:  'f1635dbe-1f74-4000-b6af-e9dd92b0025d',
            },
            {
              id:             '33144fb7-cffe-454e-8d45-9c585bc89fc6',
              firstName:      'Mary',
              lastName:       'Anne',
              primaryRoleID:  '81fe6880-af54-489d-a9dc-facfa98059ab',
            },
          ],
        });

        expect(result.rows).toBeInstanceOf(Array);
        expect(result.rows.length).toEqual(2);
      });

      it('should be able to generate a model data map from query result with a table join', async () => {
        await insertSomeRows();

        let queryGenerator  = connection.getQueryGenerator();

        await connection.query(queryGenerator.generateInsertStatement(Role, new Role({
          id:   '81fe6880-af54-489d-a9dc-facfa98059ab',
          name: 'derp',
        })));

        let sqlStatement    = queryGenerator.generateSelectStatement(User.where.primaryRoleID.EQ(Role.where.id).firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('User:firstName'));
        let result          = await connection.query(sqlStatement, { formatResponse: true, logger: console });

        expect(connection.buildModelDataMapFromSelectResults(result)).toEqual({
          User: [
            {
              id:             '33144fb7-cffe-454e-8d45-9c585bc89fc6',
              firstName:      'Mary',
              lastName:       'Anne',
              primaryRoleID:  '81fe6880-af54-489d-a9dc-facfa98059ab',
            },
          ],
          Role: [
            {
              id:   '81fe6880-af54-489d-a9dc-facfa98059ab',
              name: 'derp',
            },
          ],
        });

        expect(result.rows).toBeInstanceOf(Array);
        expect(result.rows.length).toEqual(1);
      });

      it('should be able to generate models from model map data', async () => {
        await insertSomeRows();

        let queryGenerator  = connection.getQueryGenerator();

        await connection.query(queryGenerator.generateInsertStatement(Role, new Role({
          id:   '81fe6880-af54-489d-a9dc-facfa98059ab',
          name: 'derp',
        })));

        let query           = User.where.primaryRoleID.EQ(Role.where.id).firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('User:firstName');
        let sqlStatement    = queryGenerator.generateSelectStatement(query);
        let result          = await connection.query(sqlStatement, { formatResponse: true, logger: console });
        let modelDataMap    = connection.buildModelDataMapFromSelectResults(result);
        let users           = connection.buildModelsFromModelDataMap(query, modelDataMap);

        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(1);
        expect(users[0]).toBeInstanceOf(User);
        expect(users[0].toJSON()).toEqual({
          id:             '33144fb7-cffe-454e-8d45-9c585bc89fc6',
          firstName:      'Mary',
          lastName:       'Anne',
          primaryRoleID:  '81fe6880-af54-489d-a9dc-facfa98059ab',
        });
      });
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
