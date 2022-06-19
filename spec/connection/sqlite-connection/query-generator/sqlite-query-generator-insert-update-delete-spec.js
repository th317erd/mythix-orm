/* eslint-disable indent */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../../src/connection/sqlite-connection');

describe('SQLiteQueryGenerator', () => {
  let connection;
  let User;

  beforeEach(() => {
    connection = new SQLiteConnection({
      models: require('../../../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
  });

  describe('generateInsertFieldValuesFromModel', () => {
    it('should generate all values for all fields', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertFieldValuesFromModel(new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }));

      expect(result).toEqual('\'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\'Test\',\'User\'');
    });

    it('should generate all values for dirty fields', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertFieldValuesFromModel(
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
        { dirtyFields: [ User.fields.id, User.fields.firstName, User.fields.lastName ] },
      );

      expect(result).toEqual('\'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\'Test\',\'User\'');
    });
  });

  describe('generateInsertValuesFromModels', () => {
    it('should generate all values for all fields', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertValuesFromModels(User, [
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      ]);

      expect(result).toEqual('(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\'Test\',\'User\')');
    });

    it('should generate all values for multiple models', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertValuesFromModels(User, [
        { id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbfc', firstName: 'Johnny', lastName: 'Bob' },
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      ]);

      expect(result).toEqual('(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbfc\',\'Johnny\',\'Bob\'),\n(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\'Test\',\'User\')');
    });

    it('should skip newlines when requested to do so', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertValuesFromModels(User, [
        { id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbfc', firstName: 'Johnny', lastName: 'Bob' },
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      ], { newlines: false });

      expect(result).toEqual('(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbfc\',\'Johnny\',\'Bob\'),(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\'Test\',\'User\')');
    });

    it('should work with a startIndex and endIndex', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertValuesFromModels(User, [
        { id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbfc', firstName: 'Johnny', lastName: 'Bob' },
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      ], { newlines: false, startIndex: 0, endIndex: 1 });

      expect(result).toEqual('(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbfc\',\'Johnny\',\'Bob\')');

      result = queryGenerator.generateInsertValuesFromModels(User, [
        { id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbfc', firstName: 'Johnny', lastName: 'Bob' },
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      ], { newlines: false, startIndex: 1, endIndex: 2 });

      expect(result).toEqual('(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\'Test\',\'User\')');
    });

    it('should work with a startIndex and batchSize', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertValuesFromModels(User, [
        { id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbfc', firstName: 'Johnny', lastName: 'Bob' },
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      ], { newlines: false, startIndex: 0, batchSize: 1 });

      expect(result).toEqual('(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbfc\',\'Johnny\',\'Bob\')');

      result = queryGenerator.generateInsertValuesFromModels(User, [
        { id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbfc', firstName: 'Johnny', lastName: 'Bob' },
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      ], { newlines: false, startIndex: 1, batchSize: 1 });

      expect(result).toEqual('(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\'Test\',\'User\')');
    });
  });

  describe('generateInsertStatement', () => {
    it('should generate an insert statement', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertStatement(User, [
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      ]);

      expect(result).toEqual('INSERT INTO "users" ("id","firstName","lastName") VALUES (\'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\'Test\',\'User\')');
    });

    it('should generate an insert statement for multiple models', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertStatement(User, [
        { id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbfc', firstName: 'Johnny', lastName: 'Bob' },
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      ]);

      expect(result).toEqual('INSERT INTO "users" ("id","firstName","lastName") VALUES (\'6a69f57b-9ada-45cd-8dd9-23a753a2bbfc\',\'Johnny\',\'Bob\'),\n(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\'Test\',\'User\')');
    });

    it('should skip newlines', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertStatement(User, [
        { id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbfc', firstName: 'Johnny', lastName: 'Bob' },
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      ], { newlines: false });

      expect(result).toEqual('INSERT INTO "users" ("id","firstName","lastName") VALUES (\'6a69f57b-9ada-45cd-8dd9-23a753a2bbfc\',\'Johnny\',\'Bob\'),(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\'Test\',\'User\')');
    });

    it('should skip newlines', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertStatement(User, [
        { id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbfc', firstName: 'Johnny', lastName: 'Bob' },
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      ], { newlines: false });

      expect(result).toEqual('INSERT INTO "users" ("id","firstName","lastName") VALUES (\'6a69f57b-9ada-45cd-8dd9-23a753a2bbfc\',\'Johnny\',\'Bob\'),(\'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\'Test\',\'User\')');
    });

    it('should generate nothing if no models provided', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateInsertStatement(User, [], { newlines: false });

      expect(result).toEqual('');

      result = queryGenerator.generateInsertStatement(User, undefined, { newlines: false });
      expect(result).toEqual('');
    });

    it('should generate nothing if models provided are not dirty', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let user            = new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' });

      user.clearDirty();

      let result = queryGenerator.generateInsertStatement(User, [], { newlines: false });
      expect(result).toEqual('');
    });
  });

  describe('generateUpdateStatement', () => {
    it('should generate an update statement', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateUpdateStatement(
        User,
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
      );

      expect(result).toEqual('UPDATE "users" SET \n  "id" = \'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\n  "firstName" = \'Test\',\n  "lastName" = \'User\'');
    });

    it('should generate an update statement with a where clause', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateUpdateStatement(
        User,
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
        User.where.firstName.EQ('Bob'),
      );

      expect(result).toEqual('UPDATE "users" SET \n  "id" = \'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\n  "firstName" = \'Test\',\n  "lastName" = \'User\'\nWHERE "users"."firstName" = \'Bob\'');
    });

    it('should generate an update statement with a where clause and an order, limit, and offset', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateUpdateStatement(
        User,
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
        User.where.firstName.EQ('Bob').ORDER('firstName').LIMIT(100).OFFSET(10),
      );

      expect(result).toEqual('UPDATE "users" SET \n  "id" = \'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\n  "firstName" = \'Test\',\n  "lastName" = \'User\'\nWHERE "users"."firstName" = \'Bob\' ORDER BY "users"."firstName" ASC LIMIT 100 OFFSET 10');
    });

    it('should skip newlines', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateUpdateStatement(
        User,
        new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' }),
        { newlines: false },
      );

      expect(result).toEqual('UPDATE "users" SET "id" = \'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',"firstName" = \'Test\',"lastName" = \'User\'');
    });

    it('should generate an update statement using an object instead of a model instance', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateUpdateStatement(
        User,
        { id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' },
      );

      expect(result).toEqual('UPDATE "users" SET \n  "id" = \'6a69f57b-9ada-45cd-8dd9-23a753a2bbf3\',\n  "firstName" = \'Test\',\n  "lastName" = \'User\'');
    });

    it('should generate nothing if model is not dirty', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let user            = new User({ id: '6a69f57b-9ada-45cd-8dd9-23a753a2bbf3', firstName: 'Test', lastName: 'User' });

      user.clearDirty();

      let result = queryGenerator.generateUpdateStatement(
        User,
        user,
      );

      expect(result).toEqual('');
    });
  });

  describe('generateDeleteStatement', () => {
    it('should generate a delete statement', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateDeleteStatement(User);

      expect(result).toEqual('DELETE FROM "users"');
    });

    it('should generate a delete statement with a where clause', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateDeleteStatement(User, User.where.id.EQ('test'));

      expect(result).toEqual('DELETE FROM "users" WHERE "users"."id" = \'test\'');
    });

    it('should generate a delete statement with an order, limit, and offset', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateDeleteStatement(User, User.where.ORDER('firstName').LIMIT(50).OFFSET(10));

      expect(result).toEqual('DELETE FROM "users" ORDER BY "users"."firstName" ASC LIMIT 50 OFFSET 10');
    });

    it('should generate a delete statement with a where clause, and an order, limit, and offset', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateDeleteStatement(User, User.where.firstName.EQ('Bob').ORDER('firstName').LIMIT(50).OFFSET(10));

      expect(result).toEqual('DELETE FROM "users" WHERE "users"."firstName" = \'Bob\' ORDER BY "users"."firstName" ASC LIMIT 50 OFFSET 10');
    });
  });
});
