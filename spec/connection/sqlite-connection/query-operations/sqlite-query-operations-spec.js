/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll, afterEach, beforeAll */

const { UUID_REGEXP } = require('../../../support/test-helpers');
const Utils = require('../../../../lib/utils');

const {
  createConnection,
  truncateTables,
} = require('../sqlite-connection-helper');

describe('SQLiteConnection', () => {
  let connection;
  let User;
  let Role;

  beforeAll(async () => {
    let setup = await createConnection();

    connection = setup.connection;
    User = setup.User;
    Role = setup.Role;
  });

  afterEach(async () => {
    await truncateTables(connection);
  });

  describe('destroy query', () => {
    it('should be able to destroy a model', async () => {
      let models = await connection.insert(
        User,
        [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
        ],
      );

      expect(models).toBeInstanceOf(Array);
      expect(models.length).toEqual(1);
      expect(models[0].id).toMatch(UUID_REGEXP);
      expect(models[0].firstName).toMatch('Test');
      expect(models[0].lastName).toMatch('User');
      expect(models[0].primaryRole).toBeInstanceOf(Role);
      expect(models[0].primaryRole.id).toMatch(UUID_REGEXP);
      expect(models[0].primaryRole.name).toEqual('test');

      expect(await Role.count()).toEqual(1);

      // Reload to ensure that it saved properly
      let primaryRole = await models[0].getPrimaryRole();
      expect(primaryRole).toBeInstanceOf(Role);
      expect(primaryRole.name).toEqual('test');

      let queryGenerator  = connection.getQueryGenerator();
      let sqlStr          = queryGenerator.generateDeleteStatement(User, User.where.id.EQ(models[0].id));
      await connection.query(sqlStr);

      models = await Utils.collect(connection.select(User.where));
      expect(models).toBeInstanceOf(Array);
      expect(models.length).toEqual(0);

      // Role should still exist
      expect(await Role.count()).toEqual(1);
    });

    it('should be able to delete a specific model', async () => {
      let models = await connection.insert(
        User,
        [
          new User({ firstName: 'Test', lastName: 'User1', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Test', lastName: 'User2', primaryRole: new Role({ name: 'test' }) }),
        ],
      );

      expect(models).toBeInstanceOf(Array);
      expect(models.length).toEqual(2);
      expect(models[0].id).toMatch(UUID_REGEXP);
      expect(models[0].firstName).toMatch('Test');
      expect(models[0].lastName).toMatch('User1');
      expect(models[1].id).toMatch(UUID_REGEXP);
      expect(models[1].firstName).toMatch('Test');
      expect(models[1].lastName).toMatch('User2');

      let queryGenerator  = connection.getQueryGenerator();
      let sqlStr          = queryGenerator.generateDeleteStatement(User, User.where.lastName.EQ('User2'));
      await connection.query(sqlStr);

      models = await Utils.collect(connection.select(User.where));
      expect(models).toBeInstanceOf(Array);
      expect(models.length).toEqual(1);
      expect(models[0].id).toMatch(models[0].id);
      expect(models[0].firstName).toMatch('Test');
      expect(models[0].lastName).toMatch('User1');
    });
  });

  describe('update query', () => {
    it('should be able to update a model', async () => {
      let models = await connection.insert(
        User,
        [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
        ],
      );

      expect(models).toBeInstanceOf(Array);
      expect(models.length).toEqual(1);
      expect(models[0].id).toMatch(UUID_REGEXP);
      expect(models[0].firstName).toMatch('Test');
      expect(models[0].lastName).toMatch('User');

      let model = models[0];
      model.firstName = 'Derp';
      model.lastName = 'Burp';

      let queryGenerator  = connection.getQueryGenerator();
      let sqlStr          = queryGenerator.generateUpdateStatement(User, models[0]);
      await connection.query(sqlStr);

      models = await Utils.collect(connection.select(User.where));
      expect(models).toBeInstanceOf(Array);
      expect(models.length).toEqual(1);
      expect(models[0].id).toMatch(UUID_REGEXP);
      expect(models[0].firstName).toMatch('Derp');
      expect(models[0].lastName).toMatch('Burp');
    });

    it('should be able to update a specific model', async () => {
      let models = await connection.insert(
        User,
        [
          new User({ firstName: 'Test', lastName: 'User1', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Test', lastName: 'User2', primaryRole: new Role({ name: 'test' }) }),
        ],
      );

      expect(models).toBeInstanceOf(Array);
      expect(models.length).toEqual(2);
      expect(models[0].id).toMatch(UUID_REGEXP);
      expect(models[0].firstName).toMatch('Test');
      expect(models[0].lastName).toMatch('User1');
      expect(models[1].id).toMatch(UUID_REGEXP);
      expect(models[1].firstName).toMatch('Test');
      expect(models[1].lastName).toMatch('User2');

      let queryGenerator  = connection.getQueryGenerator();
      let sqlStr          = queryGenerator.generateUpdateStatement(User, { firstName: 'Derp', lastName: 'Burp' }, User.where.lastName.EQ('User2'));
      await connection.query(sqlStr);

      models = await Utils.collect(connection.select(User.where));
      expect(models).toBeInstanceOf(Array);
      expect(models.length).toEqual(2);
      expect(models[0].id).toMatch(models[0].id);
      expect(models[0].firstName).toMatch('Test');
      expect(models[0].lastName).toMatch('User1');
      expect(models[1].id).toMatch(models[1].id);
      expect(models[1].firstName).toMatch('Derp');
      expect(models[1].lastName).toMatch('Burp');
    });
  });

  describe('insert query', () => {
    it('should be able to insert a model', async () => {
      let queryGenerator  = connection.getQueryGenerator();
      let sqlStr          = queryGenerator.generateInsertStatement(
        User,
        [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
        ],
      );

      let result = await connection.query(sqlStr);
      expect(result.rows).toBeInstanceOf(Array);
      expect(result.rows.length).toEqual(1);
      expect(result.rows[0][0]).toMatch(UUID_REGEXP);
      expect(result.columns).toBeInstanceOf(Array);
      expect(result.columns).toEqual([ 'id' ]);
    });

    it('should be able to insert multiple models', async () => {
      let queryGenerator  = connection.getQueryGenerator();
      let sqlStr          = queryGenerator.generateInsertStatement(
        User,
        [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ],
      );


      let result = await connection.query(sqlStr);
      expect(result.rows).toBeInstanceOf(Array);
      expect(result.rows.length).toEqual(2);
      expect(result.rows[0][0]).toMatch(UUID_REGEXP);
      expect(result.rows[1][0]).toMatch(UUID_REGEXP);
      expect(result.columns).toBeInstanceOf(Array);
      expect(result.columns).toEqual([ 'id' ]);
    });
  });

  describe('select query', () => {
    const insertSomeRows = async () => {
      return await connection.insert(
        User,
        [
          new User({ id: '4430db4c-8967-41d9-807c-40811fcee60a', firstName: 'Test', lastName: 'User', primaryRole: new Role({ id: 'edf06e37-fdd3-4e96-b1fc-dcaff256d24a', name: 'test' }) }),
          new User({ id: '33144fb7-cffe-454e-8d45-9c585bc89fc6', firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ id: '81fe6880-af54-489d-a9dc-facfa98059ab', name: 'derp' }) }),
          new User({ id: 'c69da6dc-189b-43e9-9b98-c9e0ba1d85eb', firstName: 'First', lastName: null, primaryRole: new Role({ id: 'f1635dbe-1f74-4000-b6af-e9dd92b0025d', name: 'derp2' }) }),
        ],
      );
    };

    it('should be able to select rows', async () => {
      await insertSomeRows();

      let queryGenerator  = connection.getQueryGenerator();
      let sqlStatement    = queryGenerator.generateSelectStatement(User.where.firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('firstName'));
      let result          = await connection.query(sqlStatement);

      // result = [ [ firstName, id, lastName, primaryRoleID ], ... ]

      expect(result.rows).toBeInstanceOf(Array);
      expect(result.rows.length).toEqual(2);
      expect(result.rows[0]).toBeInstanceOf(Array);
      expect(result.rows[0].length).toEqual(4);
      expect(result.rows[1]).toBeInstanceOf(Array);
      expect(result.rows[1].length).toEqual(4);
      expect(result.rows[0][0]).toEqual('First');
      expect(result.rows[0][1]).toMatch(/[a-f0-9-]{36}/);
      expect(result.rows[0][2]).toBe(null);
      expect(result.rows[0][3]).toMatch(/[a-f0-9-]{36}/);
      expect(result.rows[1][0]).toEqual('Mary');
      expect(result.rows[1][1]).toMatch(/[a-f0-9-]{36}/);
      expect(result.rows[1][2]).toEqual('Anne');
      expect(result.rows[1][3]).toMatch(/[a-f0-9-]{36}/);
      expect(result.columns).toBeInstanceOf(Array);
      expect(result.columns).toEqual([ 'User:firstName', 'User:id', 'User:lastName', 'User:primaryRoleID' ]);
    });

    it('should be able to request that response be formatted', async () => {
      await insertSomeRows();

      let queryGenerator  = connection.getQueryGenerator();
      let sqlStatement    = queryGenerator.generateSelectStatement(User.where.firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('firstName'));
      let result          = await connection.query(sqlStatement);

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

      let query           = User.where.firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('User:firstName');
      let queryGenerator  = connection.getQueryGenerator();
      let sqlStatement    = queryGenerator.generateSelectStatement(query);
      let result          = await connection.query(sqlStatement);

      expect(connection.buildModelDataMapFromSelectResults(query, result)).toEqual({
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
      let query           = User.where.primaryRoleID.EQ(Role.where.id).firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('User:firstName');
      let sqlStatement    = queryGenerator.generateSelectStatement(query);
      let result          = await connection.query(sqlStatement);

      expect(connection.buildModelDataMapFromSelectResults(query, result)).toEqual({
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
        Role: [
          {
            id:   'f1635dbe-1f74-4000-b6af-e9dd92b0025d',
            name: 'derp2',
          },
          {
            id:   '81fe6880-af54-489d-a9dc-facfa98059ab',
            name: 'derp',
          },
        ],
      });

      expect(result.rows).toBeInstanceOf(Array);
      expect(result.rows.length).toEqual(2);
    });

    it('should be able to generate models from model map data', async () => {
      await insertSomeRows();

      let queryGenerator  = connection.getQueryGenerator();
      let query           = User.where.primaryRoleID.EQ(Role.where.id).firstName.EQ('Mary').OR.lastName.EQ(null).ORDER('User:firstName');
      let sqlStatement    = queryGenerator.generateSelectStatement(query);
      let result          = await connection.query(sqlStatement);
      let modelDataMap    = connection.buildModelDataMapFromSelectResults(query, result);
      let users           = connection.buildModelsFromModelDataMap(query, modelDataMap);

      expect(users).toBeInstanceOf(Array);
      expect(users.length).toEqual(2);
      expect(users[0]).toBeInstanceOf(User);
      expect(users[1]).toBeInstanceOf(User);

      expect(users[0].toJSON()).toEqual({
        id:             'c69da6dc-189b-43e9-9b98-c9e0ba1d85eb',
        firstName:      'First',
        lastName:       null,
        primaryRoleID:  'f1635dbe-1f74-4000-b6af-e9dd92b0025d',
      });

      expect(users[1].toJSON()).toEqual({
        id:             '33144fb7-cffe-454e-8d45-9c585bc89fc6',
        firstName:      'Mary',
        lastName:       'Anne',
        primaryRoleID:  '81fe6880-af54-489d-a9dc-facfa98059ab',
      });

      expect(Object.keys(users[0]._).sort()).toEqual([ 'Roles' ]);
      expect(Object.keys(users[1]._).sort()).toEqual([ 'Roles' ]);
      expect(users[0]._.Roles).toBeInstanceOf(Array);
      expect(users[1]._.Roles).toBeInstanceOf(Array);
      expect(users[0]._.Roles.length).toEqual(1);
      expect(users[1]._.Roles.length).toEqual(1);

      expect(users[0]._.Roles[0].toJSON()).toEqual({
        id:   'f1635dbe-1f74-4000-b6af-e9dd92b0025d',
        name: 'derp2',
      });

      expect(users[1]._.Roles[0].toJSON()).toEqual({
        id:   '81fe6880-af54-489d-a9dc-facfa98059ab',
        name: 'derp',
      });
    });
  });
});
