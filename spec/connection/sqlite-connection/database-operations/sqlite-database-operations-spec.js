/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll, afterEach, beforeAll, spyOn */

const UUID = require('uuid');
const Utils = require('../../../../lib/utils');

const {
  createConnection,
  truncateTables,
} = require('../sqlite-connection-helper');

describe('SQLiteConnection', () => {
  describe('database operations', () => {
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

    describe('pluck', () => {
      it('should be able to pluck values', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        await connection.insert(User, insertModels);

        let values = await connection.pluck(User.where.ORDER('firstName'), 'firstName');
        expect(values).toEqual([
          'Mary',
          'Test',
        ]);
      });

      it('should be able to pluck values from multiple fields', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        await connection.insert(User, insertModels);

        let values = await connection.pluck(User.where.ORDER('firstName'), 'firstName', 'lastName');
        expect(values).toEqual([
          [ 'Mary', 'Anne' ],
          [ 'Test', 'User' ],
        ]);
      });

      it('should be able to specify fields in multiple ways', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        await connection.insert(User, insertModels);

        let values = await connection.pluck(User.where.ORDER('firstName'), 'firstName', [ 'lastName' ]);
        expect(values).toEqual([
          [ 'Mary', 'Anne' ],
          [ 'Test', 'User' ],
        ]);
      });
    });

    describe('count', () => {
      it('should be able to count models', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        await connection.insert(User, insertModels);

        let count = await connection.count(User.where);
        expect(count).toEqual(2);
      });

      it('should be able to count models specifying a field', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: null, primaryRole: new Role({ name: 'member' }) }),
        ];

        await connection.insert(User, insertModels);

        let count = await connection.count(User.where, 'firstName');
        expect(count).toEqual(2);

        count = await connection.count(User.where, 'lastName');
        expect(count).toEqual(1);
      });

      it('should be able to count models with a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        await connection.insert(User, insertModels);

        let count = await connection.count(User.where.firstName.EQ('Test'));
        expect(count).toEqual(1);
      });
    });

    describe('select', () => {
      it('should be able to select models', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        await connection.insert(User, insertModels);

        let users = await Utils.collect(connection.select(User.where));
        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(2);
        expect(users[0]).toBeInstanceOf(User);
        expect(users[1]).toBeInstanceOf(User);
        expect(users[0].id).toEqual(insertModels[0].id);
        expect(users[1].id).toEqual(insertModels[1].id);
      });

      it('should be able to select specific models', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        await connection.insert(User, insertModels);

        let users = await Utils.collect(connection.select(User.where.lastName.EQ('Anne')));
        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(1);
        expect(users[0]).toBeInstanceOf(User);
        expect(users[0].id).toEqual(insertModels[1].id);
      });

      it('should properly batch', async () => {
        let insertModels  = [];
        let testValues    = [];

        for (let i = 0; i < 100; i++) {
          let firstName = `Test${i}`;
          insertModels.push(new User({ firstName, lastName: `User${i}`, primaryRole: new Role({ name: 'member' }) }));
          testValues.push(firstName);
        }

        await connection.insert(User, insertModels);

        let queryGenerator = connection.getQueryGenerator();
        spyOn(queryGenerator, 'generateSelectStatement').and.callThrough();

        let users = await Utils.collect(connection.select(User.where, { batchSize: 20 }));
        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(100);
        expect(queryGenerator.generateSelectStatement.calls.count()).toEqual(6);

        for (let i = 0, il = users.length; i < il; i++) {
          let user      = users[i];
          let testValue = testValues[i];

          expect(user).toBeInstanceOf(User);
          expect(user.firstName).toEqual(testValue);
        }
      });
    });

    describe('insert', () => {
      it('should be able to insert models', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        spyOn(connection, 'prepareAllModelsForOperation').and.callThrough();

        let storedModels = await connection.insert(User, insertModels);

        expect(connection.prepareAllModelsForOperation.calls.count()).toEqual(6);
        expect(storedModels).toBeInstanceOf(Array);
        expect(storedModels.length).toEqual(2);

        let queryGenerator  = connection.getQueryGenerator();
        let query           = User.where;
        let sqlStatement    = queryGenerator.generateSelectStatement(query);
        let result          = await connection.query(sqlStatement, { formatResponse: true });
        let modelDataMap    = connection.buildModelDataMapFromSelectResults(query, result);
        let users           = connection.buildModelsFromModelDataMap(query, modelDataMap);

        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(2);
        expect(users[0]).toBeInstanceOf(User);
        expect(users[1]).toBeInstanceOf(User);
        expect(users[0].id).toEqual(insertModels[0].id);
        expect(users[1].id).toEqual(insertModels[1].id);
      });

      it('should be able to change batchSize', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        spyOn(connection, 'prepareAllModelsForOperation').and.callThrough();

        let storedModels = await connection.insert(User, insertModels, { batchSize: 1 });

        expect(connection.prepareAllModelsForOperation.calls.count()).toEqual(12);
        expect(storedModels).toBeInstanceOf(Array);
        expect(storedModels.length).toEqual(2);

        let queryGenerator  = connection.getQueryGenerator();
        let query           = User.where;
        let sqlStatement    = queryGenerator.generateSelectStatement(query);
        let result          = await connection.query(sqlStatement, { formatResponse: true });
        let modelDataMap    = connection.buildModelDataMapFromSelectResults(query, result);
        let users           = connection.buildModelsFromModelDataMap(query, modelDataMap);

        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(2);
        expect(users[0]).toBeInstanceOf(User);
        expect(users[1]).toBeInstanceOf(User);
        expect(users[0].id).toEqual(insertModels[0].id);
        expect(users[1].id).toEqual(insertModels[1].id);
      });
    });

    describe('update', () => {
      it('should be able to update models', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        let storedModels = await connection.insert(User, insertModels);
        expect(storedModels).toBeInstanceOf(Array);
        expect(storedModels.length).toEqual(2);

        storedModels[0].firstName = 'Test1';
        storedModels[1].lastName = 'Test1';

        let users = await connection.update(User, storedModels);

        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(2);
        expect(users[0]).toBeInstanceOf(User);
        expect(users[1]).toBeInstanceOf(User);
        expect(users[0].id).toEqual(insertModels[0].id);
        expect(users[1].id).toEqual(insertModels[1].id);

        // Ensure the changes were persisted
        let storedUsers = await Utils.collect(connection.select(User));
        expect(storedUsers).toBeInstanceOf(Array);
        expect(storedUsers.length).toEqual(2);
        expect(storedUsers[0]).toBeInstanceOf(User);
        expect(storedUsers[1]).toBeInstanceOf(User);
        expect(storedUsers[0].id).toEqual(insertModels[0].id);
        expect(storedUsers[0].firstName).toEqual('Test1');
        expect(storedUsers[0].lastName).toEqual(insertModels[0].lastName);
        expect(storedUsers[1].id).toEqual(insertModels[1].id);
        expect(storedUsers[1].firstName).toEqual(insertModels[1].firstName);
        expect(storedUsers[1].lastName).toEqual('Test1');
      });
    });

    describe('destroy', () => {
      it('should be able to delete models', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        let storedModels = await connection.insert(User, insertModels);
        expect(storedModels).toBeInstanceOf(Array);
        expect(storedModels.length).toEqual(2);

        storedModels[0].firstName = 'Test1';
        storedModels[1].lastName = 'Test1';

        await connection.destroy(User, storedModels);

        // Ensure the changes were persisted
        let storedUsers = await Utils.collect(connection.select(User));
        expect(storedUsers).toBeInstanceOf(Array);
        expect(storedUsers.length).toEqual(0);
      });

      it('should be able to delete specific models', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        let storedModels = await connection.insert(User, insertModels);
        expect(storedModels).toBeInstanceOf(Array);
        expect(storedModels.length).toEqual(2);

        await connection.destroy(User, storedModels[0]);

        // Ensure the changes were persisted
        let storedUsers = await Utils.collect(connection.select(User));
        expect(storedUsers).toBeInstanceOf(Array);
        expect(storedUsers.length).toEqual(1);
        expect(storedUsers[0].id).toEqual(insertModels[1].id);
        expect(storedUsers[0].firstName).toEqual(insertModels[1].firstName);
        expect(storedUsers[0].lastName).toEqual(insertModels[1].lastName);
      });

      it('should be able to delete specific models via query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        let storedModels = await connection.insert(User, insertModels);
        expect(storedModels).toBeInstanceOf(Array);
        expect(storedModels.length).toEqual(2);

        await connection.destroy(User, User.where.lastName.EQ('Anne'));

        // Ensure the changes were persisted
        let storedUsers = await Utils.collect(connection.select(User));
        expect(storedUsers).toBeInstanceOf(Array);
        expect(storedUsers.length).toEqual(1);
        expect(storedUsers[0].id).toEqual(insertModels[0].id);
        expect(storedUsers[0].firstName).toEqual(insertModels[0].firstName);
        expect(storedUsers[0].lastName).toEqual(insertModels[0].lastName);
      });
    });

    describe('truncate', () => {
      it('should be able to truncate a table', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'admin' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'member' }) }),
        ];

        let storedModels = await connection.insert(User, insertModels);
        expect(storedModels).toBeInstanceOf(Array);
        expect(storedModels.length).toEqual(2);

        await connection.truncate(User);

        // Ensure the changes were persisted
        let storedUsers = await Utils.collect(connection.select(User));
        expect(storedUsers).toBeInstanceOf(Array);
        expect(storedUsers.length).toEqual(0);
      });
    });
  });
});
