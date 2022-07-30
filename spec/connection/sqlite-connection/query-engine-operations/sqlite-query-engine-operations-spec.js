/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll, afterEach, beforeAll */

const UUID = require('uuid');
const Utils = require('../../../../lib/utils');

const {
  createConnection,
  truncateTables,
} = require('../sqlite-connection-helper');

describe('SQLiteConnection', () => {
  describe('QueryEngine operations', () => {
    let connection;
    let User;
    let Role;
    let Number;

    beforeAll(async () => {
      let setup = await createConnection();

      connection = setup.connection;
      User = setup.User;
      Role = setup.Role;
      Number = setup.Number;
    });

    afterEach(async () => {
      await truncateTables(connection);
    });

    describe('all', () => {
      it('can fetch all models from a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let users = await Utils.collect(User.where.all());
        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(2);
        expect(users[0]).toBeInstanceOf(User);
        expect(users[1]).toBeInstanceOf(User);
        expect(users[0].id).toEqual(insertModels[0].id);
        expect(users[1].id).toEqual(insertModels[1].id);

        users = await Utils.collect(User.where.firstName.EQ('Test').all());
        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(1);
        expect(users[0]).toBeInstanceOf(User);
        expect(users[0].id).toEqual(insertModels[0].id);
      });
    });

    describe('update', () => {
      it('can update models using a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        await User.where.update({ firstName: 'The', lastName: 'Thang' });

        let users = await Utils.collect(User.where.all());
        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(2);
        expect(users[0].id).toEqual(insertModels[0].id);
        expect(users[0].firstName).toEqual('The');
        expect(users[0].lastName).toEqual('Thang');
        expect(users[1].id).toEqual(insertModels[1].id);
        expect(users[1].firstName).toEqual('The');
        expect(users[1].lastName).toEqual('Thang');
      });

      it('can update specific models using a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        await User.where.firstName.EQ('Mary').update({ firstName: 'The', lastName: 'Thang' });

        let users = await Utils.collect(User.where.all());
        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(2);
        expect(users[0].id).toEqual(insertModels[0].id);
        expect(users[0].firstName).toEqual('Test');
        expect(users[0].lastName).toEqual('User');
        expect(users[1].id).toEqual(insertModels[1].id);
        expect(users[1].firstName).toEqual('The');
        expect(users[1].lastName).toEqual('Thang');
      });
    });

    describe('destroy', () => {
      it('can destroy models using a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let count = await User.where.count();
        expect(count).toEqual(2);

        await User.where.destroy({ firstName: 'The', lastName: 'Thang' });

        count = await User.where.count();
        expect(count).toEqual(0);
      });

      it('can destroy specific models using a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let count = await User.where.count();
        expect(count).toEqual(2);

        await User.where.firstName.EQ('Mary').destroy();

        let users = await Utils.collect(User.where.all());
        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(1);
        expect(users[0].id).toEqual(insertModels[0].id);
        expect(users[0].firstName).toEqual('Test');
        expect(users[0].lastName).toEqual('User');
      });
    });

    describe('exists', () => {
      it('can check if models exist', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        expect(await User.where.exists()).toEqual(true);
      });

      it('can check if models exist with a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        expect(await User.where.firstName.EQ('Mary').exists()).toEqual(true);
      });

      it('will report false when nothing matches query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        expect(await User.where.firstName.EQ('Derp').exists()).toEqual(false);
      });
    });

    describe('first', () => {
      it('can fetch first model from a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let user = await User.where.first();
        expect(user).toBeInstanceOf(User);
        expect(user.id).toEqual(insertModels[0].id);
      });

      it('can fetch first count models from a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let users = await User.where.first(10);
        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(2);
        expect(users[0]).toBeInstanceOf(User);
        expect(users[0].id).toEqual(insertModels[0].id);
        expect(users[1]).toBeInstanceOf(User);
        expect(users[1].id).toEqual(insertModels[1].id);
      });
    });

    describe('last', () => {
      it('can fetch last model from a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let user = await User.where.last();
        expect(user).toBeInstanceOf(User);
        expect(user.id).toEqual(insertModels[1].id);
      });

      it('can fetch last count models from a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let users = await User.where.last(10);
        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(2);
        expect(users[0]).toBeInstanceOf(User);
        expect(users[0].id).toEqual(insertModels[0].id);
        expect(users[1]).toBeInstanceOf(User);
        expect(users[1].id).toEqual(insertModels[1].id);
      });

      it('can fetch last count models from a query specifying an order', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let users = await User.where.ORDER('lastName').last(10);
        expect(users).toBeInstanceOf(Array);
        expect(users.length).toEqual(2);
        expect(users[0]).toBeInstanceOf(User);
        expect(users[0].id).toEqual(insertModels[1].id);
        expect(users[1]).toBeInstanceOf(User);
        expect(users[1].id).toEqual(insertModels[0].id);
      });
    });

    describe('count', () => {
      it('can count models from a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let count = await User.where.count();
        expect(count).toEqual(2);
      });

      it('can count specified fields from a query', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: null, primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let count = await User.where.count('firstName');
        expect(count).toEqual(2);

        count = await User.where.count('lastName');
        expect(count).toEqual(1);
      });
    });

    describe('average', () => {
      it('can get average of models from a query', async () => {
        let insertModels = [
          new Number({ numberInt: 10, numberFloat: 12.34 }),
          new Number({ numberInt: 11, numberFloat: 15.56 }),
          new Number({ numberInt: 12, numberFloat: 17.89 }),
          new Number({ numberInt: 15, numberFloat: 20.00 }),
        ];

        await connection.insert(Number, insertModels);

        let avg = await Number.where.average('numberInt');
        expect(avg).toEqual(12);

        avg = await Number.where.average('numberFloat');
        expect(avg).toEqual(16.447499999999998);
      });
    });

    describe('sum', () => {
      it('can get sum of models from a query', async () => {
        let insertModels = [
          new Number({ numberInt: 10, numberFloat: 12.34 }),
          new Number({ numberInt: 11, numberFloat: 15.56 }),
          new Number({ numberInt: 12, numberFloat: 17.89 }),
          new Number({ numberInt: 15, numberFloat: 20.00 }),
        ];

        await connection.insert(Number, insertModels);

        let avg = await Number.where.sum('numberInt');
        expect(avg).toEqual(48);

        avg = await Number.where.sum('numberFloat');
        expect(avg).toEqual(65.78999999999999);
      });
    });

    describe('min', () => {
      it('can get min of models from a query', async () => {
        let insertModels = [
          new Number({ numberInt: 10, numberFloat: 12.34 }),
          new Number({ numberInt: 11, numberFloat: 15.56 }),
          new Number({ numberInt: 12, numberFloat: 17.89 }),
          new Number({ numberInt: 15, numberFloat: 20.00 }),
        ];

        await connection.insert(Number, insertModels);

        let avg = await Number.where.min('numberInt');
        expect(avg).toEqual(10);

        avg = await Number.where.min('numberFloat');
        expect(avg).toEqual(12.34);
      });
    });

    describe('max', () => {
      it('can get max of models from a query', async () => {
        let insertModels = [
          new Number({ numberInt: 10, numberFloat: 12.34 }),
          new Number({ numberInt: 11, numberFloat: 15.56 }),
          new Number({ numberInt: 12, numberFloat: 17.89 }),
          new Number({ numberInt: 15, numberFloat: 20.00 }),
        ];

        await connection.insert(Number, insertModels);

        let avg = await Number.where.max('numberInt');
        expect(avg).toEqual(15);

        avg = await Number.where.max('numberFloat');
        expect(avg).toEqual(20.00);
      });
    });

    describe('pluck', () => {
      it('can pluck values from models', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let results = await User.where.ORDER('firstName').pluck('firstName');
        expect(results).toEqual([
          'Mary',
          'Test',
        ]);
      });

      it('can pluck multiple values from models', async () => {
        let insertModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRole: new Role({ name: 'test' }) }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRole: new Role({ name: 'test' }) }),
        ];

        await connection.insert(User, insertModels);

        let results = await User.where.ORDER('firstName').pluck('firstName', [ 'lastName' ]);
        expect(results).toEqual([
          [ 'Mary', 'Anne' ],
          [ 'Test', 'User' ],
        ]);
      });
    });
  });
});
