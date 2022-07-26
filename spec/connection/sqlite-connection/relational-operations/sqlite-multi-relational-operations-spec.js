/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, beforeAll, afterEach, beforeAll, expect */

const {
  createConnection,
  truncateTables,
} = require('../sqlite-connection-helper');

describe('SQLiteConnection', () => {
  describe('one to many, or many to many relational operations', () => {
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

    describe('create multi model', () => {
      it('can create a single model through a multi-relational field', async () => {
        let userModels = [
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: null }),
        ];

        await connection.insert(User, userModels);

        let user = await User.where.first();
        expect(await Role.count()).toBe(0);

        await user.addToRoles({ name: 'admin' });
      });
    });

    // describe('get single model', () => {
    //   it('can fetch a single model through a relational field', async () => {
    //     let roleModels = [
    //       new Role({ name: 'member', id: UUID.v4() }),
    //       new Role({ name: 'admin', id: UUID.v4() }),
    //     ];

    //     let userModels = [
    //       new User({ firstName: 'Test', lastName: 'User', primaryRoleID: roleModels[0].id }),
    //       new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: roleModels[0].id }),
    //     ];

    //     await connection.insert(Role, roleModels);
    //     await connection.insert(User, userModels);

    //     let user = await User.where.first();
    //     expect(user).toBeInstanceOf(User);

    //     let primaryRole = await user.getPrimaryRole();
    //     expect(primaryRole).toBeInstanceOf(Role);
    //     expect(primaryRole.id).toEqual(roleModels[0].id);
    //     expect(primaryRole.name).toEqual(roleModels[0].name);
    //   });
    // });

    // describe('update single model', () => {
    //   it('can update a single model through a relational field', async () => {
    //     let roleModels = [
    //       new Role({ name: 'member', id: UUID.v4() }),
    //       new Role({ name: 'admin', id: UUID.v4() }),
    //     ];

    //     let userModels = [
    //       new User({ firstName: 'Test', lastName: 'User', primaryRoleID: roleModels[0].id }),
    //       new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: roleModels[0].id }),
    //     ];

    //     await connection.insert(Role, roleModels);
    //     await connection.insert(User, userModels);

    //     let user = await User.where.first();
    //     expect(user).toBeInstanceOf(User);

    //     let result = await user.updatePrimaryRole({ name: 'bigboy' });
    //     expect(result).toEqual(true);

    //     let primaryRole = await user.getPrimaryRole();
    //     expect(primaryRole).toBeInstanceOf(Role);
    //     expect(primaryRole.id).toEqual(roleModels[0].id);
    //     expect(primaryRole.name).toEqual('bigboy');
    //   });
    // });

    // describe('destroy single model', () => {
    //   it('can destroy a single model through a relational field', async () => {
    //     let roleModels = [
    //       new Role({ name: 'member', id: UUID.v4() }),
    //       new Role({ name: 'admin', id: UUID.v4() }),
    //     ];

    //     let userModels = [
    //       new User({ firstName: 'Test', lastName: 'User', primaryRoleID: roleModels[0].id }),
    //       new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: roleModels[0].id }),
    //     ];

    //     await connection.insert(Role, roleModels);
    //     await connection.insert(User, userModels);

    //     expect(await Role.where.count()).toEqual(2);

    //     let user = await User.where.first();
    //     let result = await user.destroyPrimaryRole();
    //     expect(result).toEqual(true);

    //     expect(await Role.where.count()).toEqual(1);
    //     expect(await User.where.count()).toEqual(2);

    //     user = await User.where.first();
    //     expect(user).toBeInstanceOf(User);
    //     expect(user.id).toEqual(userModels[0].id);
    //     expect(user.primaryRoleID).toEqual(null);
    //   });
    // });
  });
});
