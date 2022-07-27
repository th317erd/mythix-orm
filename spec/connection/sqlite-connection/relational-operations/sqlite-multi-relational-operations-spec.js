/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, beforeAll, afterEach, beforeAll, expect */

const {
  createConnection,
  truncateTables,
} = require('../sqlite-connection-helper');

const Utils = require('../../../../src/utils');

describe('SQLiteConnection', () => {
  describe('one to many, or many to many relational operations', () => {
    let connection;
    let User;
    let Role;
    let UserRole;

    beforeAll(async () => {
      let setup = await createConnection();

      connection = setup.connection;
      User = setup.User;
      Role = setup.Role;
      UserRole = setup.UserRole;
    });

    afterEach(async () => {
      await truncateTables(connection);
    });

    describe('create multi-relational models', () => {
      it('can create a single model through a multi-relational field', async () => {
        let userModels = [
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: null }),
        ];

        await connection.insert(User, userModels);

        let user = await User.where.first();

        expect(await Role.count()).toBe(0);
        expect(await UserRole.count()).toBe(0);

        let roles = await user.addToRoles({ name: 'admin' });
        expect(roles).toBeInstanceOf(Array);
        expect(roles.length).toEqual(1);
        expect(roles[0]).toBeInstanceOf(Role);
        expect(roles[0].isPersisted()).toEqual(true);
        expect(roles[0].name).toEqual('admin');
        expect(roles[0]._.Roles).toBe(undefined);
        expect(roles[0]._.Users).toBeInstanceOf(Array);
        expect(roles[0]._.Users[0]).toBeInstanceOf(User);
        expect(roles[0]._.Users[0]).toBe(user);
        expect(roles[0]._.UserRoles).toBeInstanceOf(Array);
        expect(roles[0]._.UserRoles[0]).toBeInstanceOf(UserRole);
        expect(roles[0]._.UserRoles[0].isPersisted()).toEqual(true);
        expect(roles[0]._.UserRoles[0].userID).toEqual(user.id);
        expect(roles[0]._.UserRoles[0].roleID).toEqual(roles[0].id);

        expect(await Role.count()).toBe(1);
        expect(await UserRole.count()).toBe(1);

        expect(user.roles).toBe(roles);
      });

      it('can create multiple models through a multi-relational field', async () => {
        let userModels = [
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: null }),
        ];

        await connection.insert(User, userModels);

        let user = await User.where.first();

        expect(await Role.count()).toBe(0);
        expect(await UserRole.count()).toBe(0);

        let persistedRole = await connection.insert(Role, { name: 'test' });
        expect(persistedRole.isPersisted()).toEqual(true);

        let roles = await user.addToRoles([
          { name: 'admin' },
          persistedRole,
          { name: 'test2' },
        ]);

        expect(roles).toBeInstanceOf(Array);
        expect(roles.length).toEqual(3);
        expect(roles[0].name).toEqual('admin');
        expect(roles[1].name).toEqual('test');
        expect(roles[2].name).toEqual('test2');

        for (let i = 0, il = roles.length; i < il; i++) {
          expect(roles[i]).toBeInstanceOf(Role);
          expect(roles[i].isPersisted()).toEqual(true);
          expect(roles[i]._.UserRoles).toBeInstanceOf(Array);
          expect(roles[i]._.UserRoles[0]).toBeInstanceOf(UserRole);
          expect(roles[i]._.UserRoles[0].isPersisted()).toEqual(true);
          expect(roles[i]._.UserRoles[0].userID).toEqual(user.id);
          expect(roles[i]._.UserRoles[0].roleID).toEqual(roles[i].id);
        }

        expect(await Role.count()).toBe(3);
        expect(await UserRole.count()).toBe(3);

        expect(user.roles).toBe(roles);
      });
    });

    describe('get multi-relational models', () => {
      it('can fetch multiple models through a relational field', async () => {
        let userModels = [
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: null }),
        ];

        await connection.insert(User, userModels);

        let user = await User.where.first();
        let persistedRole = await connection.insert(Role, { name: 'test' });

        await user.addToRoles([
          { name: 'admin' },
          persistedRole,
          { name: 'test2' },
        ]);

        let roles = await Utils.collect(user.getRoles(Role.where.ORDER('Role:name')));

        expect(roles).toBeInstanceOf(Array);
        expect(roles.length).toEqual(3);
        expect(roles[0].name).toEqual('admin');
        expect(roles[1].name).toEqual('test');
        expect(roles[2].name).toEqual('test2');

        for (let i = 0, il = roles.length; i < il; i++) {
          expect(roles[i]).toBeInstanceOf(Role);
          expect(roles[i].isPersisted()).toEqual(true);
          expect(roles[i]._.UserRoles).toBeInstanceOf(Array);
          expect(roles[i]._.UserRoles[0]).toBeInstanceOf(UserRole);
          expect(roles[i]._.UserRoles[0].isPersisted()).toEqual(true);
          expect(roles[i]._.UserRoles[0].userID).toEqual(user.id);
          expect(roles[i]._.UserRoles[0].roleID).toEqual(roles[i].id);
        }

        expect(await Role.count()).toBe(3);
        expect(await UserRole.count()).toBe(3);

        expect(user.roles).toEqual(roles);
      });
    });

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
