/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll, afterEach, beforeAll */

const moment          = require('moment');
const UUID            = require('uuid');
const {
  UUID_REGEXP,
  XID_REGEXP,
  ISO8601_DATE_REGEXP,
} = require('../../../support/test-helpers');

const {
  sortModelNamesByCreationOrder,
} = require('../../../../lib/utils/model-utils');

const {
  createConnection,
  truncateTables,
} = require('../sqlite-connection-helper');

describe('SQLiteConnection', () => {
  describe('1x1 relational operations', () => {
    let connection;
    let User;
    let Role;
    let UserThing;
    let RoleThing;
    let ExtendedUser;
    let Time;

    beforeAll(async () => {
      try {
        let setup = await createConnection();

        connection = setup.connection;
        User = setup.User;
        Role = setup.Role;
        UserThing = setup.UserThing;
        RoleThing = setup.RoleThing;
        ExtendedUser = setup.ExtendedUser;
        Time = setup.Time;
      } catch (error) {
        console.error('Error in "beforeAll": ', error);
      }
    });

    afterEach(async () => {
      await truncateTables(connection);
    });

    describe('relational-type-base', () => {
      describe('sortModelNamesByCreationOrder', () => {
        it('should be able to properly sort by creation order', () => {
          let creationOrder = sortModelNamesByCreationOrder(connection, [
            'RoleThing',
            'UserThing',
            'User',
            'UserRole',
            'Role',
          ]);

          expect(creationOrder).toEqual([
            'Role',
            'RoleThing',
            'User',
            'UserRole',
            'UserThing',
          ]);
        });
      });
    });

    describe('timestamps', () => {
      it('will update timestamps as expected', async () => {
        let userModels = [
          new ExtendedUser({
            email:      'test@example.com',
            firstName:  'Mary',
            lastName:   'Anne',
          }),
        ];

        await connection.insert(ExtendedUser, userModels);

        let user = await ExtendedUser.where.first();
        expect(user.id).toEqual(1);
        expect(user.createdAt).toBeInstanceOf(moment);
        expect(user.createdAt.isValid()).toEqual(true);
        expect(user.updatedAt).toBeInstanceOf(moment);
        expect(user.updatedAt.isValid()).toEqual(true);

        let previousUpdatedAt = user.updatedAt;

        user.lastName = 'Joe';
        let updatedUser = await connection.update(ExtendedUser, user);
        expect(previousUpdatedAt.valueOf() < updatedUser.updatedAt.valueOf()).toEqual(true);

        // Reload stored model to ensure results
        user = await ExtendedUser.where.id.EQ(updatedUser.id).first();
        expect(user.updatedAt.valueOf()).toEqual(updatedUser.updatedAt.valueOf());
      });

      it('can use remote and local time', async () => {
        let timeModels = [ new Time() ];

        await connection.insert(Time, timeModels);

        let time = await Time.where.first();
        expect(time.id).toMatch(XID_REGEXP);
        expect(time.datetime).toBeInstanceOf(moment);
        expect(time.datetime.isValid()).toEqual(true);
        expect(time.datetimeLocal).toBeInstanceOf(moment);
        expect(time.datetimeLocal.isValid()).toEqual(true);
        expect(time.date).toBeInstanceOf(moment);
        expect(time.date.isValid()).toEqual(true);
        expect(time.date.toISOString()).toMatch(ISO8601_DATE_REGEXP);
        expect(time.dateLocal).toBeInstanceOf(moment);
        expect(time.dateLocal.isValid()).toEqual(true);
        expect(time.dateLocal.toISOString()).toMatch(ISO8601_DATE_REGEXP);
      });
    });

    describe('toJSON', () => {
      it('can serialize toJSON', async () => {
        let userModels = [
          new ExtendedUser({
            email:      'test@example.com',
            firstName:  'Mary',
            lastName:   'Anne',
            createdAt:   '07.30.2022 22:39:01',
            updatedAt:   '07.30.2022 22:39:01',
          }),
        ];

        await connection.insert(ExtendedUser, userModels);

        let user = await ExtendedUser.where.first();
        expect(JSON.stringify(user)).toEqual('{"id":1,"createdAt":"07.30.2022 15:39:01","email":"test@example.com","firstName":"Mary","lastName":"Anne","playerType":"wizard","primaryRoleID":null,"updatedAt":"07.30.2022 15:39:01"}');
      });
    });

    describe('reload', () => {
      it('can reload a model in-place', async () => {
        let userModels = [
          new ExtendedUser({
            email:      'test@example.com',
            firstName:  'Mary',
            lastName:   'Anne',
          }),
        ];

        await connection.insert(ExtendedUser, userModels);

        let user = await ExtendedUser.where.first();
        user.firstName = 'Test';
        user.lastName = 'User';

        expect(user.isDirty()).toEqual(true);
        expect(user.firstName).toEqual('Test');
        expect(user.lastName).toEqual('User');

        await user.reload();
        expect(user.isDirty()).toEqual(false);
        expect(user.firstName).toEqual('Mary');
        expect(user.lastName).toEqual('Anne');
      });
    });

    describe('create single-relational models', () => {
      it('can create a single model through a relational field', async () => {
        let userModels = [
          new User({ firstName: 'Mary', lastName: 'Anne' }),
        ];

        await connection.insert(User, userModels);

        let user = await User.where.first();
        let primaryRole = await user.getPrimaryRole();
        expect(primaryRole).toBe(undefined);

        expect(user.primaryRoleID).toBe(null);
        primaryRole = await user.createPrimaryRole({ name: 'admin' });
        expect(user.primaryRoleID).not.toBe(null);
        expect(primaryRole).toBeInstanceOf(Role);
        expect(primaryRole.name).toEqual('admin');

        primaryRole = await user.getPrimaryRole();
        expect(primaryRole).toBeInstanceOf(Role);
        expect(primaryRole.name).toEqual('admin');
      });

      it('can create a single model through a relational field using a through table', async () => {
        let user = await User.create({ firstName: 'Space', lastName: 'Pants' });
        expect(user).toBeInstanceOf(User);
        expect(user.id).toMatch(UUID_REGEXP);

        expect(await UserThing.count()).toEqual(0);
        expect(await RoleThing.count()).toEqual(0);

        let storedRole = await user.createUserThingRole({ name: 'admin' });
        expect(storedRole).toBeInstanceOf(Role);

        expect(await UserThing.count()).toEqual(1);
        expect(await RoleThing.count()).toEqual(1);

        let userThing = await user.getUserThing();
        expect(userThing).toBeInstanceOf(UserThing);

        let roleThing = await userThing.getRoleThing();
        expect(roleThing).toBeInstanceOf(RoleThing);

        let role = await roleThing.getRole();
        expect(role).toBeInstanceOf(Role);

        expect(userThing.userID).toEqual(user.id);
        expect(userThing.roleThingID).toEqual(roleThing.id);
        expect(roleThing.roleID).toEqual(role.id);
        expect(role.id).toEqual(storedRole.id);
      });

      it('should update a single model through a relational field using a through table when through table models already exist', async () => {
        let user = await User.create({ firstName: 'Space', lastName: 'Pants' });
        expect(user).toBeInstanceOf(User);
        expect(user.id).toMatch(UUID_REGEXP);

        let role      = await Role.create({ name: 'admin' });
        let roleThing = await RoleThing.create({ roleID: role.id });
        await UserThing.create({ userID: user.id, roleThingID: roleThing.id });

        expect(await UserThing.count()).toEqual(1);
        expect(await RoleThing.count()).toEqual(1);

        role = await user.createUserThingRole({ name: 'admin' });
        expect(role).toBeInstanceOf(Role);
        expect(role.name).toEqual('admin');
      });

      it('should update a single model through a relational field when the "update" option is true', async () => {
        let user = await User.create({ firstName: 'Space', lastName: 'Pants' });
        expect(user).toBeInstanceOf(User);
        expect(user.id).toMatch(UUID_REGEXP);

        let role      = await Role.create({ name: 'admin' });
        let roleThing = await RoleThing.create({ roleID: role.id });
        await UserThing.create({ userID: user.id, roleThingID: roleThing.id });

        expect(await UserThing.count()).toEqual(1);
        expect(await RoleThing.count()).toEqual(1);
        expect(role.name).toEqual('admin');

        let result = await user.createUserThingRole({ name: 'test' }, { update: true });
        expect(result).toBeInstanceOf(Role);
        expect(result.name).toEqual('test');
      });
    });

    describe('get single model', () => {
      it('can fetch a single model through a relational field', async () => {
        let roleModels = [
          new Role({ name: 'member', id: UUID.v4() }),
          new Role({ name: 'admin', id: UUID.v4() }),
        ];

        let userModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRoleID: roleModels[0].id }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: roleModels[0].id }),
        ];

        await connection.insert(Role, roleModels);
        await connection.insert(User, userModels);

        let user = await User.where.first();
        expect(user).toBeInstanceOf(User);

        let primaryRole = await user.getPrimaryRole();
        expect(primaryRole).toBeInstanceOf(Role);
        expect(primaryRole.id).toEqual(roleModels[0].id);
        expect(primaryRole.name).toEqual(roleModels[0].name);
      });
    });

    describe('update single model', () => {
      it('can update a single model through a relational field', async () => {
        let roleModels = [
          new Role({ name: 'member', id: UUID.v4() }),
          new Role({ name: 'admin', id: UUID.v4() }),
        ];

        let userModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRoleID: roleModels[0].id }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: roleModels[0].id }),
        ];

        await connection.insert(Role, roleModels);
        await connection.insert(User, userModels);

        let user = await User.where.first();
        expect(user).toBeInstanceOf(User);

        let result = await user.updatePrimaryRole({ name: 'bigboy' });
        expect(result).toBeInstanceOf(Role);

        let primaryRole = await user.getPrimaryRole();
        expect(primaryRole).toBeInstanceOf(Role);
        expect(primaryRole.id).toEqual(roleModels[0].id);
        expect(primaryRole.name).toEqual('bigboy');
      });
    });

    describe('destroy single model', () => {
      it('can destroy a single model through a relational field', async () => {
        let roleModels = [
          new Role({ name: 'member', id: UUID.v4() }),
          new Role({ name: 'admin', id: UUID.v4() }),
        ];

        let userModels = [
          new User({ firstName: 'Test', lastName: 'User', primaryRoleID: roleModels[0].id }),
          new User({ firstName: 'Mary', lastName: 'Anne', primaryRoleID: roleModels[0].id }),
        ];

        await connection.insert(Role, roleModels);
        await connection.insert(User, userModels);

        expect(await Role.where.count()).toEqual(2);

        let user = await User.where.first();
        let result = await user.destroyPrimaryRole();
        expect(result).toEqual(true);

        expect(await Role.where.count()).toEqual(1);
        expect(await User.where.count()).toEqual(2);

        user = await User.where.first();
        expect(user).toBeInstanceOf(User);
        expect(user.id).toEqual(userModels[0].id);
        expect(user.primaryRoleID).toEqual(null);
      });
    });
  });
});
