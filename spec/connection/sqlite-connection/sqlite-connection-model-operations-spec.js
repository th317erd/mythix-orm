/* eslint-disable no-magic-numbers */

'use strict';

/* global Buffer, describe, it, expect, beforeAll, afterEach, beforeAll, spyOn, fail */

const moment  = require('moment');
const {
  XID_REGEXP,
  ISO8601_DATE_REGEXP,
} = require('../../support/test-helpers');

const Utils = require('../../../lib/utils');

const {
  sortModelNamesByCreationOrder,
} = require('../../../lib/utils/model-utils');

const {
  createConnection,
  truncateTables,
} = require('./sqlite-connection-helper');

describe('SQLiteConnection', () => {
  describe('model operations', () => {
    let connection;
    let models;

    beforeAll(async () => {
      try {
        let setup = await createConnection();

        connection = setup.connection;
        models = connection.getModels();
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

    describe('blobs', () => {
      it('can save and retrieve blobs', async () => {
        let buffer    = Buffer.from('hello world', 'utf8');
        let blobModel = new models.BlobTest({ data: buffer });

        await blobModel.save();

        expect(await models.BlobTest.count()).toEqual(1);

        let thisBlobModel = await models.BlobTest.first();
        expect(thisBlobModel).toBeInstanceOf(models.BlobTest);
        expect(thisBlobModel.data).toBeInstanceOf(Buffer);
        expect(thisBlobModel.data.toString('utf8')).toEqual('hello world');
      });
    });

    describe('timestamps', () => {
      it('will update timestamps as expected', async () => {
        let userModels = [
          new models.ExtendedUser({
            email:      'test@example.com',
            firstName:  'Mary',
            lastName:   'Anne',
          }),
        ];

        await connection.insert(models.ExtendedUser, userModels);

        let user = await models.ExtendedUser.where.first();
        expect(user.id).toEqual(1);
        expect(user.createdAt).toBeInstanceOf(moment);
        expect(user.createdAt.isValid()).toEqual(true);
        expect(user.updatedAt).toBeInstanceOf(moment);
        expect(user.updatedAt.isValid()).toEqual(true);

        let previousUpdatedAt = user.updatedAt;

        user.lastName = 'Joe';
        let updatedUser = await connection.update(models.ExtendedUser, user);
        expect(previousUpdatedAt.valueOf() < updatedUser.updatedAt.valueOf()).toEqual(true);

        // Reload stored model to ensure results
        user = await models.ExtendedUser.where.id.EQ(updatedUser.id).first();
        expect(user.updatedAt.valueOf()).toEqual(updatedUser.updatedAt.valueOf());
      });

      it('can use remote and local time', async () => {
        let timeModels = [ new models.Time() ];

        await connection.insert(models.Time, timeModels);

        let time = await models.Time.where.first();
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
          new models.ExtendedUser({
            email:      'test@example.com',
            firstName:  'Mary',
            lastName:   'Anne',
            createdAt:   '07.30.2022 22:39:01',
            updatedAt:   '07.30.2022 22:39:01',
          }),
        ];

        await connection.insert(models.ExtendedUser, userModels);

        let user = await models.ExtendedUser.where.first();
        expect(JSON.stringify(user)).toEqual('{"id":1,"createdAt":"07.30.2022 15:39:01","email":"test@example.com","firstName":"Mary","lastName":"Anne","playerType":"wizard","primaryRoleID":null,"updatedAt":"07.30.2022 15:39:01"}');
      });
    });

    describe('reload', () => {
      it('can reload a model in-place', async () => {
        let userModels = [
          new models.ExtendedUser({
            email:      'test@example.com',
            firstName:  'Mary',
            lastName:   'Anne',
          }),
        ];

        await connection.insert(models.ExtendedUser, userModels);

        let user = await models.ExtendedUser.where.first();
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

    describe('onBeforeCreate', () => {
      it('can update model fields before create', async () => {
        let validationModels = [
          new models.ValidationTest({
            number:     'not a number',
            boolean:    'bad',
            date:       '20-01-01',
          }),
          new models.ValidationTest({
            number:     'not a number',
            boolean:    'bad',
            date:       '20-01-01',
          }),
        ];

        function beforeCreate() {
          this.number = 123;
          this.boolean = false;
          this.date = '2001-01-01';
        }

        validationModels[0].onBeforeCreate = beforeCreate;
        validationModels[1].onBeforeCreate = beforeCreate;

        spyOn(validationModels[0], 'onBeforeCreate').and.callThrough();
        spyOn(validationModels[1], 'onBeforeCreate').and.callThrough();

        await connection.insert(models.ValidationTest, validationModels);

        expect(validationModels[0].onBeforeCreate.calls.count()).toEqual(1);
        expect(validationModels[1].onBeforeCreate.calls.count()).toEqual(1);

        validationModels = await Utils.collect(models.ValidationTest.all());
        expect(validationModels).toBeInstanceOf(Array);
        expect(validationModels.length).toEqual(2);
        expect(Object.assign(validationModels[0].toJSON(), { id: null })).toEqual({
          id:       null,
          number:   '123',
          boolean:  'false',
          date:     '2001-01-01',
        });
        expect(Object.assign(validationModels[1].toJSON(), { id: null })).toEqual({
          id:       null,
          number:   '123',
          boolean:  'false',
          date:     '2001-01-01',
        });
      });
    });

    describe('onBeforeUpdate', () => {
      it('can update model fields before update', async () => {
        let validationModels = [
          new models.ValidationTest({
            number:     123,
            boolean:    true,
            date:       '2000-01-01',
          }),
          new models.ValidationTest({
            number:     456,
            boolean:    false,
            date:       '2000-01-01',
          }),
        ];

        function beforeUpdate() {
          this.number = this.number + '1';
          this.boolean = (this.boolean === 'true') ? false : true;
          this.date = '2001-01-05';
        }

        // Should not be called
        validationModels[0].onBeforeUpdate = beforeUpdate;
        validationModels[1].onBeforeUpdate = beforeUpdate;

        spyOn(validationModels[0], 'onBeforeUpdate').and.callThrough();
        spyOn(validationModels[1], 'onBeforeUpdate').and.callThrough();

        await connection.insert(models.ValidationTest, validationModels);

        expect(validationModels[0].onBeforeUpdate.calls.count()).toEqual(0);
        expect(validationModels[1].onBeforeUpdate.calls.count()).toEqual(0);

        validationModels = await Utils.collect(models.ValidationTest.all());
        expect(validationModels).toBeInstanceOf(Array);
        expect(validationModels.length).toEqual(2);

        // -------------- Update -----------------

        // Should be called
        validationModels[0].onBeforeUpdate = beforeUpdate;
        validationModels[1].onBeforeUpdate = beforeUpdate;

        spyOn(validationModels[0], 'onBeforeUpdate').and.callThrough();
        spyOn(validationModels[1], 'onBeforeUpdate').and.callThrough();

        await connection.update(models.ValidationTest, validationModels);

        expect(validationModels[0].onBeforeUpdate.calls.count()).toEqual(1);
        expect(validationModels[1].onBeforeUpdate.calls.count()).toEqual(1);

        validationModels = await Utils.collect(models.ValidationTest.all());
        expect(validationModels).toBeInstanceOf(Array);
        expect(validationModels.length).toEqual(2);
        expect(Object.assign(validationModels[0].toJSON(), { id: null })).toEqual({
          id:       null,
          number:   '1231',
          boolean:  'false',
          date:     '2001-01-05',
        });
        expect(Object.assign(validationModels[1].toJSON(), { id: null })).toEqual({
          id:       null,
          number:   '4561',
          boolean:  'true',
          date:     '2001-01-05',
        });
      });
    });

    describe('onValidate', () => {
      it('can validate model fields', async () => {
        let validationModels = [
          new models.ValidationTest({
            number:     123,
            boolean:    true,
            date:       '2000-01-01',
          }),
          new models.ValidationTest({
            number:     456,
            boolean:    true,
            date:       '2022-07-31',
          }),
        ];

        spyOn(validationModels[0], 'onValidate').and.callThrough();
        spyOn(validationModels[1], 'onValidate').and.callThrough();

        await connection.insert(models.ValidationTest, validationModels);

        expect(validationModels[0].onValidate.calls.count()).toEqual(1);
        expect(validationModels[1].onValidate.calls.count()).toEqual(1);
      });

      it('can should fail validations properly #1', async () => {
        let validationModels = [
          new models.ValidationTest({
            number:     123,
            boolean:    true,
            date:       '2000-01-01',
          }),
          new models.ValidationTest({
            number:     'derp',
            boolean:    true,
            date:       '2022-07-31',
          }),
        ];

        try {
          expect(await models.ValidationTest.count()).toEqual(0);
          await connection.insert(models.ValidationTest, validationModels);
          fail('unreachable');
        } catch (error) {
          expect(error.message).toEqual('Number expected');
          expect(await models.ValidationTest.count()).toEqual(0);
        }
      });

      it('can should fail validations properly #2', async () => {
        let validationModels = [
          new models.ValidationTest({
            number:     123,
            boolean:    'hello',
            date:       '2000-01-01',
          }),
          new models.ValidationTest({
            number:     'derp',
            boolean:    true,
            date:       '2022-07-31',
          }),
        ];

        try {
          expect(await models.ValidationTest.count()).toEqual(0);
          await connection.insert(models.ValidationTest, validationModels);
          fail('unreachable');
        } catch (error) {
          expect(error.message).toEqual('Boolean expected');
          expect(await models.ValidationTest.count()).toEqual(0);
        }
      });

      it('can should fail validations properly #3', async () => {
        let validationModels = [
          new models.ValidationTest({
            number:     123,
            boolean:    'true',
            date:       '20-01-01',
          }),
          new models.ValidationTest({
            number:     1,
            boolean:    true,
            date:       '2022-07-31',
          }),
        ];

        try {
          expect(await models.ValidationTest.count()).toEqual(0);
          await connection.insert(models.ValidationTest, validationModels);
          fail('unreachable');
        } catch (error) {
          expect(error.message).toEqual('Invalid date');
          expect(await models.ValidationTest.count()).toEqual(0);
        }
      });

      it('can should skip validations if onBeforeSave is overloaded', async () => {
        let validationModels = [
          new models.ValidationTest({
            number:     'not a number',
            boolean:    'nope',
            date:       '20-01-01',
          }),
        ];

        validationModels[0].onBeforeSave = function() {};

        try {
          expect(await models.ValidationTest.count()).toEqual(0);
          await connection.insert(models.ValidationTest, validationModels);
          expect(await models.ValidationTest.count()).toEqual(1);
        } catch (error) {
          fail('unreachable');
        }
      });
    });
  });
});
