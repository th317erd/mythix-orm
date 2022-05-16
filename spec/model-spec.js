/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, spyOn */

const { Model, Types, Helpers } = require('../src');

class UserModel extends Model {
  static fields = {
    'id': {
      type:       Types.BIGINT,
      allowNull:  false,
      primaryKey: true,
    },
    'firstName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'lastName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'isOver21': {
      type:       Types.BOOLEAN,
      allowNull:  true,
      index:      true,
    },
  };
}

function onSaveDefaultValue() {
  return 'saved';
}

class DefaultValuesModel extends Model {
  static fields = {
    'id': {
      type:         Types.BIGINT,
      allowNull:    false,
      primaryKey:   true,
      defaultValue: () => {
        return 654321;
      },
    },
    'name': {
      type:         Types.STRING,
      allowNull:    false,
      primaryKey:   true,
      defaultValue: 'derp',
    },
    'onlyOnSave': {
      type:         Types.STRING,
      allowNull:    false,
      primaryKey:   true,
      defaultValue: Helpers.defaultValueFlags(onSaveDefaultValue, { onStore: true }),
    },
  };
}

describe('Model', () => {
  it('can define schema', () => {
    let user = new UserModel();
    expect(Object.prototype.hasOwnProperty.call(user, 'id')).toEqual(true);
    expect(Object.prototype.hasOwnProperty.call(user, 'firstName')).toEqual(true);
    expect(Object.prototype.hasOwnProperty.call(user, 'lastName')).toEqual(true);
    expect(Object.prototype.hasOwnProperty.call(user, 'isOver21')).toEqual(true);
  });

  it('can get/set field values', () => {
    let user = new UserModel();

    user.id = 1234;
    user.firstName = 'Test';
    user.lastName = 'User';
    user.isOver21 = true;

    expect(user.id).toEqual(1234);
    expect(user.firstName).toEqual('Test');
    expect(user.lastName).toEqual('User');
    expect(user.isOver21).toEqual(true);

    expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'id')).toEqual(true);
    expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'firstName')).toEqual(true);
    expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'lastName')).toEqual(true);
    expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'isOver21')).toEqual(true);

    expect(user._fieldData.id).toEqual(undefined);
    expect(user._fieldData.firstName).toEqual(undefined);
    expect(user._fieldData.lastName).toEqual(undefined);
    expect(user._fieldData.isOver21).toEqual(undefined);

    expect(user.isDirty()).toEqual(true);
    expect(user.changes).toEqual({
      id:         { previous: undefined, current: 1234 },
      firstName:  { previous: undefined, current: 'Test' },
      lastName:   { previous: undefined, current: 'User' },
      isOver21:   { previous: undefined, current: true },
    });
  });

  it('can set field values from constructor', () => {
    let user = new UserModel({
      id:         1234,
      firstName:  'Test',
      lastName:   'User',
      isOver21:   true,
    });

    expect(user.id).toEqual(1234);
    expect(user.firstName).toEqual('Test');
    expect(user.lastName).toEqual('User');
    expect(user.isOver21).toEqual(true);

    expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'id')).toEqual(true);
    expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'firstName')).toEqual(true);
    expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'lastName')).toEqual(true);
    expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'isOver21')).toEqual(true);

    expect(user._fieldData.id).toEqual(1234);
    expect(user._fieldData.firstName).toEqual('Test');
    expect(user._fieldData.lastName).toEqual('User');
    expect(user._fieldData.isOver21).toEqual(true);

    expect(user.isDirty()).toEqual(false);
    expect(user.changes).toEqual({});
  });

  it('can have default values for fields', () => {
    let model = new DefaultValuesModel();
    expect(model.id).toEqual(654321);
    expect(model.name).toEqual('derp');
    expect(model.onlyOnSave).toEqual(undefined);
  });

  it('can use getters and setters', () => {
    class TestModel extends Model {
      static fields = {
        'test': {
          get() {
            return this.getDataValue('test') || 'derp';
          },
          set({ value }) {
            this.setDataValue('test', value);
          },
        },
      };
    }

    let model = new TestModel();

    spyOn(TestModel.fields.test, 'get').and.callThrough();
    spyOn(TestModel.fields.test, 'set').and.callThrough();

    expect(model.test).toEqual('derp');
    expect(TestModel.fields.test.get).toHaveBeenCalled();
    expect(TestModel.fields.test.set).not.toHaveBeenCalled();

    model.test = 'hello';
    expect(TestModel.fields.test.set).toHaveBeenCalled();
    expect(model.test).toEqual('hello');
  });
});
