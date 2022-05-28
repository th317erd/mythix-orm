/* eslint-disable max-classes-per-file */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, spyOn */

const Nife = require('nife');
const { Model, Types, Helpers } = require('../src');

class User extends Model {
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

class ArrayFieldsModel extends Model {
  static fields = [
    {
      type:       Types.BIGINT,
      fieldName:  'id',
      allowNull:  false,
      primaryKey: true,
    },
    {
      type:       Types.STRING(64),
      fieldName:  'test',
      allowNull:  true,
      index:      true,
    },
  ];
}

class NoFieldsModel extends Model {
}

describe('Model', () => {
  const instanceAndStaticTests = (Klass, callback) => {
    callback(Klass, 'static');
    callback(new Klass(), 'instance');
  };

  describe('getModelName', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to get the models name (${type})`, () => {
        expect(target.getModelName()).toEqual('User');
      });
    });
  });

  describe('getSingularName', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to get the models name (${type})`, () => {
        expect(target.getSingularName()).toEqual('User');
      });
    });
  });

  describe('getPluralName', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to get the models name (${type})`, () => {
        expect(target.getPluralName()).toEqual('Users');
      });
    });
  });

  describe('getFields', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to get the models fields (${type})`, () => {
        let fields = target.getFields();
        expect(typeof fields).toEqual('object');
        expect(Object.keys(fields).sort()).toEqual([
          'firstName',
          'id',
          'isOver21',
          'lastName',
        ]);
      });
    });

    instanceAndStaticTests(ArrayFieldsModel, (target, type) => {
      it(`should be able to get the models fields as an array (${type})`, () => {
        let fields = target.getFields();

        expect(Array.isArray(fields)).toEqual(true);
        expect(fields.length).toEqual(2);

        expect(Nife.pluck('fieldName', fields)).toEqual([
          'id',
          'test',
        ]);
      });
    });
  });

  describe('iterateFields', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to iterate model fields (${type})`, () => {
        let fieldNames = target.iterateFields(({ field, fieldName, stop }) => {
          expect(typeof stop).toEqual('function');
          expect(field.fieldName).toEqual(fieldName);
          return fieldName;
        });

        expect(fieldNames.sort()).toEqual([
          'firstName',
          'id',
          'isOver21',
          'lastName',
        ]);
      });

      it(`should be able to stop early while iterating model fields (${type})`, () => {
        let fieldNames = target.iterateFields(({ fieldName, stop, index }) => {
          if (index > 1) {
            stop();
            return;
          }

          return fieldName;
        });

        expect(fieldNames.sort()).toEqual([
          'firstName',
          'id',
        ]);
      });

      it(`should return immediately if callback is not a function (${type})`, () => {
        expect(target.iterateFields()).toEqual([]);
        expect(target.iterateFields({})).toEqual([]);
        expect(target.iterateFields(null)).toEqual([]);
      });
    });

    instanceAndStaticTests(NoFieldsModel, (target, type) => {
      it(`should return immediately if fields are blank (${type})`, () => {
        let wasCalled = false;
        let fieldNames = target.iterateFields(() => {
          wasCalled = true;
        });

        expect(wasCalled).toEqual(false);
        expect(fieldNames).toEqual([]);
      });
    });
  });

  describe('getField', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to get a model field (${type})`, () => {
        let field = target.getField('id');
        expect(field.fieldName).toEqual('id');

        field = target.getField('firstName');
        expect(field.fieldName).toEqual('firstName');
      });

      it(`should gacefully fail to get model field (${type})`, () => {
        let field = target.getField('_unknown');
        expect(field).toBe(undefined);
      });
    });

    instanceAndStaticTests(ArrayFieldsModel, (target, type) => {
      it(`should be able to get a model field with fields defined as an array (${type})`, () => {
        let field = target.getField('id');
        expect(field.fieldName).toEqual('id');

        field = target.getField('test');
        expect(field.fieldName).toEqual('test');
      });
    });

    instanceAndStaticTests(NoFieldsModel, (target, type) => {
      it(`should gacefully fail to get model field when there are no fields (${type})`, () => {
        let field = target.getField('id');
        expect(field).toBe(undefined);
      });
    });
  });

  describe('hasField', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to check if a field exists on a model (${type})`, () => {
        expect(target.hasField('id')).toEqual(true);
        expect(target.hasField('firstName')).toEqual(true);
        expect(target.hasField('_unknown')).toEqual(false);
      });
    });

    instanceAndStaticTests(ArrayFieldsModel, (target, type) => {
      it(`should be able to check if a field exists on a model with fields as an array (${type})`, () => {
        expect(target.hasField('id')).toEqual(true);
        expect(target.hasField('test')).toEqual(true);
        expect(target.hasField('_unknown')).toEqual(false);
      });
    });

    instanceAndStaticTests(NoFieldsModel, (target, type) => {
      it(`should gacefully fail to check if a field exists on a model when there are no fields (${type})`, () => {
        expect(target.hasField('id')).toBe(false);
      });
    });
  });

  describe('getPrimaryKeyField', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to fetch the primary key field of a model (${type})`, () => {
        let field = target.getPrimaryKeyField();
        expect(field.fieldName).toEqual('id');
        expect(field.primaryKey).toEqual(true);
      });
    });
  });

  describe('getPrimaryKeyFieldName', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to fetch the primary key field name of a model (${type})`, () => {
        expect(target.getPrimaryKeyFieldName()).toEqual('id');
      });
    });
  });

  describe('getTablePrefix', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to get the table prefix for the model (${type})`, () => {
        expect(target.getTablePrefix()).toEqual('');
      });
    });
  });

  describe('getTableName', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to get the table name for the model (${type})`, () => {
        expect(target.getTableName()).toEqual('users');
      });
    });

    class OtherUser extends User {
      static getTablePrefix() {
        return 'Table_';
      }
    }

    instanceAndStaticTests(OtherUser, (target, type) => {
      it(`should be able to get the table name for the model (${type})`, () => {
        expect(target.getTableName()).toEqual('Table_otherusers');
      });
    });
  });

  describe('misc', () => {
    it('can define schema', () => {
      let user = new User();
      expect(Object.prototype.hasOwnProperty.call(user, 'id')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user, 'firstName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user, 'lastName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user, 'isOver21')).toEqual(true);
    });

    it('can get/set field values', () => {
      let user = new User();

      user.id = 1234;
      user.firstName = 'Test';
      user.lastName = 'User';
      user.isOver21 = true;

      expect(user.id).toEqual(BigInt(1234));
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
        id:         { previous: undefined, current: BigInt(1234) },
        firstName:  { previous: undefined, current: 'Test' },
        lastName:   { previous: undefined, current: 'User' },
        isOver21:   { previous: undefined, current: true },
      });
    });

    it('can set field values from constructor', () => {
      let user = new User({
        id:         1234,
        firstName:  'Test',
        lastName:   'User',
        isOver21:   true,
      });

      expect(user.id).toEqual(BigInt(1234));
      expect(user.firstName).toEqual('Test');
      expect(user.lastName).toEqual('User');
      expect(user.isOver21).toEqual(true);

      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'id')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'firstName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'lastName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'isOver21')).toEqual(true);

      expect(user._fieldData.id).toEqual(BigInt(1234));
      expect(user._fieldData.firstName).toEqual('Test');
      expect(user._fieldData.lastName).toEqual('User');
      expect(user._fieldData.isOver21).toEqual(true);

      expect(user.isDirty()).toEqual(false);
      expect(user.changes).toEqual({});
    });

    it('can cast field values from constructor', () => {
      let user = new User({
        id:         '1234',
        firstName:  'Test',
        lastName:   'User',
        isOver21:   'true',
      });

      expect(user.id).toEqual(BigInt(1234));
      expect(user.firstName).toEqual('Test');
      expect(user.lastName).toEqual('User');
      expect(user.isOver21).toEqual(true);

      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'id')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'firstName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'lastName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'isOver21')).toEqual(true);

      expect(user._fieldData.id).toEqual(BigInt(1234));
      expect(user._fieldData.firstName).toEqual('Test');
      expect(user._fieldData.lastName).toEqual('User');
      expect(user._fieldData.isOver21).toEqual(true);

      expect(user.isDirty()).toEqual(false);
      expect(user.changes).toEqual({});
    });

    it('field values from a constructor that are methods should be called automatically', () => {
      let user = new User({
        id:         () => 1234,
        firstName:  () => 'Test',
        lastName:   () => 'User',
        isOver21:   () => true,
      });

      expect(user.id).toEqual(BigInt(1234));
      expect(user.firstName).toEqual('Test');
      expect(user.lastName).toEqual('User');
      expect(user.isOver21).toEqual(true);

      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'id')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'firstName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'lastName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._fieldData, 'isOver21')).toEqual(true);

      expect(user._fieldData.id).toEqual(BigInt(1234));
      expect(user._fieldData.firstName).toEqual('Test');
      expect(user._fieldData.lastName).toEqual('User');
      expect(user._fieldData.isOver21).toEqual(true);

      expect(user.isDirty()).toEqual(false);
      expect(user.changes).toEqual({});
    });

    it('can have default values for fields', () => {
      let model = new DefaultValuesModel();
      expect(model.id).toEqual(BigInt(654321));
      expect(model.name).toEqual('derp');
      expect(model.onlyOnSave).toEqual(undefined);
    });

    it('can use getters and setters', () => {
      class TestModel extends Model {
        static fields = {
          'test': {
            type: Types.STRING(128),
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

    it('has injected methods from a virtual field', () => {
      class VirtualFieldTest extends Model {
        static fields = {
          'id': {
            type:       Types.BIGINT,
            allowNull:  false,
            primaryKey: true,
          },
          'role': {
            type:       Types.Model('User:id'),
          },
        };
      }

      let instance = new VirtualFieldTest();
      expect(instance.createRole).toBeInstanceOf(Function);
      expect(instance.getRole).toBeInstanceOf(Function);
      expect(instance.updateRole).toBeInstanceOf(Function);
      expect(instance.destroyRole).toBeInstanceOf(Function);
    });
  });

  // TODO: Need to be able to serialize/deserialize models
});
