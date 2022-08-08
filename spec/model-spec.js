/* eslint-disable max-classes-per-file */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, spyOn, beforeAll */

const Nife = require('nife');
const { Model, Types, ConnectionBase } = require('../lib');

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
      fieldName:    'id',
      type:         Types.BIGINT,
      allowNull:    false,
      primaryKey:   true,
      defaultValue: () => {
        return 654321;
      },
    },
    'name': {
      fieldName:    'name',
      type:         Types.STRING,
      allowNull:    false,
      primaryKey:   true,
      defaultValue: 'derp',
    },
    'onlyOnSave': {
      fieldName:    'onlyOnSave',
      type:         Types.STRING,
      allowNull:    false,
      primaryKey:   true,
      defaultValue: Types.Helpers.defaultValueFlags(onSaveDefaultValue, { onStore: true }),
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
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     {
        User,
        DefaultValuesModel,
        ArrayFieldsModel,
        NoFieldsModel,
      },
    });
  });

  const instanceAndStaticTests = (Klass, callback) => {
    callback(Klass, 'static');
    callback(new Klass(), 'instance');
  };

  describe('mergeFields', () => {
    it('should be able to clone object fields', () => {
      let clonedFields = DefaultValuesModel.mergeFields();
      expect(typeof clonedFields).toEqual('object');

      let keys = Object.keys(clonedFields).sort();
      expect(keys.length).toEqual(3);
      expect(keys).toEqual([ 'id', 'name', 'onlyOnSave' ]);

      expect(clonedFields.id).not.toBe(DefaultValuesModel.fields['id']);
      expect(clonedFields.name).not.toBe(DefaultValuesModel.fields['name']);
      expect(clonedFields.onlyOnSave).not.toBe(DefaultValuesModel.fields['onlyOnSave']);

      expect(clonedFields.id.fieldName).toEqual(DefaultValuesModel.fields['id'].fieldName);
      expect(clonedFields.name.fieldName).toEqual(DefaultValuesModel.fields['name'].fieldName);
      expect(clonedFields.onlyOnSave.fieldName).toEqual(DefaultValuesModel.fields['onlyOnSave'].fieldName);
    });

    it('should be able to clone array fields', () => {
      let clonedFields = ArrayFieldsModel.mergeFields();
      expect(Array.isArray(clonedFields)).toEqual(true);

      expect(clonedFields.length).toEqual(2);
      expect(Nife.pluck('fieldName', clonedFields)).toEqual([ 'id', 'test' ]);

      expect(typeof clonedFields[0]).toEqual('object');
      expect(typeof clonedFields[1]).toEqual('object');

      expect(clonedFields[0]).not.toBe(ArrayFieldsModel.fields[0]);
      expect(clonedFields[1]).not.toBe(ArrayFieldsModel.fields[1]);

      expect(clonedFields[0].fieldName).toEqual(ArrayFieldsModel.fields[0].fieldName);
      expect(clonedFields[1].fieldName).toEqual(ArrayFieldsModel.fields[1].fieldName);
    });

    it('should be able to clone object fields adding extra fields', () => {
      let clonedFields = DefaultValuesModel.mergeFields({
        'derp': {
          fieldName:  'derp',
          type:       Types.INTEGER,
        },
      });

      expect(typeof clonedFields).toEqual('object');

      let keys = Object.keys(clonedFields).sort();
      expect(Object.keys(DefaultValuesModel.fields).length).toEqual(3);
      expect(keys.length).toEqual(4);
      expect(keys).toEqual([ 'derp', 'id', 'name', 'onlyOnSave' ]);

      expect(clonedFields.id).not.toBe(DefaultValuesModel.fields['id']);
      expect(clonedFields.name).not.toBe(DefaultValuesModel.fields['name']);
      expect(clonedFields.onlyOnSave).not.toBe(DefaultValuesModel.fields['onlyOnSave']);
      expect(typeof clonedFields.derp).toEqual('object');

      expect(clonedFields.id.fieldName).toEqual(DefaultValuesModel.fields['id'].fieldName);
      expect(clonedFields.name.fieldName).toEqual(DefaultValuesModel.fields['name'].fieldName);
      expect(clonedFields.onlyOnSave.fieldName).toEqual(DefaultValuesModel.fields['onlyOnSave'].fieldName);
      expect(clonedFields.derp.fieldName).toEqual('derp');
    });

    it('should be able to clone array fields adding extra fields', () => {
      let clonedFields = ArrayFieldsModel.mergeFields([
        {
          fieldName:  'derp',
          type:       Types.STRING(128),
        },
        {
          fieldName: 'id',
          type:       Types.INTEGER,
          allowNull:  true,
          hello:      'world',
        },
      ]);

      expect(Array.isArray(clonedFields)).toEqual(true);

      expect(clonedFields.length).toEqual(3);
      expect(Nife.pluck('fieldName', clonedFields)).toEqual([ 'id', 'test', 'derp' ]);

      expect(typeof clonedFields[0]).toEqual('object');
      expect(typeof clonedFields[1]).toEqual('object');
      expect(typeof clonedFields[2]).toEqual('object');

      expect(clonedFields[0]).not.toBe(ArrayFieldsModel.fields[0]);
      expect(clonedFields[1]).not.toBe(ArrayFieldsModel.fields[1]);

      expect(clonedFields[0].fieldName).toEqual(ArrayFieldsModel.fields[0].fieldName);
      expect(clonedFields[1].fieldName).toEqual(ArrayFieldsModel.fields[1].fieldName);
      expect(clonedFields[2].fieldName).toEqual('derp');

      expect(clonedFields[0].fieldName).toEqual('id');
      expect(clonedFields[0].hello).toEqual('world');
      expect(clonedFields[0].allowNull).toEqual(true);
    });
  });

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

  describe('getPluralModelName', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to get the models name (${type})`, () => {
        expect(target.getPluralModelName()).toEqual('Users');
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

    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to get the models fields by name (${type})`, () => {
        let fields = target.getFields([ 'id', 'firstName' ]);
        expect(fields).toBeInstanceOf(Array);
        expect(fields.map((field) => field.fieldName).sort()).toEqual([
          'firstName',
          'id',
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

    class BadTypeModel extends Model {
      static fields = {
        'name': {
          fieldName:  'name',
          allowNull:  false,
        },
      };
    }

    it('should throw an error if "type" is empty (static)', () => {
      expect(() => BadTypeModel.iterateFields(() => {})).toThrow(new Error('BadTypeModel::initializeFields: "type" not found on "BadTypeModel.name". "type" is required for all fields.'));
    });

    it('should throw an error if "type" is empty (instance)', () => {
      expect(() => new BadTypeModel()).toThrow(new Error('BadTypeModel::initializeFields: "type" not found on "BadTypeModel.name". "type" is required for all fields.'));
    });

    it('should throw an error with no "fieldName" when fields are an array', () => {
      class BadModel extends Model {
        static fields = [
          {
            type:       Types.UUIDV4,
            allowNull:  false,
            primaryKey: true,
          },
        ];
      }

      expect(() => {
        BadModel.iterateFields(() => {});
      }).toThrow(new Error('BadModel::initializeFields: "fieldName" is missing on field index 0.'));

      expect(() => {
        new BadModel();
      }).toThrow(new Error('BadModel::initializeFields: "fieldName" is missing on field index 0.'));
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

  describe('getTableName', () => {
    instanceAndStaticTests(User, (target, type) => {
      it(`should be able to get the table name for the model (${type})`, () => {
        expect(target.getTableName()).toEqual('users');
      });
    });

    class OtherUser extends User {
      static getTableName() {
        return 'tableOTHERUsers';
      }
    }

    instanceAndStaticTests(OtherUser, (target, type) => {
      it(`should be able to get the table name for the model (${type})`, () => {
        expect(target.getTableName()).toEqual('tableOTHERUsers');
      });
    });
  });

  describe('getConcreteFieldCount', () => {
    it('should fetch concrete field count', () => {
      expect(User.getConcreteFieldCount()).toEqual(4);
    });
  });

  describe('_castFieldValue', () => {
    it('should be able cast a value', () => {
      let user = new User(null, { connection });
      expect(user._castFieldValue({ type: { castToType: () => 'derp' } }, 'hello')).toEqual('derp');
    });

    it('should return value if type is empty', () => {
      let user = new User(null, { connection });
      expect(user._castFieldValue({ type: null }, null)).toBe(null);
      expect(user._castFieldValue({ type: null }, undefined)).toBe(undefined);
      expect(user._castFieldValue({ type: null }, 'derp')).toBe('derp');
    });
  });

  describe('getDataValue', () => {
    it('should be able get a value', () => {
      let user = new User({ firstName: 'Bob', lastName: 'Pickle' }, { connection });
      expect(user.getDataValue('firstName')).toEqual('Bob');
    });

    it('should be able get a dirty value', () => {
      let user = new User({ firstName: 'Bob', lastName: 'Pickle' }, { connection });
      user.clearDirty();

      expect(user.getDataValue('lastName')).toEqual('Pickle');
      expect(user._fieldData.lastName).toEqual('Pickle');
      expect(user._dirtyFieldData.lastName).toBe(undefined);

      user.lastName = 'Mustard';

      expect(user._fieldData.lastName).toEqual('Pickle');
      expect(user._dirtyFieldData.lastName).toBe('Mustard');
      expect(user.getDataValue('lastName')).toEqual('Mustard');
      expect(user.lastName).toEqual('Mustard');
      expect(user.changes).toEqual({
        lastName: { previous: 'Pickle', current: 'Mustard' },
      });
    });
  });

  describe('setDataValue', () => {
    it('should be able set a value', () => {
      let user = new User({ firstName: 'Bob', lastName: 'Pickle' }, { connection });

      user.clearDirty();
      expect(user.firstName).toEqual('Bob');

      user.setDataValue('firstName', 'Booger');

      expect(user.firstName).toEqual('Booger');
      expect(user._fieldData.firstName).toEqual('Bob');
      expect(user._dirtyFieldData.firstName).toBe('Booger');
    });

    it('should throw an error if the field does not exist', () => {
      let user = new User({ firstName: 'Bob', lastName: 'Pickle' }, { connection });
      expect(() => user.setDataValue('invalid', 'Booger')).toThrow(new Error('User::setDataValue: Unable to find field named "invalid".'));
    });

    it('should clear dirty state if field is set back to original value', () => {
      let user = new User({ firstName: 'Bob', lastName: 'Pickle' }, { connection });
      user.clearDirty();

      user.firstName = 'Booger';

      expect(user.firstName).toEqual('Booger');
      expect(user._fieldData.firstName).toEqual('Bob');
      expect(user._dirtyFieldData.firstName).toBe('Booger');

      user.firstName = 'Bob';

      expect(user.firstName).toEqual('Bob');
      expect(user._fieldData.firstName).toEqual('Bob');
      expect(user._dirtyFieldData.firstName).toBe(undefined);
    });
  });

  describe('misc', () => {
    it('can define schema', () => {
      let user = new User(null, { connection });
      expect(Object.prototype.hasOwnProperty.call(user, 'id')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user, 'firstName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user, 'lastName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user, 'isOver21')).toEqual(true);
    });

    it('can get/set field values', () => {
      let user = new User(null, { connection });

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
      let user = new User({
        id:         1234,
        firstName:  'Test',
        lastName:   'User',
        isOver21:   true,
      }, { connection });

      expect(user.id).toEqual(1234);
      expect(user.firstName).toEqual('Test');
      expect(user.lastName).toEqual('User');
      expect(user.isOver21).toEqual(true);

      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'id')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'firstName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'lastName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'isOver21')).toEqual(true);

      expect(user._dirtyFieldData.id).toEqual(1234);
      expect(user._dirtyFieldData.firstName).toEqual('Test');
      expect(user._dirtyFieldData.lastName).toEqual('User');
      expect(user._dirtyFieldData.isOver21).toEqual(true);

      expect(user.isDirty()).toEqual(true);
      expect(user.changes).toEqual({
        id:         { previous: undefined, current: 1234 },
        firstName:  { previous: undefined, current: 'Test' },
        isOver21:   { previous: undefined, current: true },
        lastName:   { previous: undefined, current: 'User' },
      });
    });

    it('can cast field values from constructor', () => {
      let user = new User({
        id:         '1234',
        firstName:  'Test',
        lastName:   'User',
        isOver21:   'true',
      }, { connection });

      expect(user.id).toEqual(1234);
      expect(user.firstName).toEqual('Test');
      expect(user.lastName).toEqual('User');
      expect(user.isOver21).toEqual(true);

      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'id')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'firstName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'lastName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'isOver21')).toEqual(true);

      expect(user._dirtyFieldData.id).toEqual(1234);
      expect(user._dirtyFieldData.firstName).toEqual('Test');
      expect(user._dirtyFieldData.lastName).toEqual('User');
      expect(user._dirtyFieldData.isOver21).toEqual(true);

      expect(user.isDirty()).toEqual(true);
      expect(user.changes).toEqual({
        id:         { previous: undefined, current: 1234 },
        firstName:  { previous: undefined, current: 'Test' },
        isOver21:   { previous: undefined, current: true },
        lastName:   { previous: undefined, current: 'User' },
      });
    });

    it('field values from a constructor that are methods should be called automatically', () => {
      let user = new User({
        id:         () => 1234,
        firstName:  () => 'Test',
        lastName:   () => 'User',
        isOver21:   () => true,
      }, { connection });

      expect(user.id).toEqual(1234);
      expect(user.firstName).toEqual('Test');
      expect(user.lastName).toEqual('User');
      expect(user.isOver21).toEqual(true);

      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'id')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'firstName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'lastName')).toEqual(true);
      expect(Object.prototype.hasOwnProperty.call(user._dirtyFieldData, 'isOver21')).toEqual(true);

      expect(user._dirtyFieldData.id).toEqual(1234);
      expect(user._dirtyFieldData.firstName).toEqual('Test');
      expect(user._dirtyFieldData.lastName).toEqual('User');
      expect(user._dirtyFieldData.isOver21).toEqual(true);

      expect(user.isDirty()).toEqual(true);
      expect(user.changes).toEqual({
        id:         { previous: undefined, current: 1234 },
        firstName:  { previous: undefined, current: 'Test' },
        isOver21:   { previous: undefined, current: true },
        lastName:   { previous: undefined, current: 'User' },
      });
    });

    it('can have default values for fields', () => {
      let model = new DefaultValuesModel(null, { connection });
      expect(model.id).toEqual(654321);
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

      let model = new TestModel(null, { connection });

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
            type:       Types.Model('Role', ({ Role }) => {
              return Role.where;
            }),
          },
        };
      }

      let instance = new VirtualFieldTest(null, { connection });
      expect(instance.createRole).toBeInstanceOf(Function);
      expect(instance.getRole).toBeInstanceOf(Function);
      expect(instance.updateRole).toBeInstanceOf(Function);
      expect(instance.destroyRole).toBeInstanceOf(Function);
    });
  });

  // TODO: Need to be able to serialize/deserialize models
});
