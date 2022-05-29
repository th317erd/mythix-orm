/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, spyOn */

const { Types } = require('../../src');
const {
  User,
} = require('../support/models');

class CustomType extends Types.Type {
  constructor(...args) {
    super(...args);
  }
}

describe('Type', () => {
  it('will store arguments', () => {
    let type = new CustomType('test', 123, true);
    expect(type._args).toEqual([ 'test', 123, true ]);
  });

  describe('clone', () => {
    it('can clone (static)', () => {
      expect(CustomType.clone()).toBe(CustomType);
    });

    it('can clone (instance)', () => {
      let instance = new CustomType('hello', 'world');
      let clone = instance.clone();

      expect(clone).not.toBe(instance);
      expect(instance._args).toEqual(clone._args);
      expect(clone).toBeInstanceOf(CustomType);
    });
  });

  describe('instantiateType', () => {
    it('will throw an error if type is empty', () => {
      expect(() => CustomType.instantiateType()).toThrow(new TypeError('Type::instantiateType: Provided field "type" is empty, but "type" is required.'));
    });

    it('should construct a type if it is not constructed', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = UserIDType.instantiateType(User, new User(), User.fields.id, UserIDType);
      expect(UserIDType).not.toBeInstanceOf(UserIDType);
      expect(instance).toBeInstanceOf(UserIDType);
    });
  });

  describe('getField', () => {
    it('can get the types field', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = UserIDType.instantiateType(User, new User(), User.fields.id, UserIDType);
      expect(instance.getField()).toBe(User.fields.id);
    });
  });

  describe('getModel', () => {
    it('can get the types model', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = UserIDType.instantiateType(User, new User(), User.fields.id, UserIDType);
      expect(instance.getModel()).toBe(User);
    });

    it('can get the types model even if there is only a model instance', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = new UserIDType();

      instance.setModel(null);
      instance.setModelInstance(new User());
      instance.setField(User.fields.id);

      expect(instance._Model).toBe(null);
      expect(instance.getModel()).toBe(User);
    });

    it('should fail to get model if Modle and instance are empty', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = new UserIDType();

      instance.setModel(null);
      instance.setModelInstance(null);
      instance.setField(User.fields.id);

      expect(instance._Model).toBe(null);
      expect(instance.getModel()).toBe(undefined);
    });
  });

  describe('getConnection', () => {
    it('should fail without Model or instance', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = new UserIDType();

      instance.setModel(null);
      instance.setModelInstance(null);
      instance.setField(User.fields.id);

      expect(instance.getConnection()).toBe(null);
    });

    it('should succeed if not instance but has Model', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = new UserIDType();

      spyOn(User, 'getConnection').and.callFake(() => {
        return 'derp';
      });

      instance.setModel(User);
      instance.setModelInstance(null);
      instance.setField(User.fields.id);

      expect(instance.getConnection()).toBe('derp');
    });

    it('should succeed if has instance but no Model', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = new UserIDType();

      let userInstance = { getConnection: () => {} };
      spyOn(userInstance, 'getConnection').and.callFake(() => {
        return 'derp';
      });

      instance.setModel(null);
      instance.setModelInstance(userInstance);
      instance.setField(User.fields.id);

      expect(instance.getConnection()).toBe('derp');
    });
  });

  describe('initialize', () => {
    it('should fail if invalid Model provided', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = new UserIDType();

      expect(() => instance.initialize(null, {}, User.fields.id)).toThrow(new TypeError('UUIDV4Type::initialize: "Model" is required.'));
    });

    it('should fail if invalid model instance provided', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = new UserIDType();

      expect(() => instance.initialize(User, null, User.fields.id)).toThrow(new TypeError('UUIDV4Type::initialize: "modelInstance" is required.'));
    });

    it('should fail if invalid field provided', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = new UserIDType();

      expect(() => instance.initialize(User, new User(), null)).toThrow(new TypeError('UUIDV4Type::initialize: "field" is required.'));
    });
  });
});
