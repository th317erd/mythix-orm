/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Types, ConnectionBase } = require('../../lib');

class CustomType extends Types.Type {
  constructor(...args) {
    super(...args);
  }
}

describe('Type', () => {
  let connection;
  let User;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
  });

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
      expect(() => CustomType.instantiateType()).toThrow(new TypeError('Type::instantiateType: "type" is required.'));
    });

    it('should construct a type if it is not constructed', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = UserIDType.instantiateType(User.fields.id.type);
      expect(UserIDType).not.toBeInstanceOf(UserIDType);
      expect(instance).toBeInstanceOf(UserIDType);
    });
  });

  describe('getField', () => {
    it('can get the types field', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = UserIDType.instantiateType(User.fields.id.type);
      expect(instance.getField()).toBe(User.fields.id);
    });
  });

  describe('getModel', () => {
    it('can get the types model', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = UserIDType.instantiateType(User.fields.id.type);
      expect(instance.getModel()).toBe(User);
    });

    it('can get the types model even if there is only a model instance', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = new UserIDType();

      instance.setModel(null);
      instance.setField(User.fields.id);

      expect(instance._Model).toBe(null);
      expect(instance.getModel()).toBe(User);
    });

    it('should fail to get model if Model and instance are empty', () => {
      const UserIDType = Types.UUIDV4Type;
      let instance = new UserIDType();

      instance.setModel(null);
      expect(instance.getModel()).toBe(undefined);

      instance.setField(User.fields.id);

      expect(instance._Model).toBe(null);
      expect(instance.getModel()).toBe(User);
    });
  });
});
