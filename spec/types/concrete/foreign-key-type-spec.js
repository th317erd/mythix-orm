/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Types, ConnectionBase } = require('../../../lib');

describe('ForeignKeyType', () => {
  let connection;
  let User;
  let Role;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
    Role = models.Role;
  });

  describe('getDisplayName', () => {
    it('returns FOREIGN_KEY', () => {
      expect(Types.ForeignKeyType.getDisplayName()).toEqual('FOREIGN_KEY');
    });
  });

  describe('isForeignKey', () => {
    it('returns true for ForeignKeyType class', () => {
      expect(Types.ForeignKeyType.isForeignKey()).toBe(true);
    });

    it('returns true for ForeignKeyType instance', () => {
      let type = new Types.ForeignKeyType('Role:id');
      expect(type.isForeignKey()).toBe(true);
    });
  });

  describe('constructor', () => {
    it('can construct with fully qualified field name', () => {
      let type = new Types.ForeignKeyType('Role:id');
      expect(type.fullyQualifiedName).toEqual('Role:id');
    });

    it('can construct with options object', () => {
      let type = new Types.ForeignKeyType({
        modelName: 'Role',
        fieldName: 'id',
      });
      expect(type.options.modelName).toEqual('Role');
      expect(type.options.fieldName).toEqual('id');
    });

    it('can construct with fully qualified name and options', () => {
      let type = new Types.ForeignKeyType('Role:id', {
        onDelete: 'CASCADE',
        onUpdate: 'SET NULL',
      });
      expect(type.options.onDelete).toEqual('CASCADE');
      expect(type.options.onUpdate).toEqual('SET NULL');
    });

    it('throws error when no arguments provided', () => {
      expect(() => new Types.ForeignKeyType()).toThrow();
    });

    it('throws error when model name is missing with two arguments', () => {
      // Note: validation only happens with 2 arguments
      expect(() => new Types.ForeignKeyType(':id', {})).toThrow();
    });
  });

  describe('getOptions', () => {
    it('returns options object', () => {
      let type = new Types.ForeignKeyType('Role:id', { onDelete: 'CASCADE' });
      let options = type.getOptions();
      expect(options.onDelete).toEqual('CASCADE');
    });
  });

  describe('getTargetModel', () => {
    it('returns target model when connection provided', () => {
      let type = new Types.ForeignKeyType('Role:id');
      // Need to initialize first
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      let targetModel = type.getTargetModel(connection);
      expect(targetModel).toBe(Role);
    });
  });

  describe('getTargetModelName', () => {
    it('returns target model name', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      let name = type.getTargetModelName(connection);
      expect(name).toEqual('Role');
    });
  });

  describe('getTargetField', () => {
    it('returns target field when connection provided', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      let targetField = type.getTargetField(connection);
      expect(targetField).toBeDefined();
      expect(targetField.fieldName).toEqual('id');
    });
  });

  describe('getTargetFieldName', () => {
    it('returns target field name', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      let name = type.getTargetFieldName(connection);
      expect(name).toEqual('id');
    });

    it('returns undefined if target field not found', () => {
      let type = new Types.ForeignKeyType('Role:id');
      // Don't initialize - no connection
      let name = type.getTargetFieldName();
      expect(name).toBeUndefined();
    });
  });

  describe('castToType', () => {
    it('returns null unchanged', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      let result = type.castToType({ value: null, connection });
      expect(result).toBeNull();
    });

    it('returns undefined unchanged', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      let result = type.castToType({ value: undefined, connection });
      expect(result).toBeUndefined();
    });

    it('casts value to target field type', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      // Role:id is a UUID field, so it should validate/cast UUIDs
      let validUUID = 'a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11';
      let result = type.castToType({ value: validUUID, connection });
      expect(result).toEqual(validUUID);
    });

    it('throws error if target field not defined', () => {
      let type = new Types.ForeignKeyType('Role:id');
      // Don't set field/model and don't provide connection

      expect(() => {
        type.castToType({ value: 'test' });
      }).toThrow();
    });
  });

  describe('isValidValue', () => {
    it('delegates to target field type', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);
      type.initialize(connection);

      let validUUID = 'a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11';
      expect(type.isValidValue(validUUID, { connection })).toBe(true);
    });

    it('throws error if target field not defined', () => {
      let type = new Types.ForeignKeyType('Role:id');

      expect(() => {
        type.isValidValue('test');
      }).toThrow();
    });
  });

  describe('toConnectionType', () => {
    it('returns target field connection type', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      let connectionType = type.toConnectionType(connection);
      expect(typeof connectionType).toBe('string');
      expect(connectionType.length).toBeGreaterThan(0);
    });

    it('returns empty string if target field not found', () => {
      let type = new Types.ForeignKeyType('Role:id');
      let connectionType = type.toConnectionType();
      expect(connectionType).toEqual('');
    });
  });

  describe('toString', () => {
    it('returns representation without connection', () => {
      let type = new Types.ForeignKeyType('Role:id');
      let str = type.toString();
      expect(str).toEqual('ForeignKeyType {}');
    });

    it('returns target field type with connection', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      let str = type.toString(connection);
      expect(typeof str).toBe('string');
      expect(str.length).toBeGreaterThan(0);
    });
  });

  describe('initialize', () => {
    it('caches target model and field', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      type.initialize(connection);

      expect(type.targetModel).toBe(Role);
      expect(type.targetField).toBeDefined();
      expect(type.targetField.fieldName).toEqual('id');
    });

    it('only initializes once', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      type.initialize(connection);
      let firstTarget = type.targetModel;

      type.initialize(connection);
      expect(type.targetModel).toBe(firstTarget);
    });

    it('throws error without connection', () => {
      let type = new Types.ForeignKeyType('Role:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      expect(() => {
        type.initialize();
      }).toThrow();
    });
  });

  describe('parseOptionsAndCheckForErrors', () => {
    it('throws error when target field not found', () => {
      let type = new Types.ForeignKeyType('Role:nonexistent');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      expect(() => {
        type.parseOptionsAndCheckForErrors(User, User.fields.primaryRoleID, connection);
      }).toThrow();
    });

    it('throws error when target model not found', () => {
      let type = new Types.ForeignKeyType('NonexistentModel:id');
      type.setField(User.fields.primaryRoleID);
      type.setModel(User);

      expect(() => {
        type.parseOptionsAndCheckForErrors(User, User.fields.primaryRoleID, connection);
      }).toThrow();
    });
  });

  describe('type helper', () => {
    it('FOREIGN_KEY helper creates instance', () => {
      let type = Types.FOREIGN_KEY('Role:id');
      expect(type).toBeInstanceOf(Types.ForeignKeyType);
    });

    it('FOREIGN_KEY helper accepts options', () => {
      let type = Types.FOREIGN_KEY('Role:id', { onDelete: 'CASCADE' });
      expect(type.options.onDelete).toEqual('CASCADE');
    });

    it('FOREIGN_KEY helper accepts options-only argument', () => {
      let type = Types.FOREIGN_KEY({ modelName: 'Role', fieldName: 'id' });
      expect(type.options.modelName).toEqual('Role');
      expect(type.options.fieldName).toEqual('id');
    });
  });
});
