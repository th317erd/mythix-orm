/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { ConnectionBase, Model, Types } = require('../../src');

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
  };
}

class Role extends Model {
  static fields = {
    'id': {
      type:       Types.BIGINT,
      allowNull:  false,
      primaryKey: true,
    },
    'name': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
  };
}

describe('ConnectionBase', () => {
  let connection;

  beforeEach(() => {
    connection = new ConnectionBase({
      models: [
        User,
        Role,
      ],
    });
  });

  describe('getModel', () => {
    it('should be able to get a model by name', () => {
      let model = connection.getModel('User');
      expect(model.getModelName()).toEqual('User');
    });

    it('should be able to get a model by a fully qualified name', () => {
      let model = connection.getModel('User:id');
      expect(model.getModelName()).toEqual('User');

      model = connection.getModel('User::id.field');
      expect(model.getModelName()).toEqual('User');

      model = connection.getModel('User::');
      expect(model.getModelName()).toEqual('User');
    });
  });

  describe('getField', () => {
    it('should fail if model not defined', () => {
      let field = connection.getField('firstName');
      expect(field).toBe(undefined);
    });

    it('should be able to get a field by name', () => {
      let field = connection.getField('firstName', 'User');
      expect(field.fieldName).toEqual('firstName');
      expect(field.Model.getModelName()).toEqual('User');
    });

    it('should be able to get a field by a fully qualified name', () => {
      let field = connection.getField('User:firstName');
      expect(field.fieldName).toEqual('firstName');
      expect(field.Model.getModelName()).toEqual('User');
    });
  });
});
