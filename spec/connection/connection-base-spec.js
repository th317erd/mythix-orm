/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll, fail, spyOn */

const { Model, ConnectionBase, Types } = require('../../lib');
const { User } = require('../support/models');

class TestModel extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'firstName': {
      type:         Types.STRING(64),
      allowNull:    true,
      index:        true,
    },
    'lastName': {
      type:         Types.STRING(64),
      allowNull:    true,
      index:        true,
    },
  };
}

describe('ConnectionBase', () => {
  let connection;

  beforeAll(() => {
    try {
      connection = new ConnectionBase({ models: [ TestModel ] });
    } catch (error) {
      console.error('Error in BeforeAll: ', error);
    }
  });

  describe('Model', () => {
    describe('where', () => {
      it('should be able to get a query engine from where', () => {
        expect(User.where(connection)._getRawQuery()[0].rootModelName).toEqual('User');
        expect(User.where(connection)._getRawQuery()[0].Model.getModelName()).toEqual('User');
      });
    });

    describe('getUnscopedQueryEngine', () => {
      it('should be able to get a query engine from getUnscopedQueryEngine', () => {
        let user = new TestModel();

        expect(user.getUnscopedQueryEngine()._getRawQuery()[0].rootModelName).toEqual('TestModel');
        expect(user.getUnscopedQueryEngine()._getRawQuery()[0].Model.getModelName()).toEqual('TestModel');
      });
    });

    describe('getQueryEngine', () => {
      it('should be able to get a query engine from getQueryEngine', () => {
        let user = new TestModel();

        expect(user.getQueryEngine()._getRawQuery()[0].rootModelName).toEqual('TestModel');
        expect(user.getQueryEngine()._getRawQuery()[0].Model.getModelName()).toEqual('TestModel');
      });
    });
  });

  describe('getModel', () => {
    it('should be able to get a model by name', () => {
      let model = connection.getModel('TestModel');
      expect(model.getModelName()).toEqual('TestModel');
    });

    it('should be able to get a model by a fully qualified name', () => {
      let model = connection.getModel('TestModel:id');
      expect(model.getModelName()).toEqual('TestModel');

      model = connection.getModel('TestModel::id.field');
      expect(model.getModelName()).toEqual('TestModel');

      model = connection.getModel('TestModel::');
      expect(model.getModelName()).toEqual('TestModel');
    });
  });

  describe('getField', () => {
    it('should fail if model not defined', () => {
      let field = connection.getField('firstName');
      expect(field).toBe(undefined);
    });

    it('should be able to get a field by name', () => {
      let field = connection.getField('firstName', 'TestModel');
      expect(field.fieldName).toEqual('firstName');
      expect(field.Model.getModelName()).toEqual('TestModel');
    });

    it('should be able to get a field by a fully qualified name', () => {
      let field = connection.getField('TestModel:firstName');
      expect(field.fieldName).toEqual('firstName');
      expect(field.Model.getModelName()).toEqual('TestModel');
    });
  });

  describe('start', () => {
    it('should throw an unimplemented error', async () => {
      try {
        await connection.start();
        fail('unreachable');
      } catch (error) {
        expect(error).toBeInstanceOf(Error);
        expect(error.message).toEqual('ConnectionBase::start: Child class is required to implement "start".');
      }
    });
  });

  describe('stop', () => {
    it('should unbind all event listeners', async () => {
      spyOn(connection, 'removeAllListeners').and.callThrough();
      await connection.stop();

      expect(connection.removeAllListeners.calls.count()).toEqual(1);
    });
  });
});
