/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { ConnectionBase } = require('../../src');
const {
  Role,
  User,
} = require('../support/models');

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

  describe('Model', () => {
    describe('where', () => {
      it('should be able to get a query engine from where', () => {
        let User = connection.getModel('User');

        expect(User.where._getRawQuery()[0].rootModelName).toEqual('User');
        expect(User.where._getRawQuery()[0].Model.getModelName()).toEqual('User');
      });
    });

    describe('getUnscopedQueryEngine', () => {
      it('should be able to get a query engine from getUnscopedQueryEngine', () => {
        let User = connection.getModel('User');
        let user = new User();

        expect(user.getUnscopedQueryEngine()._getRawQuery()[0].rootModelName).toEqual('User');
        expect(user.getUnscopedQueryEngine()._getRawQuery()[0].Model.getModelName()).toEqual('User');
      });
    });

    describe('getQueryEngine', () => {
      it('should be able to get a query engine from getQueryEngine', () => {
        let User = connection.getModel('User');
        let user = new User();

        expect(user.getQueryEngine()._getRawQuery()[0].rootModelName).toEqual('User');
        expect(user.getQueryEngine()._getRawQuery()[0].Model.getModelName()).toEqual('User');
      });
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
