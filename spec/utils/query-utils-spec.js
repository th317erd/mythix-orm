/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Utils, ConnectionBase } = require('../../lib');

describe('Utils::query', () => {
  let connection;
  let User;
  let Role;

  beforeAll(async () => {
    try {
      connection = new ConnectionBase({
        bindModels: false,
        models:     require('../support/models'),
      });

      let models = connection.getModels();
      User = models.User;
      Role = models.Role;
    } catch (error) {
      console.error('Error in beforeAll: ', error);
    }
  });

  describe('parseFilterFieldAndOperator', function() {
    it('should be able to parse a field', function() {
      expect(Utils.parseFilterFieldAndOperator(' test ')).toEqual({
        field:    'test',
        operator: '=',
      });
    });

    it('should be able to parse field and operator', function() {
      expect(Utils.parseFilterFieldAndOperator('test=')).toEqual({
        field:    'test',
        operator: '=',
      });

      expect(Utils.parseFilterFieldAndOperator('test !=')).toEqual({
        field:    'test',
        operator: '!=',
      });

      expect(Utils.parseFilterFieldAndOperator('test>')).toEqual({
        field:    'test',
        operator: '>',
      });

      expect(Utils.parseFilterFieldAndOperator(' test >= ')).toEqual({
        field:    'test',
        operator: '>=',
      });

      expect(Utils.parseFilterFieldAndOperator('test<')).toEqual({
        field:    'test',
        operator: '<',
      });

      expect(Utils.parseFilterFieldAndOperator('test<=')).toEqual({
        field:    'test',
        operator: '<=',
      });

      expect(Utils.parseFilterFieldAndOperator('test><')).toEqual({
        field:    'test',
        operator: '><',
      });

      expect(Utils.parseFilterFieldAndOperator('test<>')).toEqual({
        field:    'test',
        operator: '<>',
      });

      expect(Utils.parseFilterFieldAndOperator('test*')).toEqual({
        field:    'test',
        operator: '*',
      });

      expect(Utils.parseFilterFieldAndOperator('test!*')).toEqual({
        field:    'test',
        operator: '!*',
      });
    });

    it('should throw an error without a field', function() {
      expect(() => Utils.parseFilterFieldAndOperator(' != ')).toThrow(new Error('generateQueryFromFilter: "field" is blank'));
    });

    it('should throw an error with a bad operator', function() {
      expect(() => Utils.parseFilterFieldAndOperator(' test !! ')).toThrow(new Error('generateQueryFromFilter: Unknown operator "!!"'));
    });
  });

  describe('generateQueryFromFilter', function() {
    it('should throw an error on a bad operator for an array value', function() {
      expect(() => Utils.generateQueryFromFilter(connection, User, { 'lastName>': [ 'derp' ] })).toThrow(new Error('Invalid array value for operator ">"'));
    });

    it('should throw an error on a bad operator for a NULL value', function() {
      expect(() => Utils.generateQueryFromFilter(connection, User, { 'firstName>': null })).toThrow(new Error('Invalid "NULL" value for operator ">"'));
    });
  });

  describe('margeFields', function() {
    it('should be able to merge fields from scratch', function() {
      let result = Utils.margeFields(User.where(connection), undefined, [ 'firstName', 'lastName' ]);
      expect(result.size).toEqual(2);
      expect(result.get('User:firstName').value).toBe(connection.getField('firstName', 'User'));
      expect(result.get('User:lastName').value).toBe(connection.getField('lastName', 'User'));
    });

    it('should be able to merge fields', function() {
      // Start from blank
      let result = Utils.margeFields(User.where(connection), undefined, [ 'firstName', 'lastName' ]);
      expect(result.size).toEqual(2);
      expect(result.get('User:firstName').value).toBe(connection.getField('firstName', 'User'));
      expect(result.get('User:lastName').value).toBe(connection.getField('lastName', 'User'));

      // Add
      result = Utils.margeFields(User.where(connection), result, [ '+', 'primaryRoleID' ]);
      expect(result.size).toEqual(3);
      expect(result.get('User:firstName').value).toBe(connection.getField('firstName', 'User'));
      expect(result.get('User:lastName').value).toBe(connection.getField('lastName', 'User'));
      expect(result.get('User:primaryRoleID').value).toBe(connection.getField('primaryRoleID', 'User'));

      // Add plus re-add
      result = Utils.margeFields(User.where(connection), result, [ '+', User.fields.firstName, '+lastName' ]);
      expect(result.size).toEqual(3);
      expect(result.get('User:firstName').value).toBe(connection.getField('firstName', 'User'));
      expect(result.get('User:lastName').value).toBe(connection.getField('lastName', 'User'));
      expect(result.get('User:primaryRoleID').value).toBe(connection.getField('primaryRoleID', 'User'));

      // Subtract
      result = Utils.margeFields(User.where(connection), result, [ '-', User.fields.primaryRoleID ]);
      expect(result.size).toEqual(2);
      expect(result.get('User:firstName').value).toBe(connection.getField('firstName', 'User'));
      expect(result.get('User:lastName').value).toBe(connection.getField('lastName', 'User'));

      // Mixed add and subtract
      result = Utils.margeFields(User.where(connection), result, [ '-firstName', '+', User.fields.primaryRoleID ]);
      expect(result.size).toEqual(2);
      expect(result.get('User:lastName').value).toBe(connection.getField('lastName', 'User'));
      expect(result.get('User:primaryRoleID').value).toBe(connection.getField('primaryRoleID', 'User'));

      // Reset with model
      result = Utils.margeFields(User.where(connection), result, [ Role ]);
      expect(result.size).toEqual(2);
      expect(result.get('Role:id').value).toBe(connection.getField('id', 'Role'));
      expect(result.get('Role:name').value).toBe(connection.getField('name', 'Role'));

      // Add with model
      result = Utils.margeFields(User.where(connection), result, [ '+User' ]);
      expect(result.size).toEqual(6);
      expect(result.get('Role:id').value).toBe(connection.getField('id', 'Role'));
      expect(result.get('Role:name').value).toBe(connection.getField('name', 'Role'));
      expect(result.get('User:id').value).toBe(connection.getField('id', 'User'));
      expect(result.get('User:firstName').value).toBe(connection.getField('firstName', 'User'));
      expect(result.get('User:lastName').value).toBe(connection.getField('lastName', 'User'));
      expect(result.get('User:primaryRoleID').value).toBe(connection.getField('primaryRoleID', 'User'));

      // Subtract field by name
      result = Utils.margeFields(User.where(connection), result, [ '-User:firstName' ]);
      expect(result.size).toEqual(5);

      // Subtract model by name
      result = Utils.margeFields(User.where(connection), result, [ '-Role' ]);
      expect(result.size).toEqual(3);
    });
  });
});
