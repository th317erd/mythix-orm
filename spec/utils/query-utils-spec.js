/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Utils, ConnectionBase } = require('../../lib');

describe('Utils::query', () => {
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
    let connection;
    let User;

    beforeAll(async () => {
      try {
        connection = new ConnectionBase({
          bindModels: false,
          models:     require('../support/models'),
        });

        let models = connection.getModels();
        User = models.User;
      } catch (error) {
        console.error('Error in beforeAll: ', error);
      }
    });

    it('should throw an error on a bad operator for an array value', function() {
      expect(() => Utils.generateQueryFromFilter(connection, User, { 'lastName>': [ 'derp' ] })).toThrow(new Error('Invalid array value for operator ">"'));
    });

    it('should throw an error on a bad operator for a NULL value', function() {
      expect(() => Utils.generateQueryFromFilter(connection, User, { 'firstName>': null })).toThrow(new Error('Invalid "NULL" value for operator ">"'));
    });
  });
});
