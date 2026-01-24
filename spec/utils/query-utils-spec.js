/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { ConnectionBase, Utils, QueryEngine } = require('../../lib');
const { QueryUtils } = Utils;

describe('QueryUtils', () => {
  let connection;
  let User;
  let Role;

  beforeAll(() => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
    Role = models.Role;
  });

  describe('parseFilterFieldAndOperator', () => {
    it('parses field name without operator', () => {
      let result = QueryUtils.parseFilterFieldAndOperator('firstName');
      expect(result.field).toEqual('firstName');
      expect(result.operator).toEqual('=');
    });

    it('parses field with equality operator', () => {
      let result = QueryUtils.parseFilterFieldAndOperator('firstName =');
      expect(result.field).toEqual('firstName');
      expect(result.operator).toEqual('=');
    });

    it('parses field with not-equal operator', () => {
      let result = QueryUtils.parseFilterFieldAndOperator('firstName !=');
      expect(result.field).toEqual('firstName');
      expect(result.operator).toEqual('!=');
    });

    it('parses field with greater-than operator', () => {
      let result = QueryUtils.parseFilterFieldAndOperator('age >');
      expect(result.field).toEqual('age');
      expect(result.operator).toEqual('>');
    });

    it('parses field with greater-than-or-equal operator', () => {
      let result = QueryUtils.parseFilterFieldAndOperator('age >=');
      expect(result.field).toEqual('age');
      expect(result.operator).toEqual('>=');
    });

    it('parses field with less-than operator', () => {
      let result = QueryUtils.parseFilterFieldAndOperator('age <');
      expect(result.field).toEqual('age');
      expect(result.operator).toEqual('<');
    });

    it('parses field with less-than-or-equal operator', () => {
      let result = QueryUtils.parseFilterFieldAndOperator('age <=');
      expect(result.field).toEqual('age');
      expect(result.operator).toEqual('<=');
    });

    it('parses field with between operator', () => {
      let result = QueryUtils.parseFilterFieldAndOperator('age ><');
      expect(result.field).toEqual('age');
      expect(result.operator).toEqual('><');
    });

    it('parses field with not-between operator', () => {
      let result = QueryUtils.parseFilterFieldAndOperator('age <>');
      expect(result.field).toEqual('age');
      expect(result.operator).toEqual('<>');
    });

    it('parses field with LIKE operator', () => {
      let result = QueryUtils.parseFilterFieldAndOperator('firstName *');
      expect(result.field).toEqual('firstName');
      expect(result.operator).toEqual('*');
    });

    it('parses field with NOT LIKE operator', () => {
      let result = QueryUtils.parseFilterFieldAndOperator('firstName !*');
      expect(result.field).toEqual('firstName');
      expect(result.operator).toEqual('!*');
    });

    it('throws error for blank field', () => {
      expect(() => QueryUtils.parseFilterFieldAndOperator('')).toThrow();
      expect(() => QueryUtils.parseFilterFieldAndOperator('   ')).toThrow();
    });

    it('throws error for unknown operator', () => {
      expect(() => QueryUtils.parseFilterFieldAndOperator('field %%')).toThrow();
    });
  });

  describe('generateQueryFromFilter', () => {
    it('generates query from simple filter object', () => {
      let query = QueryUtils.generateQueryFromFilter(connection, User, { firstName: 'John' });
      expect(QueryEngine.isQuery(query)).toBe(true);
      let context = query.getOperationContext();
      expect(context.hasCondition).toBe(true);
    });

    it('generates query with multiple AND conditions', () => {
      let query = QueryUtils.generateQueryFromFilter(connection, User, {
        firstName: 'John',
        lastName: 'Doe',
      });
      expect(QueryEngine.isQuery(query)).toBe(true);
    });

    it('generates query with OR conditions from array', () => {
      let query = QueryUtils.generateQueryFromFilter(connection, User, [
        { firstName: 'John' },
        { firstName: 'Jane' },
      ]);
      expect(QueryEngine.isQuery(query)).toBe(true);
    });

    it('handles equality operator', () => {
      let query = QueryUtils.generateQueryFromFilter(connection, User, { 'firstName =': 'John' });
      expect(QueryEngine.isQuery(query)).toBe(true);
    });

    it('handles not-equal operator', () => {
      let query = QueryUtils.generateQueryFromFilter(connection, User, { 'firstName !=': 'John' });
      expect(QueryEngine.isQuery(query)).toBe(true);
    });

    it('handles LIKE operator', () => {
      let query = QueryUtils.generateQueryFromFilter(connection, User, { 'firstName *': '%John%' });
      expect(QueryEngine.isQuery(query)).toBe(true);
    });

    it('handles NOT LIKE operator', () => {
      let query = QueryUtils.generateQueryFromFilter(connection, User, { 'firstName !*': '%John%' });
      expect(QueryEngine.isQuery(query)).toBe(true);
    });

    it('handles array value for IN operator', () => {
      let query = QueryUtils.generateQueryFromFilter(connection, User, {
        firstName: [ 'John', 'Jane', 'Bob' ],
      });
      expect(QueryEngine.isQuery(query)).toBe(true);
    });

    it('handles null value', () => {
      let query = QueryUtils.generateQueryFromFilter(connection, User, { firstName: null });
      expect(QueryEngine.isQuery(query)).toBe(true);
    });

    it('skips undefined values', () => {
      let query = QueryUtils.generateQueryFromFilter(connection, User, {
        firstName: 'John',
        lastName: undefined,
      });
      expect(QueryEngine.isQuery(query)).toBe(true);
    });

    it('skips virtual fields', () => {
      let query = QueryUtils.generateQueryFromFilter(connection, User, {
        firstName: 'John',
        primaryRole: { name: 'admin' },
      });
      expect(QueryEngine.isQuery(query)).toBe(true);
    });

    it('returns undefined for non-object filter', () => {
      let result = QueryUtils.generateQueryFromFilter(connection, User, 'invalid');
      expect(result).toBeUndefined();
    });

    it('handles model instance as filter', () => {
      let user = new User({ firstName: 'John' }, { connection });
      // Need to give it a primary key
      user.id = 'a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11';

      let query = QueryUtils.generateQueryFromFilter(connection, User, user);
      expect(QueryEngine.isQuery(query)).toBe(true);
    });

    it('throws error for invalid array operator usage', () => {
      expect(() => {
        QueryUtils.generateQueryFromFilter(connection, User, { 'firstName >': [ 'a', 'b' ] });
      }).toThrow();
    });

    it('throws error for null with invalid operator', () => {
      expect(() => {
        QueryUtils.generateQueryFromFilter(connection, User, { 'firstName >': null });
      }).toThrow();
    });
  });

  describe('mergeFields', () => {
    it('replaces fields by default (no prefix)', () => {
      let query = User.where(connection);
      let currentFields = new Map([[ 'User:id', { value: User.fields.id } ]]);
      let result = QueryUtils.mergeFields(query, currentFields, [ 'firstName' ]);

      expect(result.has('User:firstName')).toBe(true);
      expect(result.has('User:id')).toBe(false);
    });

    it('adds fields with + prefix', () => {
      let query = User.where(connection);
      let currentFields = new Map([[ 'User:id', { value: User.fields.id } ]]);
      let result = QueryUtils.mergeFields(query, currentFields, [ '+firstName' ]);

      expect(result.has('User:firstName')).toBe(true);
      expect(result.has('User:id')).toBe(true);
    });

    it('removes fields with - prefix', () => {
      let query = User.where(connection);
      let currentFields = new Map([
        [ 'User:id', { value: User.fields.id } ],
        [ 'User:firstName', { value: User.fields.firstName } ],
      ]);
      let result = QueryUtils.mergeFields(query, currentFields, [ '-id' ]);

      expect(result.has('User:firstName')).toBe(true);
      expect(result.has('User:id')).toBe(false);
    });

    it('handles * wildcard for all fields', () => {
      let query = User.where(connection);
      let result = QueryUtils.mergeFields(query, new Map(), [ '*' ]);

      expect(result.size).toBeGreaterThan(0);
      expect(result.has('User:id')).toBe(true);
      expect(result.has('User:firstName')).toBe(true);
    });

    it('handles @ prefix for literals', () => {
      let query = User.where(connection);
      let result = QueryUtils.mergeFields(query, new Map(), [ '@COUNT(*) AS total' ]);

      expect(result.size).toBe(1);
      expect(result.has('COUNT(*) AS total')).toBe(true);
    });

    it('handles Field instance', () => {
      let query = User.where(connection);
      let result = QueryUtils.mergeFields(query, new Map(), [ User.fields.firstName ]);

      expect(result.has('User:firstName')).toBe(true);
    });

    it('handles Model class (adds all fields)', () => {
      let query = User.where(connection);
      let result = QueryUtils.mergeFields(query, new Map(), [ User ]);

      expect(result.has('User:id')).toBe(true);
      expect(result.has('User:firstName')).toBe(true);
      expect(result.has('User:lastName')).toBe(true);
    });

    it('returns empty map for empty incoming fields', () => {
      let query = User.where(connection);
      let result = QueryUtils.mergeFields(query, new Map(), []);
      expect(result.size).toBe(0);
    });

    it('handles fully qualified field names', () => {
      let query = User.where(connection);
      let result = QueryUtils.mergeFields(query, new Map(), [ 'User:firstName' ]);

      expect(result.has('User:firstName')).toBe(true);
    });

    it('throws for unknown model name', () => {
      let query = User.where(connection);
      expect(() => {
        QueryUtils.mergeFields(query, new Map(), [ 'UnknownModel:field' ]);
      }).toThrow();
    });

    it('throws for unknown field name', () => {
      let query = User.where(connection);
      expect(() => {
        QueryUtils.mergeFields(query, new Map(), [ 'User:nonexistentField' ]);
      }).toThrow();
    });
  });
});
