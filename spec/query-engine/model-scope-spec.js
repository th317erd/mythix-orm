/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, expect, beforeAll */

const { ConnectionBase, QueryEngine } = require('../../lib');
const { DistinctLiteral, CountLiteral, Literal } = require('../../lib/connection/literals');
const { createRunners } = require('../support/test-helpers');
const AllModels = require('../support/models');

describe('ModelScope', () => {
  let connection;
  let User;
  let UserRole;

  // eslint-disable-next-line no-unused-vars
  const { it, fit } = createRunners(() => connection);

  beforeAll(() => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     AllModels,
    });

    let models = connection.getModels();
    User = models.User;
    UserRole = models.UserRole;
  });

  describe('Field()', () => {
    it('returns field scope for valid field name', () => {
      let scope = User.where.Field('id');
      expect(scope).toBeDefined();
      let context = scope.getOperationContext();
      expect(context.fieldName).toEqual('id');
    });

    it('throws error for invalid field name', () => {
      expect(() => User.where.Field('nonexistent')).toThrow();
    });

    it('can be used for fields with reserved names', () => {
      let scope = User.where.Field('id').EQ('test');
      expect(scope).toBeDefined();
    });
  });

  describe('NOT', () => {
    it('inverts the following condition', () => {
      let context = User.where.NOT.id.EQ('test').getOperationContext();
      expect(context.not).toBe(true);
    });

    it('is single-use toggle (resets after condition)', () => {
      let stack = User.where.NOT.id.EQ('test').firstName.EQ('John').getOperationStack();
      let notOps = stack.filter((op) => op.operator === 'NOT');
      expect(notOps.length).toBe(1);
    });

    it('can be chained without calling', () => {
      let context = User.where.NOT.id.EQ('test').getOperationContext();
      expect(context.not).toBe(true);
    });
  });

  describe('AND', () => {
    it('sets AND logical mode', () => {
      let context = User.where.id.EQ('1').AND.firstName.EQ('John').getOperationContext();
      expect(context.and).toBe(true);
      expect(context.or).toBe(false);
    });

    it('is permanent toggle', () => {
      let stack = User.where.id.EQ('1').OR.firstName.EQ('John').AND.lastName.EQ('Smith').getOperationStack();
      let lastAnd = stack.filter((op) => op.operator === 'AND').pop();
      expect(lastAnd.and).toBe(true);
    });

    it('can group conditions with query argument', () => {
      let subQuery = User.where.lastName.EQ('Smith').OR.lastName.EQ('Jones');
      let query = User.where.id.EQ('test').AND(subQuery);
      let stack = query.getOperationStack();
      let andOp = stack.find((op) => op.operator === 'AND' && op.value);
      expect(andOp).toBeDefined();
      expect(QueryEngine.isQuery(andOp.value)).toBe(true);
    });
  });

  describe('OR', () => {
    it('sets OR logical mode', () => {
      let context = User.where.id.EQ('1').OR.firstName.EQ('John').getOperationContext();
      expect(context.or).toBe(true);
      expect(context.and).toBe(false);
    });

    it('is permanent toggle', () => {
      let stack = User.where.id.EQ('1').AND.firstName.EQ('John').OR.lastName.EQ('Smith').getOperationStack();
      let lastOr = stack.filter((op) => op.operator === 'OR').pop();
      expect(lastOr.or).toBe(true);
    });

    it('can group conditions with query argument', () => {
      let subQuery = User.where.firstName.EQ('John').AND.firstName.EQ('Jane');
      let query = User.where.id.EQ('test').OR(subQuery);
      let stack = query.getOperationStack();
      let orOp = stack.find((op) => op.operator === 'OR' && op.value);
      expect(orOp).toBeDefined();
    });
  });

  describe('LIMIT', () => {
    it('sets limit value', () => {
      let context = User.where.LIMIT(10).getOperationContext();
      expect(context.limit).toBe(10);
    });

    it('accepts Infinity', () => {
      let context = User.where.LIMIT(Infinity).getOperationContext();
      expect(context.limit).toBe(Infinity);
    });

    it('rounds floating point values', () => {
      let context = User.where.LIMIT(10.7).getOperationContext();
      expect(context.limit).toBe(11);
    });

    it('throws error for NaN', () => {
      expect(() => User.where.LIMIT(NaN)).toThrow();
    });

    it('throws error for negative values', () => {
      expect(() => User.where.LIMIT(-1)).toThrow();
    });

    it('throws error for non-numbers', () => {
      expect(() => User.where.LIMIT('10')).toThrow();
    });
  });

  describe('OFFSET', () => {
    it('sets offset value', () => {
      let context = User.where.OFFSET(20).getOperationContext();
      expect(context.offset).toBe(20);
    });

    it('rounds floating point values', () => {
      let context = User.where.OFFSET(5.3).getOperationContext();
      expect(context.offset).toBe(5);
    });

    it('throws error for Infinity', () => {
      expect(() => User.where.OFFSET(Infinity)).toThrow();
    });

    it('throws error for negative values', () => {
      expect(() => User.where.OFFSET(-5)).toThrow();
    });

    it('throws error for non-numbers', () => {
      expect(() => User.where.OFFSET('20')).toThrow();
    });
  });

  describe('ORDER', () => {
    it('sets order clause', () => {
      let context = User.where.ORDER('id').getOperationContext();
      expect(context.order).toBeDefined();
      expect(context.order.size).toBeGreaterThan(0);
    });

    it('ORDER.ASC sets ascending order', () => {
      let context = User.where.ORDER.ASC('id').getOperationContext();
      expect(context.order).toBeDefined();
      let orderEntry = Array.from(context.order.values())[0];
      expect(orderEntry.direction).toEqual('+');
    });

    it('ORDER.DESC sets descending order', () => {
      let context = User.where.ORDER.DESC('id').getOperationContext();
      expect(context.order).toBeDefined();
      let orderEntry = Array.from(context.order.values())[0];
      expect(orderEntry.direction).toEqual('-');
    });

    it('ORDER.ADD adds to existing order', () => {
      let query = User.where.ORDER('id').ORDER.ADD('+firstName');
      let context = query.getOperationContext();
      expect(context.order.size).toBe(2);
    });

    it('ORDER.REPLACE replaces existing order', () => {
      let query = User.where.ORDER('id', 'firstName').ORDER.REPLACE('+lastName');
      let context = query.getOperationContext();
      let keys = Array.from(context.order.keys());
      expect(keys.some((k) => k.includes('lastName'))).toBe(true);
    });

    it('accepts fields with direction prefix', () => {
      let context = User.where.ORDER.DESC('id').ORDER.ADD('+firstName').getOperationContext();
      expect(context.order.size).toBe(2);
    });

    it('accepts array of fields', () => {
      let context = User.where.ORDER([ 'id', 'firstName' ]).getOperationContext();
      expect(context.order.size).toBe(2);
    });

    it('throws error for invalid values', () => {
      expect(() => User.where.ORDER(123)).toThrow();
    });
  });

  describe('GROUP_BY', () => {
    it('sets group by clause', () => {
      let context = User.where.GROUP_BY('id').getOperationContext();
      expect(context.groupBy).toBeDefined();
      expect(context.groupBy.size).toBeGreaterThan(0);
    });

    it('accepts multiple fields', () => {
      let context = User.where.GROUP_BY('id', 'firstName').getOperationContext();
      expect(context.groupBy.size).toBe(2);
    });

    it('accepts array of fields', () => {
      let context = User.where.GROUP_BY([ 'id', 'firstName' ]).getOperationContext();
      expect(context.groupBy.size).toBe(2);
    });
  });

  describe('HAVING', () => {
    it('sets having clause with query', () => {
      let havingQuery = User.where.firstName.EQ('Test');
      let context = User.where.GROUP_BY('id').HAVING(havingQuery).getOperationContext();
      expect(context.having).toBeDefined();
      expect(QueryEngine.isQuery(context.having)).toBe(true);
    });
  });

  describe('EXISTS', () => {
    it('creates EXISTS subquery', () => {
      let subQuery = UserRole.where.userID.EQ(User.where.id).roleID.EQ('test');
      let context = User.where.EXISTS(subQuery).getOperationContext();
      expect(context.hasCondition).toBe(true);
    });

    it('throws error for query without conditions', () => {
      expect(() => User.where.EXISTS(UserRole.where)).toThrow();
    });

    it('can be combined with NOT', () => {
      let subQuery = UserRole.where.userID.EQ(User.where.id).roleID.EQ('test');
      let stack = User.where.NOT.EXISTS(subQuery).getOperationStack();
      let notOp = stack.find((op) => op.operator === 'NOT');
      expect(notOp).toBeDefined();
    });
  });

  describe('PROJECT', () => {
    it('sets projection fields', () => {
      let context = User.where.PROJECT('id', 'firstName').getOperationContext();
      expect(context.projection).toBeDefined();
      expect(context.projection.size).toBe(2);
    });

    it('accepts array of fields', () => {
      let context = User.where.PROJECT([ 'id', 'firstName' ]).getOperationContext();
      expect(context.projection.size).toBe(2);
    });

    it('can add fields with + prefix', () => {
      let context = User.where.PROJECT('id').PROJECT('+firstName').getOperationContext();
      expect(context.projection.size).toBe(2);
    });

    it('can remove fields with - prefix', () => {
      let context = User.where.PROJECT('id', 'firstName').PROJECT('-firstName').getOperationContext();
      expect(context.projection.size).toBe(1);
    });

    it('replaces projection without prefix', () => {
      let context = User.where.PROJECT('id', 'firstName').PROJECT('lastName').getOperationContext();
      let keys = Array.from(context.projection.keys());
      expect(keys.some((k) => k.includes('lastName'))).toBe(true);
      expect(keys.some((k) => k.includes('firstName'))).toBe(false);
    });

    it('accepts * wildcard', () => {
      let context = User.where.PROJECT('*').getOperationContext();
      expect(context.projection.size).toBeGreaterThan(1);
    });

    it('accepts string literals', () => {
      let literal = new Literal('COUNT(*) AS total');
      let context = User.where.PROJECT(literal).getOperationContext();
      expect(context.projection.size).toBe(1);
    });

    it('throws for unsupported literal types in base connection', () => {
      let literal = new CountLiteral('User:id', { as: 'count' });
      try {
        User.where.PROJECT(literal);
        fail('Expected to throw');
      } catch (e) {
        expect(e.message).toContain('not supported for this connection type');
      }
    });
  });

  describe('DISTINCT', () => {
    it('sets distinct with primary key by default', () => {
      let context = User.where.DISTINCT.id.EQ('test').getOperationContext();
      expect(context.distinct).toBeDefined();
      expect(context.distinct instanceof DistinctLiteral).toBe(true);
    });

    it('can be called with specific field', () => {
      let context = User.where.DISTINCT('firstName').id.EQ('test').getOperationContext();
      expect(context.distinct).toBeDefined();
    });

    it('can be disabled with false', () => {
      let context = User.where.DISTINCT.DISTINCT(false).id.EQ('test').getOperationContext();
      expect(context.distinct).toBeNull();
    });
  });

  describe('JOIN operations', () => {
    it('INNER_JOIN sets inner join type', () => {
      let context = User.where.INNER_JOIN.id.EQ(UserRole.where.userID).getOperationContext();
      expect(context.joinType).toEqual('inner');
    });

    it('LEFT_JOIN sets left join type', () => {
      let context = User.where.LEFT_JOIN.id.EQ(UserRole.where.userID).getOperationContext();
      expect(context.joinType).toEqual('left');
    });

    it('LEFT_JOIN with outer option', () => {
      let context = User.where.LEFT_JOIN(true).id.EQ(UserRole.where.userID).getOperationContext();
      expect(context.joinType).toEqual('left');
      expect(context.joinOuter).toBe(true);
    });

    it('RIGHT_JOIN sets right join type', () => {
      let context = User.where.RIGHT_JOIN.id.EQ(UserRole.where.userID).getOperationContext();
      expect(context.joinType).toEqual('right');
    });

    it('FULL_JOIN sets full join type', () => {
      let context = User.where.FULL_JOIN.id.EQ(UserRole.where.userID).getOperationContext();
      expect(context.joinType).toEqual('full');
    });

    it('CROSS_JOIN sets cross join type', () => {
      let context = User.where.CROSS_JOIN.id.EQ(UserRole.where.userID).getOperationContext();
      expect(context.joinType).toEqual('cross');
    });

    it('JOIN accepts custom join type string', () => {
      let context = User.where.JOIN('natural').id.EQ(UserRole.where.userID).getOperationContext();
      expect(context.joinType).toEqual('natural');
    });

    it('JOIN accepts literal', () => {
      let literal = new Literal('LATERAL');
      let context = User.where.JOIN(literal).id.EQ(UserRole.where.userID).getOperationContext();
      expect(context.joinType).toBe(literal);
    });

    it('JOIN throws error for invalid values', () => {
      expect(() => User.where.JOIN(123)).toThrow();
    });
  });

  describe('mergeFields', () => {
    it('replaces fields by default', () => {
      let context = User.where.ORDER('id').ORDER('firstName').getOperationContext();
      let keys = Array.from(context.order.keys());
      expect(keys.some((k) => k.includes('firstName'))).toBe(true);
      expect(keys.some((k) => k.includes('id'))).toBe(false);
    });

    it('adds fields with + prefix', () => {
      let context = User.where.ORDER('id').ORDER('+firstName').getOperationContext();
      expect(context.order.size).toBe(2);
    });

    it('removes fields with - prefix', () => {
      let context = User.where.ORDER('id', 'firstName').ORDER('-id').getOperationContext();
      let keys = Array.from(context.order.keys());
      expect(keys.some((k) => k.includes('id'))).toBe(false);
      expect(keys.some((k) => k.includes('firstName'))).toBe(true);
    });

    it('handles * wildcard for all fields', () => {
      let context = User.where.PROJECT('*').getOperationContext();
      expect(context.projection.size).toBeGreaterThan(3);
    });

    it('supports @ prefix for literals', () => {
      let context = User.where.PROJECT('@COUNT(*) AS total').getOperationContext();
      expect(context.projection.size).toBe(1);
    });
  });

  describe('chaining', () => {
    it('returns ModelScope for continued chaining', () => {
      let scope = User.where.LIMIT(10).OFFSET(5).ORDER('id');
      let context = scope.getOperationContext();
      expect(context.limit).toBe(10);
      expect(context.offset).toBe(5);
      expect(context.order).toBeDefined();
    });

    it('supports complex chaining', () => {
      let query = User.where
        .DISTINCT
        .LEFT_JOIN
        .id.EQ(UserRole.where.userID)
        .AND.firstName.EQ('John')
        .OR.lastName.EQ('Smith')
        .ORDER.DESC('id')
        .LIMIT(10)
        .OFFSET(0)
        .PROJECT('id', 'firstName', 'lastName');

      let context = query.getOperationContext();
      expect(context.distinct).toBeDefined();
      expect(context.limit).toBe(10);
      expect(context.offset).toBe(0);
      expect(context.order).toBeDefined();
      expect(context.projection).toBeDefined();
    });
  });

  describe('toString', () => {
    it('delegates to query engine scope', () => {
      let query = User.where.id.EQ('test');
      let str = query.toString();
      expect(typeof str).toEqual('string');
    });
  });
});
