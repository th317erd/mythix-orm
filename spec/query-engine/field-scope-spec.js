/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, expect, beforeAll */

const { ConnectionBase, QueryEngine } = require('../../lib');
const { createRunners } = require('../support/test-helpers');
const AllModels = require('../support/models');

describe('FieldScope', () => {
  let connection;
  let User;
  let UserRole;
  let NumberModel;

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
    NumberModel = models.Number;
  });

  describe('EQ', () => {
    it('creates equality condition', () => {
      let context = User.where.id.EQ('test').getOperationContext();
      expect(context.hasCondition).toBe(true);
    });

    it('handles null value (IS NULL)', () => {
      let stack = User.where.id.EQ(null).getOperationStack();
      let condition = stack.find((op) => op.operator === 'EQ');
      expect(condition.value).toBeNull();
    });

    it('handles array value (IN)', () => {
      // Use numbers intentionally to test internal type handling
      let stack = User.where.id.EQ([ 1, 2, 3 ]).getOperationStack();
      let condition = stack.find((op) => op.operator === 'EQ');
      expect(Array.isArray(condition.value)).toBe(true);
    });

    it('handles subquery without conditions (table join)', () => {
      let context = User.where.id.EQ(UserRole.where.userID).getOperationContext();
      expect(context.hasCondition).toBe(true);
    });

    it('handles subquery with conditions (IN subquery)', () => {
      let subQuery = UserRole.where.userID.EQ('test-id').PROJECT('userID');
      let context = User.where.id.EQ(subQuery).getOperationContext();
      expect(context.hasCondition).toBe(true);
    });

    describe('EQ.ANY', () => {
      it('creates ANY subquery condition', () => {
        let subQuery = NumberModel.where.numberInt.GTE(18).PROJECT('numberInt');
        let context = NumberModel.where.numberInt.EQ.ANY(subQuery).getOperationContext();
        expect(context.hasCondition).toBe(true);
      });

      it('throws error for query without conditions', () => {
        expect(() => NumberModel.where.numberInt.EQ.ANY(NumberModel.where)).toThrow();
      });
    });

    describe('EQ.ALL', () => {
      it('creates ALL subquery condition', () => {
        let subQuery = NumberModel.where.numberInt.GTE(18).PROJECT('numberInt');
        let context = NumberModel.where.numberInt.EQ.ALL(subQuery).getOperationContext();
        expect(context.hasCondition).toBe(true);
      });
    });
  });

  describe('NEQ', () => {
    it('creates not-equal condition', () => {
      let stack = User.where.id.NEQ('test').getOperationStack();
      let condition = stack.find((op) => op.operator === 'NEQ');
      expect(condition).toBeDefined();
      expect(condition.inverseOperator).toEqual('EQ');
    });

    it('handles null value (IS NOT NULL)', () => {
      let stack = User.where.id.NEQ(null).getOperationStack();
      let condition = stack.find((op) => op.operator === 'NEQ');
      expect(condition.value).toBeNull();
    });

    it('handles array value (NOT IN)', () => {
      // Use numbers intentionally to test internal type handling
      let stack = User.where.id.NEQ([ 1, 2, 3 ]).getOperationStack();
      let condition = stack.find((op) => op.operator === 'NEQ');
      expect(Array.isArray(condition.value)).toBe(true);
    });

    describe('NEQ.ANY', () => {
      it('creates ANY subquery condition', () => {
        let subQuery = NumberModel.where.numberInt.GTE(18).PROJECT('numberInt');
        let context = NumberModel.where.numberInt.NEQ.ANY(subQuery).getOperationContext();
        expect(context.hasCondition).toBe(true);
      });
    });
  });

  describe('GT', () => {
    it('creates greater-than condition', () => {
      let stack = NumberModel.where.numberInt.GT(18).getOperationStack();
      let condition = stack.find((op) => op.operator === 'GT');
      expect(condition).toBeDefined();
      expect(condition.value).toBe(18);
      expect(condition.inverseOperator).toEqual('LTE');
    });

    it('handles table join', () => {
      let context = User.where.id.GT(UserRole.where.userID).getOperationContext();
      expect(context.hasCondition).toBe(true);
    });

    describe('GT.ANY', () => {
      it('creates ANY subquery condition', () => {
        let subQuery = NumberModel.where.numberInt.GTE(18).PROJECT('numberInt');
        let context = NumberModel.where.numberInt.GT.ANY(subQuery).getOperationContext();
        expect(context.hasCondition).toBe(true);
      });
    });

    describe('GT.ALL', () => {
      it('creates ALL subquery condition', () => {
        let subQuery = NumberModel.where.numberInt.GTE(18).PROJECT('numberInt');
        let context = NumberModel.where.numberInt.GT.ALL(subQuery).getOperationContext();
        expect(context.hasCondition).toBe(true);
      });
    });
  });

  describe('GTE', () => {
    it('creates greater-than-or-equal condition', () => {
      let stack = NumberModel.where.numberInt.GTE(18).getOperationStack();
      let condition = stack.find((op) => op.operator === 'GTE');
      expect(condition).toBeDefined();
      expect(condition.value).toBe(18);
      expect(condition.inverseOperator).toEqual('LT');
    });

    describe('GTE.ANY', () => {
      it('creates ANY subquery condition', () => {
        let subQuery = NumberModel.where.numberInt.GTE(18).PROJECT('numberInt');
        let context = NumberModel.where.numberInt.GTE.ANY(subQuery).getOperationContext();
        expect(context.hasCondition).toBe(true);
      });
    });
  });

  describe('LT', () => {
    it('creates less-than condition', () => {
      let stack = NumberModel.where.numberInt.LT(65).getOperationStack();
      let condition = stack.find((op) => op.operator === 'LT');
      expect(condition).toBeDefined();
      expect(condition.value).toBe(65);
      expect(condition.inverseOperator).toEqual('GTE');
    });

    describe('LT.ANY', () => {
      it('creates ANY subquery condition', () => {
        let subQuery = NumberModel.where.numberInt.GTE(18).PROJECT('numberInt');
        let context = NumberModel.where.numberInt.LT.ANY(subQuery).getOperationContext();
        expect(context.hasCondition).toBe(true);
      });
    });

    describe('LT.ALL', () => {
      it('creates ALL subquery condition', () => {
        let subQuery = NumberModel.where.numberInt.GTE(18).PROJECT('numberInt');
        let context = NumberModel.where.numberInt.LT.ALL(subQuery).getOperationContext();
        expect(context.hasCondition).toBe(true);
      });
    });
  });

  describe('LTE', () => {
    it('creates less-than-or-equal condition', () => {
      let stack = NumberModel.where.numberInt.LTE(65).getOperationStack();
      let condition = stack.find((op) => op.operator === 'LTE');
      expect(condition).toBeDefined();
      expect(condition.value).toBe(65);
      expect(condition.inverseOperator).toEqual('GT');
    });

    describe('LTE.ANY', () => {
      it('creates ANY subquery condition', () => {
        let subQuery = NumberModel.where.numberInt.GTE(18).PROJECT('numberInt');
        let context = NumberModel.where.numberInt.LTE.ANY(subQuery).getOperationContext();
        expect(context.hasCondition).toBe(true);
      });
    });
  });

  describe('LIKE', () => {
    it('creates LIKE condition', () => {
      let stack = User.where.firstName.LIKE('%John%').getOperationStack();
      let condition = stack.find((op) => op.operator === 'LIKE');
      expect(condition).toBeDefined();
      expect(condition.value).toEqual('%John%');
      expect(condition.inverseOperator).toEqual('NOT_LIKE');
    });

    it('is case-insensitive by default', () => {
      let stack = User.where.firstName.LIKE('%john%').getOperationStack();
      let condition = stack.find((op) => op.operator === 'LIKE');
      expect(condition.caseSensitive).toBe(false);
    });

    it('can be case-sensitive with option', () => {
      let stack = User.where.firstName.LIKE('%John%', { caseSensitive: true }).getOperationStack();
      let condition = stack.find((op) => op.operator === 'LIKE');
      expect(condition.caseSensitive).toBe(true);
    });
  });

  describe('NOT_LIKE', () => {
    it('creates NOT LIKE condition', () => {
      let stack = User.where.firstName.NOT_LIKE('%John%').getOperationStack();
      let condition = stack.find((op) => op.operator === 'NOT_LIKE');
      expect(condition).toBeDefined();
      expect(condition.inverseOperator).toEqual('LIKE');
    });

    it('can be case-sensitive with option', () => {
      let stack = User.where.firstName.NOT_LIKE('%John%', { caseSensitive: true }).getOperationStack();
      let condition = stack.find((op) => op.operator === 'NOT_LIKE');
      expect(condition.caseSensitive).toBe(true);
    });
  });

  describe('NOT', () => {
    it('inverts the following condition', () => {
      let stack = User.where.id.NOT.EQ('test').getOperationStack();
      let notOp = stack.find((op) => op.operator === 'NOT');
      expect(notOp).toBeDefined();
      expect(notOp.not).toBe(true);
    });

    it('returns proxy for chaining', () => {
      let query = User.where.id.NOT.EQ('test');
      expect(QueryEngine.isQuery(query)).toBe(true);
    });
  });

  describe('_fetchOperatorValue', () => {
    it('returns primitive values unchanged', () => {
      let stack = User.where.id.EQ('test').getOperationStack();
      let condition = stack.find((op) => op.operator === 'EQ');
      expect(condition.value).toEqual('test');
    });

    it('extracts primary key from model class', () => {
      let context = User.where.id.EQ(UserRole).getOperationContext();
      expect(context.hasCondition).toBe(true);
    });

    it('extracts primary key value from model instance', () => {
      let userInstance = new User();
      // Use a valid UUID to avoid validation errors
      let validUUID = 'a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11';
      userInstance.id = validUUID;

      let stack = User.where.id.EQ(userInstance).getOperationStack();
      let condition = stack.find((op) => op.operator === 'EQ');
      expect(condition.value).toEqual(validUUID);
    });

    it('handles null and undefined', () => {
      let stack1 = User.where.id.EQ(null).getOperationStack();
      let condition1 = stack1.find((op) => op.operator === 'EQ');
      expect(condition1.value).toBeNull();

      let stack2 = User.where.id.EQ(undefined).getOperationStack();
      let condition2 = stack2.find((op) => op.operator === 'EQ');
      expect(condition2.value).toBeUndefined();
    });
  });

  describe('chaining', () => {
    it('returns to ModelScope after condition', () => {
      let query = User.where.id.EQ('test').firstName.EQ('John');
      let context = query.getOperationContext();
      expect(context.hasCondition).toBe(true);
    });

    it('supports complex chaining', () => {
      let query = User.where
        .id.EQ('1')
        .AND.firstName.LIKE('%John%')
        .OR.lastName.NEQ('Doe');

      let context = query.getOperationContext();
      expect(context.hasCondition).toBe(true);
    });

    it('can access ModelScope methods through MISSING', () => {
      let query = User.where.id.EQ('test').LIMIT(10);
      let context = query.getOperationContext();
      expect(context.limit).toBe(10);
    });
  });

  describe('toString', () => {
    it('delegates to query engine scope', () => {
      let query = User.where.id.EQ('test');
      let str = query.toString();
      expect(typeof str).toEqual('string');
    });
  });

  describe('error handling', () => {
    it('throws error for object values in operators', () => {
      expect(() => User.where.id.EQ({})).toThrow();
    });

    it('throws for model class without primary key', () => {
      class NoPKModel {}
      NoPKModel._isMythixModel = true;
      NoPKModel.getPrimaryKeyFieldName = () => null;

      expect(() => User.where.id.EQ(NoPKModel)).toThrow();
    });
  });
});
