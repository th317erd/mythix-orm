/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, expect, beforeAll */

const { ConnectionBase, QueryEngine } = require('../../lib');
const matchesSnapshot = require('../support/snapshots');
const { createRunners } = require('../support/test-helpers');
const AllModels = require('../support/models');

class _TestModel extends AllModels.UserThing {
  static fields = {
    ...AllModels.UserThing.fields,
  };

  static getModelName() {
    return 'TestModel';
  }

  static async finalizeQuery({ type, query }) {
    return query.AND.TestModel.roleThingID.EQ('derp');
  }
}

describe('QueryEngine', () => {
  let connection;
  let User;
  let ScopedUser;
  let UserThing;
  let TestModel;

  // eslint-disable-next-line no-unused-vars
  const { it, fit } = createRunners(() => connection);

  beforeAll(() => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     {
        ...AllModels,
        TestModel: _TestModel,
      },
    });

    let models = connection.getModels();
    User = models.User;
    ScopedUser = models.ScopedUser;
    UserThing = models.UserThing;
    TestModel = models.TestModel;
  });

  const getFilteredContext = (parts) => {
    return parts.map((part) => {
      return Object.assign({}, part, {
        contextID:    undefined,
        modelContext: undefined,
        fieldContext: undefined,
      });
    });
  };

  describe('isQueryOperationContext', () => {
    it('can validly detect a query context', () => {
      expect(QueryEngine.isQueryOperationContext({})).toBe(false);
      expect(QueryEngine.isQueryOperationContext(User.where)).toBe(false);
      expect(QueryEngine.isQueryOperationContext(User.where.getOperationContext())).toBe(true);
    });
  });

  describe('queryOperationInfo', () => {
    it('can validly detect a query context type', () => {
      expect(QueryEngine.queryOperationInfo(User.where.getOperationContext())).toEqual({
        hasCondition: false,
        hasField:     false,
        hasModel:     true,
      });

      expect(QueryEngine.queryOperationInfo(User.where.id.getOperationContext())).toEqual({
        hasCondition: false,
        hasField:     true,
        hasModel:     true,
      });

      expect(QueryEngine.queryOperationInfo(User.where.id.EQ('test').getOperationContext())).toEqual({
        hasCondition: true,
        hasField:     true,
        hasModel:     true,
      });
    });
  });

  describe('query operations and chaining', () => {
    it('can set a default scope on a model', () => {
      let context = ScopedUser.where.id.EQ('test').getOperationStack();
      expect(matchesSnapshot(getFilteredContext(context))).toEqual(true);
    });

    it('can unscope default scope on a model', () => {
      let context = ScopedUser.where.unscoped().id.EQ('test').getOperationStack();
      expect(matchesSnapshot(getFilteredContext(context))).toEqual(true);
    });

    it('can construct a query context with a model sub-key', () => {
      let context = User.where.id.EQ('test').getOperationStack();
      expect(matchesSnapshot(getFilteredContext(context))).toEqual(true);
    });

    it('can construct a query context with a model call', () => {
      let context = User.where('id').EQ('test').getOperationStack();
      expect(matchesSnapshot(getFilteredContext(context))).toEqual(true);
    });

    it('can chain query conditions', () => {
      let context = User.where.id.EQ(1).AND.firstName.EQ('Test').AND.NOT.lastName.EQ('Stuff').getOperationStack();
      expect(matchesSnapshot(getFilteredContext(context))).toEqual(true);
    });
  });

  describe('merging', () => {
    it('can merge two queries', () => {
      let subQuery = UserThing.where.userID.EQ(User.where.id).AND(User.where.firstName.EQ('Bob').OR.lastName.EQ('Brown'));
      let context = UserThing.where.id.EQ('test').AND.MERGE(subQuery).getOperationStack();
      expect(matchesSnapshot(getFilteredContext(context))).toEqual(true);
    });
  });

  describe('clone', () => {
    it('can clone a query', () => {
      let query1    = UserThing.where.userID.EQ(User.where.id).AND(User.where.firstName.EQ('Bob').OR.lastName.EQ('Brown'));
      let query2    = query1.clone();
      let context1  = query1.getOperationStack();
      let context2  = query2.getOperationStack();

      expect(context1.length).toEqual(context2.length);

      for (let i = 0, il = context1.length; i < il; i++) {
        expect(getFilteredContext([ context1[i] ])).toEqual(getFilteredContext([ context2[i] ]));

        context1[i].testIndex = i;
        expect(context2[i].testIndex).toBe(undefined);
      }
    });
  });

  describe('filter', () => {
    it('can filter a query', () => {
      let query1    = UserThing.where.userID.EQ(User.where.id).AND(User.where.firstName.EQ('Bob').OR.lastName.EQ('Brown'));
      let query2    = query1.filter((part) => {
        if (Object.prototype.hasOwnProperty.call(part, 'condition'))
          return false;

        return true;
      });
      let context1  = query1.getOperationStack();
      let context2  = query2.getOperationStack();

      expect(context1.length).not.toEqual(context2.length);

      expect(matchesSnapshot(getFilteredContext(context1))).toEqual(true);
      expect(matchesSnapshot(getFilteredContext(context2))).toEqual(true);
    });
  });

  describe('map', () => {
    it('can map a query', () => {
      let query1    = UserThing.where.userID.EQ(User.where.id).AND(User.where.firstName.EQ('Bob').OR.lastName.EQ('Brown'));
      let query2    = query1.map((part) => {
        return {
          ...part,
          isMapped: true,
        };
      });
      let context1  = query1.getOperationStack();
      let context2  = query2.getOperationStack();

      expect(matchesSnapshot(getFilteredContext(context1))).toEqual(true);
      expect(matchesSnapshot(getFilteredContext(context2))).toEqual(true);
    });
  });

  describe('walk', () => {
    it('can walk queries', () => {
      let subQuery1 = User.where.id;
      let subQuery2 = User.where.firstName.EQ('Bob').OR.lastName.EQ('Brown');
      let subQuery3 = UserThing.where.userID.EQ(subQuery1).AND(subQuery2);
      let query     = UserThing.where.id.EQ('test').AND(subQuery3);

      let queries = [];

      query.walk((query, parent, contextKey, depth) => {
        queries.push({ query, parent, contextKey, depth });
      });

      expect(queries.length).toEqual(3);

      expect(queries[0].query).toBe(subQuery1);
      expect(queries[0].parent.operator).toEqual('EQ');
      expect(queries[0].contextKey).toEqual('value');
      expect(queries[0].depth).toEqual(2);

      expect(queries[1].query).toBe(subQuery2);
      expect(queries[1].parent.operator).toEqual('AND');
      expect(queries[1].contextKey).toEqual('value');
      expect(queries[1].depth).toEqual(2);

      expect(queries[2].query).toBe(subQuery3);
      expect(queries[2].parent.operator).toEqual('AND');
      expect(queries[2].contextKey).toEqual('value');
      expect(queries[2].depth).toEqual(1);
    });
  });

  describe('finalizeQuery', () => {
    it('can finalize a query', async () => {
      let subQuery1 = User.where.id;
      let subQuery2 = User.where.firstName.EQ('Bob').OR.lastName.EQ('Brown');
      let subQuery3 = TestModel.where.userID.EQ(subQuery1).AND(subQuery2);
      let query     = TestModel.where.id.EQ('test').AND(subQuery3);

      query = await query.finalizeQuery('read');
      expect(matchesSnapshot(getFilteredContext(query.getOperationStack()))).toEqual(true);
    });
  });
});
