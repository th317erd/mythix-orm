/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const Nife = require('nife');
const { ConnectionBase, QueryEngine } = require('../../lib');
const matchesSnapshot = require('../support/snapshots');

describe('QueryEngine', () => {
  let connection;
  let User;
  let ScopedUser;

  beforeAll(() => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
    ScopedUser = models.ScopedUser;
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

  // fdescribe('operations', () => {
  //   it('can live happily without a connection', () => {
  //     expect((new QueryEngine().User.id('derp'))._getRawQuery()).toBe([ ]);
  //   });
  // });

  describe('isQueryContext', () => {
    it('can validly detect a query context', () => {
      expect(QueryEngine.isQueryContext({})).toBe(false);
      expect(QueryEngine.isQueryContext(User.where)).toBe(false);
      expect(QueryEngine.isQueryContext(User.where._getRawQueryContext())).toBe(true);
    });
  });

  describe('queryContextType', () => {
    it('can validly detect a query context type', () => {
      expect(QueryEngine.queryContextType(User.where._getRawQueryContext())).toEqual({
        hasCondition: false,
        hasField:     false,
        hasModel:     true,
      });

      expect(QueryEngine.queryContextType(User.where.id._getRawQueryContext())).toEqual({
        hasCondition: false,
        hasField:     true,
        hasModel:     true,
      });

      expect(QueryEngine.queryContextType(User.where.id.EQ('test')._getRawQueryContext())).toEqual({
        hasCondition: true,
        hasField:     true,
        hasModel:     true,
      });
    });
  });

  describe('query operations and chaining', () => {
    it('can set a default scope on a model', () => {
      let context = ScopedUser.where.id.EQ('test')._getRawQuery();
      expect(matchesSnapshot(getFilteredContext(context))).toEqual(true);
    });

    it('can unscope default scope on a model', () => {
      let context = ScopedUser.where.unscoped().id.EQ('test')._getRawQuery();
      expect(matchesSnapshot(getFilteredContext(context))).toEqual(true);
    });

    it('can construct a query context with a model sub-key', () => {
      let context = User.where.id.EQ('test')._getRawQuery();
      expect(matchesSnapshot(getFilteredContext(context))).toEqual(true);
    });

    it('can construct a query context with a model call', () => {
      let context = User.where('id').EQ('test')._getRawQuery();
      expect(matchesSnapshot(getFilteredContext(context))).toEqual(true);
    });

    it('can chain query conditions', () => {
      let context = User.where.id.EQ(1).AND.firstName.EQ('Test').AND.NOT.lastName.EQ('Stuff')._getRawQuery();
      expect(matchesSnapshot(getFilteredContext(context))).toEqual(true);
    });
  });
});
