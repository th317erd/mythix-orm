/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { ConnectionBase, QueryEngine } = require('../../src');

describe('QueryEngine', () => {
  let User;
  let ScopedUser;

  beforeAll(() => {
    let connection = new ConnectionBase({
      models: require('../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
    ScopedUser = models.ScopedUser;
  });

  const getContext = (context, extraFields) => {
    let data = {
      rootModelName:  context.rootModelName,
      modelName:      context.modelName,
      fieldName:      context.fieldName,
      operator:       context.operator,
      value:          context.value,
      and:            context.and,
      or:             context.or,
    };

    if (extraFields) {
      extraFields.forEach((fieldName) => {
        data[fieldName] = context[fieldName];
      });
    }

    return data;
  };

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

      expect(context).toBeInstanceOf(Array);
      expect(context.length).toEqual(5);

      expect(getContext(context[0])).toEqual({
        rootModelName:  'ScopedUser',
        modelName:      'ScopedUser',
        fieldName:      undefined,
        operator:       'MODEL',
        value:          undefined,
        and:            true,
        or:             false,
      });

      expect(getContext(context[1])).toEqual({
        rootModelName:  'ScopedUser',
        modelName:      'ScopedUser',
        fieldName:      'firstName',
        operator:       'FIELD',
        value:          undefined,
        and:            true,
        or:             false,
      });

      expect(getContext(context[2])).toEqual({
        rootModelName:  'ScopedUser',
        modelName:      'ScopedUser',
        fieldName:      'firstName',
        operator:       'EQ',
        value:          'Bob',
        and:            true,
        or:             false,
      });

      expect(getContext(context[3])).toEqual({
        rootModelName:  'ScopedUser',
        modelName:      'ScopedUser',
        fieldName:      'id',
        operator:       'FIELD',
        value:          undefined,
        and:            true,
        or:             false,
      });

      expect(getContext(context[4])).toEqual({
        rootModelName:  'ScopedUser',
        modelName:      'ScopedUser',
        fieldName:      'id',
        operator:       'EQ',
        value:          'test',
        and:            true,
        or:             false,
      });
    });

    it('can unscope default scope on a model', () => {
      let context = ScopedUser.where.unscoped().id.EQ('test')._getRawQuery();

      expect(context).toBeInstanceOf(Array);
      expect(context.length).toEqual(3);

      expect(getContext(context[0])).toEqual({
        rootModelName:  'ScopedUser',
        modelName:      'ScopedUser',
        fieldName:      undefined,
        operator:       'MODEL',
        value:          undefined,
        and:            true,
        or:             false,
      });

      expect(getContext(context[1])).toEqual({
        rootModelName:  'ScopedUser',
        modelName:      'ScopedUser',
        fieldName:      'id',
        operator:       'FIELD',
        value:          undefined,
        and:            true,
        or:             false,
      });

      expect(getContext(context[2])).toEqual({
        rootModelName:  'ScopedUser',
        modelName:      'ScopedUser',
        fieldName:      'id',
        operator:       'EQ',
        value:          'test',
        and:            true,
        or:             false,
      });
    });

    it('can construct a query context with a model sub-key', () => {
      let context = User.where.id.EQ('test')._getRawQuery();

      expect(context).toBeInstanceOf(Array);
      expect(context.length).toEqual(3);

      expect(getContext(context[0])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      undefined,
        operator:       'MODEL',
        value:          undefined,
        and:            true,
        or:             false,
      });

      expect(getContext(context[1])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      'id',
        operator:       'FIELD',
        value:          undefined,
        and:            true,
        or:             false,
      });

      expect(getContext(context[2])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      'id',
        operator:       'EQ',
        value:          'test',
        and:            true,
        or:             false,
      });
    });

    it('can construct a query context with a model call', () => {
      let context = User.where('id').EQ('test')._getRawQuery();

      expect(context).toBeInstanceOf(Array);
      expect(context.length).toEqual(3);

      expect(getContext(context[0])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      undefined,
        operator:       'MODEL',
        value:          undefined,
        and:            true,
        or:             false,
      });

      expect(getContext(context[1])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      'id',
        operator:       'FIELD',
        value:          undefined,
        and:            true,
        or:             false,
      });

      expect(getContext(context[2])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      'id',
        operator:       'EQ',
        value:          'test',
        and:            true,
        or:             false,
      });
    });

    it('can chain query conditions', () => {
      let context = User.where.id.EQ(1).AND.firstName.EQ('Test').AND.NOT.lastName.EQ('Stuff')._getRawQuery();

      expect(context).toBeInstanceOf(Array);
      expect(context.length).toEqual(10);

      expect(getContext(context[0])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      undefined,
        operator:       'MODEL',
        value:          undefined,
        and:            true,
        or:             false,
      });

      expect(getContext(context[1])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      'id',
        operator:       'FIELD',
        value:          undefined,
        and:            true,
        or:             false,
      });

      // 0
      expect(getContext(context[2])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      'id',
        operator:       'EQ',
        value:          1,
        and:            true,
        or:             false,
      });

      expect({
        operator:   context[3].operator,
        partIndex:  context[3].partIndex,
      }).toEqual({
        operator:   'AND',
        partIndex:  3,
      });

      expect(getContext(context[4], [ 'partIndex' ])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      'firstName',
        operator:       'FIELD',
        value:          undefined,
        and:            true,
        or:             false,
        partIndex:      4,
      });

      expect(getContext(context[5])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      'firstName',
        operator:       'EQ',
        value:          'Test',
        and:            true,
        or:             false,
      });

      expect(getContext(context[6], [ 'partIndex' ])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      'firstName',
        operator:       'AND',
        value:          undefined,
        and:            true,
        or:             false,
        partIndex:      6,
      });

      expect({
        operator:   context[7].operator,
        partIndex:  context[7].partIndex,
      }).toEqual({
        operator:   'NOT',
        partIndex:  7,
      });

      expect(getContext(context[8], [ 'partIndex' ])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      'lastName',
        operator:       'FIELD',
        value:          undefined,
        and:            true,
        or:             false,
        partIndex:      8,
      });

      expect(getContext(context[9])).toEqual({
        rootModelName:  'User',
        modelName:      'User',
        fieldName:      'lastName',
        operator:       'EQ',
        value:          'Stuff',
        and:            true,
        or:             false,
      });
    });

    // it('can chain query conditions from more than one model', () => {
    //   let context = query.User.id.EQ(1).AND.firstName.EQ('Test').AND.NOT.lastName.EQ('Stuff').AND.Role.id.EQ(User.)._getRawQuery();

    //   expect(context).toBeInstanceOf(Array);
    //   expect(context.length).toEqual(6);

    //   // 0
    //   expect({
    //     modelName:  context[0].modelName,
    //     fieldName:  context[0].fieldName,
    //     operator:   context[0].operator,
    //     value:      context[0].value,
    //     partIndex:  context[0].partIndex,
    //   }).toEqual({
    //     modelName:  'User',
    //     fieldName:  'id',
    //     operator:   'EQ',
    //     value:      1,
    //     partIndex:  0,
    //   });

    //   // 1
    //   expect({
    //     operator:   context[1].operator,
    //     partIndex:  context[1].partIndex,
    //   }).toEqual({
    //     operator:   'AND',
    //     partIndex:  1,
    //   });

    //   // 2
    //   expect({
    //     modelName:  context[2].modelName,
    //     fieldName:  context[2].fieldName,
    //     operator:   context[2].operator,
    //     value:      context[2].value,
    //     partIndex:  context[2].partIndex,
    //   }).toEqual({
    //     modelName:  'User',
    //     fieldName:  'firstName',
    //     operator:   'EQ',
    //     value:      'Test',
    //     partIndex:  2,
    //   });

    //   // 3
    //   expect({
    //     operator:   context[3].operator,
    //     partIndex:  context[3].partIndex,
    //   }).toEqual({
    //     operator:   'AND',
    //     partIndex:  3,
    //   });

    //   // 4
    //   expect({
    //     operator:   context[4].operator,
    //     partIndex:  context[4].partIndex,
    //   }).toEqual({
    //     operator:   'NOT',
    //     partIndex:  4,
    //   });

    //   // 5
    //   expect({
    //     modelName:  context[5].modelName,
    //     fieldName:  context[5].fieldName,
    //     operator:   context[5].operator,
    //     value:      context[5].value,
    //     partIndex:  context[5].partIndex,
    //   }).toEqual({
    //     modelName:  'User',
    //     fieldName:  'lastName',
    //     operator:   'EQ',
    //     value:      'Stuff',
    //     partIndex:  5,
    //   });
    // });
  });

  // TODO: AND and OR need to have callbacks for grouping
  // TODO: Need to implement join
  // TODO: Need to implement project
  // TODO: Need to implement limit
  // TODO: Need to implement offset
  // TODO: Need to implement order
  // TODO: Need to implement include (load sub-models)
  // TODO: Need to implement all operators
});
