/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Model, QueryEngine, Types } = require('../../src');

class User extends Model {
  static fields = {
    'id': {
      type:       Types.BIGINT,
      allowNull:  false,
      primaryKey: true,
    },
    'firstName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'lastName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
  };
}

describe('QueryEngine', () => {
  it('can construct a query context with a model sub-key', () => {
    let query = QueryEngine.query(User);
    let context = query.User.id.EQ('test').getRawQueryContext();

    expect(context).toBeInstanceOf(Array);
    expect(context.length).toEqual(1);

    expect({
      modelName:  context[0].modelName,
      fieldName:  context[0].fieldName,
      operator:   context[0].operator,
      value:      context[0].value,
    }).toEqual({
      modelName:  'User',
      fieldName:  'id',
      operator:   'EQ',
      value:      'test',
    });
  });

  it('can construct a query context with a model call', () => {
    let query = QueryEngine.query(User);
    let context = query.User('id').EQ('test').getRawQueryContext();

    expect(context).toBeInstanceOf(Array);
    expect(context.length).toEqual(1);

    expect({
      modelName:  context[0].modelName,
      fieldName:  context[0].fieldName,
      operator:   context[0].operator,
      value:      context[0].value,
    }).toEqual({
      modelName:  'User',
      fieldName:  'id',
      operator:   'EQ',
      value:      'test',
    });
  });

  it('can chain query conditions', () => {
    let query = QueryEngine.query(User);
    let context = query.User('id').EQ(1).AND.firstName.EQ('Test').AND.NOT.lastName.EQ('Stuff').getRawQueryContext();

    expect(context).toBeInstanceOf(Array);
    expect(context.length).toEqual(6);

    // 0
    expect({
      modelName:  context[0].modelName,
      fieldName:  context[0].fieldName,
      operator:   context[0].operator,
      value:      context[0].value,
      index:      context[0].index,
    }).toEqual({
      modelName:  'User',
      fieldName:  'id',
      operator:   'EQ',
      value:      1,
      index:      0,
    });

    // 1
    expect({
      operator:   context[1].operator,
      index:      context[1].index,
    }).toEqual({
      operator:   'AND',
      index:      1,
    });

    // 2
    expect({
      modelName:  context[2].modelName,
      fieldName:  context[2].fieldName,
      operator:   context[2].operator,
      value:      context[2].value,
      index:      context[2].index,
    }).toEqual({
      modelName:  'User',
      fieldName:  'firstName',
      operator:   'EQ',
      value:      'Test',
      index:      2,
    });

    // 3
    expect({
      operator:   context[3].operator,
      index:      context[3].index,
    }).toEqual({
      operator:   'AND',
      index:      3,
    });

    // 4
    expect({
      operator:   context[4].operator,
      index:      context[4].index,
    }).toEqual({
      operator:   'NOT',
      index:      4,
    });

    // 5
    expect({
      modelName:  context[5].modelName,
      fieldName:  context[5].fieldName,
      operator:   context[5].operator,
      value:      context[5].value,
      index:      context[5].index,
    }).toEqual({
      modelName:  'User',
      fieldName:  'lastName',
      operator:   'EQ',
      value:      'Stuff',
      index:      5,
    });
  });
});
