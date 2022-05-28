/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll, beforeEach */

const {
  Model,
  QueryEngine,
  Types,
  ConnectionBase,
} = require('../../src');

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
      partIndex:      true,
    },
    'lastName': {
      type:       Types.STRING(64),
      allowNull:  true,
      partIndex:      true,
    },
  };
}

describe('QueryEngine', () => {
  let connection;
  let query;

  beforeAll(() => {
    connection = new ConnectionBase({
      models: [
        User,
      ],
    });
  });

  beforeEach(() => {
    query = new QueryEngine({
      connection,
    });
  });

  it('can construct a query context with a model sub-key', () => {
    let context = query.User.id.EQ('test')._getRawQuery();

    expect(context).toBeInstanceOf(Array);
    expect(context.length).toEqual(1);

    expect({
      modelName:  context[0].modelName,
      fieldName:  context[0].fieldName,
      operator:   context[0].operator,
      value:      context[0].value,
      and:        context[0].and,
      or:         context[0].or,
    }).toEqual({
      modelName:  'User',
      fieldName:  'id',
      operator:   'EQ',
      value:      'test',
      and:        true,
      or:         false,
    });
  });

  it('can construct a query context with a model call', () => {
    let context = query.User('id').EQ('test')._getRawQuery();

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
    let context = query.User.id.EQ(1).AND.firstName.EQ('Test').AND.NOT.lastName.EQ('Stuff')._getRawQuery();

    expect(context).toBeInstanceOf(Array);
    expect(context.length).toEqual(6);

    // 0
    expect({
      modelName:  context[0].modelName,
      fieldName:  context[0].fieldName,
      operator:   context[0].operator,
      value:      context[0].value,
      partIndex:  context[0].partIndex,
    }).toEqual({
      modelName:  'User',
      fieldName:  'id',
      operator:   'EQ',
      value:      1,
      partIndex:  0,
    });

    // 1
    expect({
      operator:   context[1].operator,
      partIndex:  context[1].partIndex,
    }).toEqual({
      operator:   'AND',
      partIndex:  1,
    });

    // 2
    expect({
      modelName:  context[2].modelName,
      fieldName:  context[2].fieldName,
      operator:   context[2].operator,
      value:      context[2].value,
      partIndex:  context[2].partIndex,
    }).toEqual({
      modelName:  'User',
      fieldName:  'firstName',
      operator:   'EQ',
      value:      'Test',
      partIndex:  2,
    });

    // 3
    expect({
      operator:   context[3].operator,
      partIndex:  context[3].partIndex,
    }).toEqual({
      operator:   'AND',
      partIndex:  3,
    });

    // 4
    expect({
      operator:   context[4].operator,
      partIndex:  context[4].partIndex,
    }).toEqual({
      operator:   'NOT',
      partIndex:  4,
    });

    // 5
    expect({
      modelName:  context[5].modelName,
      fieldName:  context[5].fieldName,
      operator:   context[5].operator,
      value:      context[5].value,
      partIndex:  context[5].partIndex,
    }).toEqual({
      modelName:  'User',
      fieldName:  'lastName',
      operator:   'EQ',
      value:      'Stuff',
      partIndex:  5,
    });
  });

  // it('can chain query field names', () => {
  //   let query = QueryEngine.query(User);
  //   let context = query.User('id').roles.name.EQ('admin').AND.NOT.roles.name.EQ('booger')._getRawQuery();

  //   expect(context).toBeInstanceOf(Array);
  //   expect(context.length).toEqual(6);

  //   // 0
  //   expect({
  //     modelName:  context[0].modelName,
  //     fieldName:  context[0].fieldName,
  //     operator:   context[0].operator,
  //     value:      context[0].value,
  //     partIndex:      context[0].partIndex,
  //   }).toEqual({
  //     modelName:  'User',
  //     fieldName:  'id',
  //     operator:   'EQ',
  //     value:      1,
  //     partIndex:      0,
  //   });
  // });

  // TODO: AND and OR need to have callbacks for grouping
  // TODO: Need to implement join
  // TODO: Need to implement project
  // TODO: Need to implement limit
  // TODO: Need to implement offset
  // TODO: Need to implement order
  // TODO: Need to implement include (load sub-models)
  // TODO: Need to implement all operators
});
