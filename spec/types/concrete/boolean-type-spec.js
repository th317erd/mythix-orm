/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../../src');

describe('BooleanType', () => {
  it('can construct from class', () => {
    let type = new Types.BooleanType();
    expect(type.toString()).toEqual('BOOLEAN');
  });

  it('can construct from type helper', () => {
    let type = Types.BOOLEAN();
    expect(type.toString()).toEqual('BOOLEAN');
  });

  it('can cast to type', () => {
    let value = Types.BOOLEAN.castToType({ value: 1 });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(true);

    value = Types.BOOLEAN.castToType({ value: 0 });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(false);

    value = Types.BOOLEAN.castToType({ value: -1 });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(true);

    value = Types.BOOLEAN.castToType({ value: false });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(false);

    value = Types.BOOLEAN.castToType({ value: true });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(true);
  });
});
