/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../../src');

describe('StringType', () => {
  it('can construct from class', () => {
    let type = new Types.StringType(123);
    expect(type.toString()).toEqual('VARCHAR(123)');
    expect(type.length).toBe(123);
  });

  it('can construct from type helper', () => {
    let type = Types.STRING(123);
    expect(type.toString()).toEqual('VARCHAR(123)');
    expect(type.length).toBe(123);
  });

  it('can cast to type', () => {
    let value = Types.STRING.castToType({ value: 123 });
    expect(typeof value).toEqual('string');
    expect(value).toEqual('123');

    value = Types.STRING.castToType({ value: {} });
    expect(typeof value).toEqual('string');
    expect(value).toEqual('[object Object]');

    value = Types.STRING.castToType({ value: undefined });
    expect(value).toBe(undefined);

    value = Types.STRING.castToType({ value: null });
    expect(value).toBe(null);
  });
});
