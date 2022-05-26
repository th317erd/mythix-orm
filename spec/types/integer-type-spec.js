/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../src');

describe('IntegerType', () => {
  it('can construct from class', () => {
    let type = new Types.IntegerType();
    expect(type.toString()).toEqual('INTEGER');
  });

  it('can construct from type helper', () => {
    let type = Types.INTEGER();
    expect(type.toString()).toEqual('INTEGER');
  });

  it('can cast to type', () => {
    let value = Types.INTEGER.castToType('1234');
    expect(typeof value).toEqual('number');
    expect(value).toEqual(1234);

    value = Types.INTEGER.castToType('-1234.6');
    expect(typeof value).toEqual('number');
    expect(value).toEqual(-1235);

    expect(() => Types.INTEGER.castToType('derp')).toThrow(new TypeError('IntegerType::castToType: Value provided ("derp") can not be cast into an integer.'));
  });
});
