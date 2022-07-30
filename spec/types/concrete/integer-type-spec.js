/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../../lib');

describe('IntegerType', () => {
  describe('toConnectionType', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.IntegerType();
      expect(type.toConnectionType({ dialect: 'sqlite' })).toEqual('INTEGER');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.IntegerType();
      expect(type.toConnectionType({ dialect: undefined })).toEqual('INTEGER');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.IntegerType();
      expect(type.toString({ dialect: 'sqlite' })).toEqual('INTEGER');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.IntegerType();
      expect(type.toString({ dialect: undefined })).toEqual('INTEGER');
    });
  });

  it('can construct from class', () => {
    let type = new Types.IntegerType();
    expect(type.toString()).toEqual('INTEGER');
  });

  it('can construct from type helper', () => {
    let type = Types.INTEGER();
    expect(type.toString()).toEqual('INTEGER');
  });

  it('can cast to type', () => {
    let type = Types.INTEGER();
    let value = type.castToType({ value: '1234' });
    expect(typeof value).toEqual('number');
    expect(value).toEqual(1234);

    value = type.castToType({ value: '-1234.6' });
    expect(typeof value).toEqual('number');
    expect(value).toEqual(-1235);

    value = type.castToType({ value: undefined });
    expect(value).toBe(undefined);

    value = type.castToType({ value: null });
    expect(value).toBe(null);

    expect(() => type.castToType({ value: 'derp' })).toThrow(new TypeError('IntegerType::castToType: Value provided ("derp") can not be cast into an integer.'));
  });
});
