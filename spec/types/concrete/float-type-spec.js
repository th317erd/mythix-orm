/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../../src');

describe('FloatType', () => {
  describe('toConnectionType', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.FloatType();
      expect(type.toConnectionType({ dialect: 'sqlite' })).toEqual('REAL');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.FloatType();
      expect(type.toConnectionType({ dialect: undefined })).toEqual('FLOAT');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.FloatType();
      expect(type.toString({ dialect: 'sqlite' })).toEqual('REAL');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.FloatType();
      expect(type.toString({ dialect: undefined })).toEqual('FLOAT');
    });
  });

  it('can construct from class', () => {
    let type = new Types.FloatType();
    expect(type.toString()).toEqual('FLOAT');
  });

  it('can construct from type helper', () => {
    let type = Types.FLOAT();
    expect(type.toString()).toEqual('FLOAT');
  });

  it('can cast to type', () => {
    let type = Types.FLOAT();
    let value = type.castToType({ value: '1234' });
    expect(typeof value).toEqual('number');
    expect(value).toEqual(1234);

    value = type.castToType({ value: '-1234.6' });
    expect(typeof value).toEqual('number');
    expect(value).toEqual(-1234.6);

    value = type.castToType({ value: undefined });
    expect(value).toBe(undefined);

    value = type.castToType({ value: null });
    expect(value).toBe(null);

    expect(() => type.castToType({ value: 'derp' })).toThrow(new TypeError('FloatType::castToType: Value provided ("derp") can not be cast into an floating point number.'));
  });
});
