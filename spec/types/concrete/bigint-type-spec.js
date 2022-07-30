/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../../lib');

describe('BigIntType', () => {
  describe('toConnectionType', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.BigIntType();
      expect(type.toConnectionType({ dialect: 'sqlite' })).toEqual('BIGINT');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.BigIntType(6);
      expect(type.toConnectionType({ dialect: undefined })).toEqual('BIGINT(6)');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.BigIntType();
      expect(type.toString({ dialect: 'sqlite' })).toEqual('BIGINT');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.BigIntType(6);
      expect(type.toString({ dialect: undefined })).toEqual('BIGINT(6)');
    });
  });

  it('can construct from class', () => {
    let type = new Types.BigIntType();
    expect(type.toString()).toEqual('BIGINT');
    expect(type.length).toBe(null);
  });

  it('can construct from class with length', () => {
    let type = new Types.BigIntType(11);
    expect(type.toString()).toEqual('BIGINT(11)');
    expect(type.length).toEqual(11);
  });

  it('can construct from type helper', () => {
    let type = Types.BIGINT();
    expect(type.toString()).toEqual('BIGINT');
    expect(type.length).toBe(null);
  });

  it('can construct from type helper with length', () => {
    let type = Types.BIGINT(11);
    expect(type.toString()).toEqual('BIGINT(11)');
    expect(type.length).toEqual(11);
  });

  it('can cast to type', () => {
    let type = Types.BIGINT();
    let value = type.castToType({ value: '1234' });
    expect(typeof value).toEqual('bigint');
    expect(value).toEqual(BigInt(1234));

    expect(() => type.castToType({ value: 'derp' })).toThrow(new SyntaxError('Cannot convert derp to a BigInt'));
  });
});
