/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../../src');

describe('BooleanType', () => {
  describe('toConnectionType', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.BooleanType();
      expect(type.toConnectionType({ dialect: 'sqlite' })).toEqual('BOOLEAN');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.BooleanType();
      expect(type.toConnectionType({ dialect: undefined })).toEqual('BOOLEAN');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.BooleanType();
      expect(type.toString({ dialect: 'sqlite' })).toEqual('BOOLEAN');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.BooleanType();
      expect(type.toString({ dialect: undefined })).toEqual('BOOLEAN');
    });
  });

  it('can construct from class', () => {
    let type = new Types.BooleanType();
    expect(type.toString()).toEqual('BOOLEAN');
  });

  it('can construct from type helper', () => {
    let type = Types.BOOLEAN();
    expect(type.toString()).toEqual('BOOLEAN');
  });

  it('can cast to type', () => {
    let type = Types.BOOLEAN();
    let value = type.castToType({ value: 1 });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(true);

    value = type.castToType({ value: 0 });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(false);

    value = type.castToType({ value: -1 });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(true);

    value = type.castToType({ value: false });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(false);

    value = type.castToType({ value: true });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(true);

    value = type.castToType({ value: 'false' });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(false);

    value = type.castToType({ value: 'true' });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(true);
  });
});
