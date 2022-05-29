/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../../src');

describe('DateType', () => {
  describe('toConnectionType', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.DateType();
      expect(type.toConnectionType({ dialect: 'sqlite' })).toEqual('DATE');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.DateType();
      expect(type.toConnectionType({ dialect: undefined })).toEqual('DATE');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.DateType();
      expect(type.toString({ dialect: 'sqlite' })).toEqual('DATE');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.DateType();
      expect(type.toString({ dialect: undefined })).toEqual('DATE');
    });
  });

  it('can construct from class', () => {
    let type = new Types.DateType();
    expect(type.toString()).toEqual('DATE');
  });

  it('can construct from type helper', () => {
    let type = Types.DATE();
    expect(type.toString()).toEqual('DATE');
  });

  it('can cast to type', () => {
    let value = Types.DATE.castToType({ value: '2001-01-01' });
    expect(value).toBeInstanceOf(Date);
    expect(value.toISOString()).toEqual('2001-01-01T00:00:00.000Z');

    value = Types.DATE.castToType({ value: undefined });
    expect(value).toBe(undefined);

    value = Types.DATE.castToType({ value: null });
    expect(value).toBe(null);
  });

  it('should throw an error on bad datetime string', () => {
    expect(() => Types.DATE.castToType({ value: 'derp' })).toThrow(new TypeError('DateType::castToType: Value provided ("derp") can not be cast into a date.'));
  });
});
