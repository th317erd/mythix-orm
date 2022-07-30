/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../../lib');

describe('DateTimeType', () => {
  describe('toConnectionType', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.DateTimeType();
      expect(type.toConnectionType({ dialect: 'sqlite' })).toEqual('DATETIME');
    });

    it('can convert to connection type when dialect is "mysql" without length', () => {
      let type = new Types.DateTimeType();
      expect(type.toConnectionType({ dialect: 'mysql' })).toEqual('DATETIME');
    });

    it('can convert to connection type when dialect is "mysql" with length', () => {
      let type = new Types.DateTimeType(5);
      expect(type.toConnectionType({ dialect: 'mysql' })).toEqual('DATETIME(5)');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.DateTimeType();
      expect(type.toConnectionType({ dialect: undefined })).toEqual('DATETIME');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.DateTimeType();
      expect(type.toString({ dialect: 'sqlite' })).toEqual('DATETIME');
    });

    it('can convert to connection type when dialect is "mysql" without length', () => {
      let type = new Types.DateTimeType();
      expect(type.toString({ dialect: 'mysql' })).toEqual('DATETIME');
    });

    it('can convert to connection type when dialect is "mysql" with length', () => {
      let type = new Types.DateTimeType(5);
      expect(type.toString({ dialect: 'mysql' })).toEqual('DATETIME(5)');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.DateTimeType();
      expect(type.toString({ dialect: undefined })).toEqual('DATETIME');
    });
  });

  it('can construct from class', () => {
    let type = new Types.DateTimeType();
    expect(type.toString()).toEqual('DATETIME');
    expect(type.length).toBe(null);
  });

  it('can construct from type helper', () => {
    let type = Types.DATETIME(6);
    expect(type.toString()).toEqual('DATETIME');
    expect(type.length).toBe(6);
  });

  it('can cast to type', () => {
    let type = Types.DATETIME();
    let value = type.castToType({ value: '2001-01-01' });
    expect(value).toBeInstanceOf(Date);
    expect(value.toISOString()).toEqual('2001-01-01T00:00:00.000Z');

    value = type.castToType({ value: undefined });
    expect(value).toBe(undefined);

    value = type.castToType({ value: null });
    expect(value).toBe(null);
  });

  it('should throw an error on bad datetime string', () => {
    let type = Types.DATETIME();
    expect(() => type.castToType({ value: 'derp' })).toThrow(new TypeError('DateTimeType::castToType: Value provided ("derp") can not be cast into a date.'));
  });
});
