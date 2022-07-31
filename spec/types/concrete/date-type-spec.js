/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const moment    = require('moment');
const { Types } = require('../../../lib');

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
    let type = Types.DATE();
    let value = type.castToType({ value: '2001-01-01' });
    expect(value).toBeInstanceOf(moment);
    expect(value.toISOString()).toEqual('2001-01-01T00:00:00.000Z');

    value = type.castToType({ value: undefined });
    expect(value).toBe(undefined);

    value = type.castToType({ value: null });
    expect(value).toBe(null);
  });

  it('can cast to type with format', () => {
    let type = Types.DATE('MM.DD.YYYY');
    let value = type.castToType({ value: '07.23.2022' });
    expect(value).toBeInstanceOf(moment);
    expect(value.toISOString()).toEqual('2022-07-23T00:00:00.000Z');
    expect(type.serialize(value)).toEqual('07.23.2022');

    value = type.castToType({ value: undefined });
    expect(value).toBe(undefined);

    value = type.castToType({ value: null });
    expect(value).toBe(null);
  });

  it('should throw an error on bad datetime string', () => {
    let type = Types.DATE();
    expect(() => type.castToType({ value: 'derp' })).toThrow(new TypeError('DateType::deserialize: Value provided ("derp") can not be cast into a date.'));
  });
});
