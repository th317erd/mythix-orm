/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const moment    = require('moment');
const { Types, ConnectionBase } = require('../../../lib');

describe('DateTimeType', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.DateTimeType();
      expect(type.toConnectionType(connection)).toEqual('DATETIME');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.DateTimeType();
      expect(type.toConnectionType()).toEqual('DATETIME');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.DateTimeType();
      expect(type.toString(connection)).toEqual('DATETIME');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.DateTimeType();
      expect(type.toString()).toEqual('DATETIME');
    });
  });

  it('can construct from class', () => {
    let type = new Types.DateTimeType();
    expect(type.toString()).toEqual('DATETIME');
    expect(type.length).toBe(null);
  });

  it('can construct from type helper', () => {
    let type = Types.DATETIME(null, 6);
    expect(type.toString()).toEqual('DATETIME');
    expect(type.length).toBe(6);
  });

  it('can cast to type', () => {
    let type = Types.DATETIME();
    let value = type.castToType({ value: '2001-01-01' });
    expect(value).toBeInstanceOf(moment);
    expect(value.utc(true).toISOString()).toEqual('2001-01-01T00:00:00.000Z');

    value = type.castToType({ value: undefined });
    expect(value).toBe(undefined);

    value = type.castToType({ value: null });
    expect(value).toBe(null);
  });

  it('can cast to type with format', () => {
    let type = Types.DATETIME('MM.DD.YYYY / HH:mm:ss.SSS');
    let value = type.castToType({ value: '07.23.2022 / 10:15:45.125' });
    expect(value).toBeInstanceOf(moment);
    expect(value.utc(true).toISOString()).toEqual('2022-07-23T10:15:45.125Z');
    expect(type.serialize(value)).toEqual('07.23.2022 / 10:15:45.125');

    value = type.castToType({ value: undefined });
    expect(value).toBe(undefined);

    value = type.castToType({ value: null });
    expect(value).toBe(null);
  });

  it('should throw an error on bad datetime string', () => {
    let type = Types.DATETIME();
    expect(() => type.castToType({ value: 'derp' })).toThrow(new TypeError('DateType::deserialize: Value provided ("derp") can not be cast into a date.'));
  });
});
