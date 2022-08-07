/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Types, ConnectionBase } = require('../../../lib');

describe('IntegerType', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.IntegerType();
      expect(type.toConnectionType(connection)).toEqual('INTEGER');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.IntegerType();
      expect(type.toConnectionType()).toEqual('INTEGER');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.IntegerType();
      expect(type.toString(connection)).toEqual('INTEGER');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.IntegerType();
      expect(type.toString()).toEqual('INTEGER');
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
