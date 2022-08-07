/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Types, ConnectionBase } = require('../../../lib');

describe('BigIntType', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection is defined', () => {
      let type = new Types.BigIntType();
      expect(type.toConnectionType(connection)).toEqual('BIGINT');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.BigIntType(6);
      expect(type.toConnectionType()).toEqual('BIGINT(6)');
    });
  });

  describe('toString', () => {
    it('can convert to connection is defined', () => {
      let type = new Types.BigIntType();
      expect(type.toString(connection)).toEqual('BIGINT');
    });

    it('can convert to connection type when connection undefined', () => {
      let type = new Types.BigIntType(6);
      expect(type.toString()).toEqual('BIGINT(6)');
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
    expect(typeof value).toEqual('number');
    expect(value).toEqual(1234);

    expect(() => type.castToType({ value: 'derp' })).toThrow(new SyntaxError('Cannot convert derp to a BigInt'));
  });

  it('can cast to type (strict)', () => {
    let type = Types.BIGINT(null, { strict: true });
    let value = type.castToType({ value: '1234' });
    expect(typeof value).toEqual('bigint');
    expect(value).toEqual(BigInt(1234));

    expect(() => type.castToType({ value: 'derp' })).toThrow(new SyntaxError('Cannot convert derp to a BigInt'));
  });
});
