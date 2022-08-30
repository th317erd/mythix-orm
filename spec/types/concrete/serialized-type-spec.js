/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll, fail */

const { Types, ConnectionBase } = require('../../../lib');

describe('SerializedType', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('constructor', () => {
    it('should throw an error if a inner "type" is not defined', () => {
      try {
        new Types.SerializedType();
        fail('unreachable');
      } catch (error) {
        expect(error.message).toMatch(/"type" must be specified on the "options" for this type/);
      }
    });

    it('should except a type as options', () => {
      try {
        let type = new Types.SerializedType(Types.STRING(10));
        expect(type.getOptions().type.toString()).toEqual('VARCHAR(10)');
      } catch (error) {
        fail('unreachable');
      }
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.SerializedType(Types.STRING(10));
      expect(type.toConnectionType(connection)).toEqual('VARCHAR(10)');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.SerializedType(Types.STRING(10));
      expect(type.toConnectionType()).toEqual('VARCHAR(10)');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.SerializedType(Types.STRING(10));
      expect(type.toString(connection)).toEqual('VARCHAR(10)');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.SerializedType({ type: Types.STRING(10) });
      expect(type.toString()).toEqual('VARCHAR(10)');
    });
  });

  it('can construct from class', () => {
    let type = new Types.SerializedType({ type: Types.STRING(10) });
    expect(type.toString()).toEqual('VARCHAR(10)');
  });

  it('can construct from type helper', () => {
    let type = Types.SERIALIZED({ type: Types.STRING(10) });
    expect(type.toString()).toEqual('VARCHAR(10)');
  });

  it('can cast to type', () => {
    let type = Types.SERIALIZED(Types.STRING(100));
    let value = type.castToType({ value: '{ "hello": "world" }' });
    expect(typeof value).toEqual('object');
    expect(value).toEqual({ hello: 'world' });

    value = type.castToType({ value: undefined });
    expect(value).toBe(undefined);

    value = type.castToType({ value: null });
    expect(value).toBe(null);
  });

  it('can properly serialize a value', () => {
    let type = Types.SERIALIZED(Types.STRING(100));
    let value = type.serialize({ hello: 'world' }, {});
    expect(typeof value).toEqual('string');
    expect(value).toEqual('{"hello":"world"}');

    value = type.serialize('derp', {});
    expect(typeof value).toEqual('string');
    expect(value).toEqual('derp');
  });

  it('can properly deserialize a value', () => {
    let type = Types.SERIALIZED(Types.STRING(100));
    let value = type.deserialize('{ "hello": "world" }');
    expect(typeof value).toEqual('object');
    expect(value).toEqual({ hello: 'world' });

    value = type.deserialize({ hello: 'world' });
    expect(typeof value).toEqual('object');
    expect(value).toEqual({ hello: 'world' });
  });
});
