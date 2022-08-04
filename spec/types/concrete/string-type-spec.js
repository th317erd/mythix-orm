/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types, ConnectionBase } = require('../../../lib');

describe('StringType', () => {
  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.StringType();
      expect(type.toConnectionType(new ConnectionBase())).toEqual('VARCHAR(256)');
    });

    it('can convert to connection type when connection is defined with length', () => {
      let type = new Types.StringType(64);
      expect(type.toConnectionType(new ConnectionBase())).toEqual('VARCHAR(64)');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.StringType();
      expect(type.toConnectionType()).toEqual('VARCHAR(256)');
    });

    it('can convert to connection type when connection is undefined with length', () => {
      let type = new Types.StringType(64);
      expect(type.toConnectionType()).toEqual('VARCHAR(64)');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.StringType();
      expect(type.toString(new ConnectionBase())).toEqual('VARCHAR(256)');
    });

    it('can convert to connection type when connection is defined with length', () => {
      let type = new Types.StringType(64);
      expect(type.toString(new ConnectionBase())).toEqual('VARCHAR(64)');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.StringType();
      expect(type.toString()).toEqual('VARCHAR(256)');
    });

    it('can convert to connection type when connection is undefined with length', () => {
      let type = new Types.StringType(64);
      expect(type.toString()).toEqual('VARCHAR(64)');
    });
  });

  it('can construct from class', () => {
    let type = new Types.StringType(123);
    expect(type.toString()).toEqual('VARCHAR(123)');
    expect(type.length).toBe(123);
  });

  it('can construct from type helper', () => {
    let type = Types.STRING(123);
    expect(type.toString()).toEqual('VARCHAR(123)');
    expect(type.length).toBe(123);
  });

  it('can cast to type', () => {
    let type = Types.STRING(123);
    let value = type.castToType({ value: 123 });
    expect(typeof value).toEqual('string');
    expect(value).toEqual('123');

    value = type.castToType({ value: {} });
    expect(typeof value).toEqual('string');
    expect(value).toEqual('[object Object]');

    value = type.castToType({ value: undefined });
    expect(value).toBe(undefined);

    value = type.castToType({ value: null });
    expect(value).toBe(null);
  });
});
