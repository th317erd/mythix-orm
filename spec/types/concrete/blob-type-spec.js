/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll, Buffer */

const { Types, ConnectionBase } = require('../../../lib');

describe('BlobType', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('getDisplayName', () => {
    it('returns BLOB', () => {
      expect(Types.BlobType.getDisplayName()).toEqual('BLOB');
    });

    it('can be called on instance', () => {
      let type = new Types.BlobType();
      expect(type.getDisplayName()).toEqual('BLOB');
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.BlobType();
      expect(type.toConnectionType(connection)).toEqual('BLOB');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.BlobType();
      expect(type.toConnectionType()).toEqual('BLOB');
    });
  });

  describe('toString', () => {
    it('can convert to string when connection is defined', () => {
      let type = new Types.BlobType();
      expect(type.toString(connection)).toEqual('BLOB');
    });

    it('can convert to string when connection is undefined', () => {
      let type = new Types.BlobType();
      expect(type.toString()).toEqual('BLOB');
    });
  });

  describe('constructor', () => {
    it('can construct from class', () => {
      let type = new Types.BlobType(1024);
      expect(type.toString()).toEqual('BLOB');
      expect(type.length).toBe(1024);
    });

    it('can construct from type helper', () => {
      let type = Types.BLOB(2048);
      expect(type.toString()).toEqual('BLOB');
      expect(type.length).toBe(2048);
    });

    it('defaults length to null when not provided', () => {
      let type = new Types.BlobType();
      expect(type.length).toBeNull();
    });
  });

  describe('castToType', () => {
    it('returns Buffer unchanged', () => {
      let type = Types.BLOB();
      let buffer = Buffer.from('test data');
      let value = type.castToType({ value: buffer });
      expect(Buffer.isBuffer(value)).toBe(true);
      expect(value).toBe(buffer);
    });

    it('returns undefined when value is undefined', () => {
      let type = Types.BLOB();
      let value = type.castToType({ value: undefined });
      expect(value).toBe(undefined);
    });

    it('returns null when value is null', () => {
      let type = Types.BLOB();
      let value = type.castToType({ value: null });
      expect(value).toBe(null);
    });

    it('throws error for non-Buffer values', () => {
      let type = Types.BLOB();
      expect(() => type.castToType({ value: 'not a buffer' })).toThrow();
      expect(() => type.castToType({ value: 123 })).toThrow();
      expect(() => type.castToType({ value: {} })).toThrow();
      expect(() => type.castToType({ value: [] })).toThrow();
    });
  });

  describe('isValidValue', () => {
    it('returns true for Buffer', () => {
      let type = Types.BLOB();
      expect(type.isValidValue(Buffer.from('test'))).toBe(true);
      expect(type.isValidValue(Buffer.alloc(0))).toBe(true);
    });

    it('returns false for non-Buffer values', () => {
      let type = Types.BLOB();
      expect(type.isValidValue('string')).toBe(false);
      expect(type.isValidValue(123)).toBe(false);
      expect(type.isValidValue(null)).toBe(false);
      expect(type.isValidValue(undefined)).toBe(false);
      expect(type.isValidValue({})).toBe(false);
      expect(type.isValidValue([])).toBe(false);
    });
  });
});
