/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Types, ConnectionBase } = require('../../../lib');
const { XID_REGEXP } = require('../../support/test-helpers');

describe('XIDType', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('getDisplayName', () => {
    it('returns XID', () => {
      expect(Types.XIDType.getDisplayName()).toEqual('XID');
    });

    it('can be called on instance', () => {
      let type = new Types.XIDType();
      expect(type.getDisplayName()).toEqual('XID');
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.XIDType();
      expect(type.toConnectionType(connection)).toEqual('VARCHAR(20)');
    });

    it('can convert to connection type with prefix', () => {
      let type = new Types.XIDType({ prefix: 'USER_' });
      expect(type.toConnectionType(connection)).toEqual('VARCHAR(25)');
    });
  });

  describe('toString', () => {
    it('can convert to string when connection is defined', () => {
      let type = new Types.XIDType();
      expect(type.toString(connection)).toEqual('VARCHAR(20)');
    });

    it('can convert to string when connection is undefined', () => {
      let type = new Types.XIDType();
      expect(type.toString()).toEqual('VARCHAR(20)');
    });

    it('includes prefix length in VARCHAR size', () => {
      let type = new Types.XIDType({ prefix: 'TEST_' });
      expect(type.toString()).toEqual('VARCHAR(25)');
    });
  });

  describe('constructor', () => {
    it('can construct from class', () => {
      let type = new Types.XIDType();
      expect(type.toString()).toEqual('VARCHAR(20)');
    });

    it('can construct from class with prefix', () => {
      let type = new Types.XIDType({ prefix: 'USR_' });
      expect(type.getPrefix()).toEqual('USR_');
    });

    it('can construct from type helper', () => {
      let type = Types.XID();
      expect(type.toString()).toEqual('VARCHAR(20)');
    });

    it('can construct from type helper with prefix', () => {
      let type = Types.XID({ prefix: 'ORG_' });
      expect(type.getPrefix()).toEqual('ORG_');
      expect(type.toString()).toEqual('VARCHAR(24)');
    });
  });

  describe('getPrefix', () => {
    it('returns empty string when no prefix', () => {
      let type = new Types.XIDType();
      expect(type.getPrefix()).toEqual('');
    });

    it('returns configured prefix', () => {
      let type = new Types.XIDType({ prefix: 'ITEM_' });
      expect(type.getPrefix()).toEqual('ITEM_');
    });
  });

  describe('getBaseLength', () => {
    it('returns 20 (XID length)', () => {
      let type = new Types.XIDType();
      expect(type.getBaseLength()).toBe(20);
    });
  });

  describe('getTotalLength', () => {
    it('returns 20 without prefix', () => {
      let type = new Types.XIDType();
      expect(type.getTotalLength()).toBe(20);
    });

    it('returns base + prefix length with prefix', () => {
      let type = new Types.XIDType({ prefix: 'USER_' });
      expect(type.getTotalLength()).toBe(25);
    });
  });

  describe('castToType', () => {
    it('returns valid XID unchanged', () => {
      let type = Types.XID();
      let validXid = 'c7s4gdr0hm8dnbjgmm70';
      let value = type.castToType({ value: validXid });
      expect(value).toEqual(validXid);
    });

    it('adds prefix to unprefixed XID', () => {
      let type = Types.XID({ prefix: 'USER_' });
      let validXid = 'c7s4gdr0hm8dnbjgmm70';
      let value = type.castToType({ value: validXid });
      expect(value).toEqual('USER_c7s4gdr0hm8dnbjgmm70');
    });

    it('returns prefixed XID unchanged when prefix matches', () => {
      let type = Types.XID({ prefix: 'USER_' });
      let prefixedXid = 'USER_c7s4gdr0hm8dnbjgmm70';
      let value = type.castToType({ value: prefixedXid });
      expect(value).toEqual(prefixedXid);
    });

    it('returns undefined when value is undefined', () => {
      let type = Types.XID();
      let value = type.castToType({ value: undefined });
      expect(value).toBe(undefined);
    });

    it('returns null when value is null', () => {
      let type = Types.XID();
      let value = type.castToType({ value: null });
      expect(value).toBe(null);
    });

    it('throws error for invalid XID format', () => {
      let type = Types.XID();
      expect(() => type.castToType({ value: 'invalid' })).toThrow();
      expect(() => type.castToType({ value: '12345' })).toThrow();
      expect(() => type.castToType({ value: 'ILOU0123456789012345' })).toThrow();
    });

    it('throws error for non-string values', () => {
      let type = Types.XID();
      expect(() => type.castToType({ value: 123 })).toThrow();
      expect(() => type.castToType({ value: {} })).toThrow();
    });
  });

  describe('isValidValue', () => {
    it('returns true for valid base32 XID format', () => {
      let type = Types.XID();
      expect(type.isValidValue('c7s4gdr0hm8dnbjgmm70')).toBe(true);
      expect(type.isValidValue('00000000000000000000')).toBe(true);
    });

    it('returns true for prefixed valid XID', () => {
      let type = Types.XID({ prefix: 'USER_' });
      expect(type.isValidValue('USER_c7s4gdr0hm8dnbjgmm70')).toBe(true);
    });

    it('returns false for invalid characters (I, L, O, U are excluded)', () => {
      let type = Types.XID();
      expect(type.isValidValue('ilouilouilouilouxxxx')).toBe(false);
      expect(type.isValidValue('ILOUILOUILOUILOUXXXX')).toBe(false);
    });

    it('returns false for wrong length', () => {
      let type = Types.XID();
      expect(type.isValidValue('c7s4gdr0hm8dnbjgmm7')).toBe(false);
    });

    it('returns false for non-string values', () => {
      let type = Types.XID();
      expect(type.isValidValue(123)).toBe(false);
      expect(type.isValidValue(null)).toBe(false);
      expect(type.isValidValue(undefined)).toBe(false);
    });
  });

  describe('Default.XID', () => {
    it('generates valid XID when used as default', () => {
      let type = Types.XID();
      let mockContext = {
        field: {
          type: type,
        },
      };
      let defaultFn = Types.XID.Default.XID;
      let generatedXid = defaultFn(mockContext);
      expect(generatedXid).toMatch(XID_REGEXP);
    });

    it('generates XID with prefix when configured', () => {
      let type = Types.XID({ prefix: 'TEST_' });
      let mockContext = {
        field: {
          type: type,
        },
      };
      let defaultFn = Types.XID.Default.XID;
      let generatedXid = defaultFn(mockContext);
      expect(generatedXid).toMatch(/^TEST_[0-9abcdefghjkmnpqrstvwxyz]{20}$/);
    });
  });
});
