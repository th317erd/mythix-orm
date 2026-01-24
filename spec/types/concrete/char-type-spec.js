/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Types, ConnectionBase } = require('../../../lib');

describe('CharType', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('getDisplayName', () => {
    it('returns CHAR', () => {
      expect(Types.CharType.getDisplayName()).toEqual('CHAR');
    });

    it('can be called on instance', () => {
      let type = new Types.CharType();
      expect(type.getDisplayName()).toEqual('CHAR');
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.CharType();
      expect(type.toConnectionType(connection)).toEqual('CHAR');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.CharType();
      expect(type.toConnectionType()).toEqual('CHAR');
    });
  });

  describe('toString', () => {
    it('can convert to string when connection is defined', () => {
      let type = new Types.CharType();
      expect(type.toString(connection)).toEqual('CHAR');
    });

    it('can convert to string when connection is undefined', () => {
      let type = new Types.CharType();
      expect(type.toString()).toEqual('CHAR');
    });
  });

  describe('constructor', () => {
    it('can construct from class', () => {
      let type = new Types.CharType();
      expect(type.toString()).toEqual('CHAR');
    });

    it('can construct from type helper', () => {
      let type = Types.CHAR();
      expect(type.toString()).toEqual('CHAR');
    });
  });

  describe('castToType', () => {
    it('returns single character unchanged', () => {
      let type = Types.CHAR();
      let value = type.castToType({ value: 'a' });
      expect(value).toEqual('a');
    });

    it('extracts first character from string', () => {
      let type = Types.CHAR();
      let value = type.castToType({ value: 'x' });
      expect(value).toEqual('x');
    });

    it('returns undefined when value is undefined', () => {
      let type = Types.CHAR();
      let value = type.castToType({ value: undefined });
      expect(value).toBe(undefined);
    });

    it('returns null when value is null', () => {
      let type = Types.CHAR();
      let value = type.castToType({ value: null });
      expect(value).toBe(null);
    });

    it('throws error for multi-character strings', () => {
      let type = Types.CHAR();
      expect(() => type.castToType({ value: 'ab' })).toThrow();
      expect(() => type.castToType({ value: 'hello' })).toThrow();
    });

    it('throws error for empty strings', () => {
      let type = Types.CHAR();
      expect(() => type.castToType({ value: '' })).toThrow();
    });

    it('throws error for non-string values', () => {
      let type = Types.CHAR();
      expect(() => type.castToType({ value: 123 })).toThrow();
      expect(() => type.castToType({ value: {} })).toThrow();
      expect(() => type.castToType({ value: [] })).toThrow();
    });
  });

  describe('isValidValue', () => {
    it('returns true for single character strings', () => {
      let type = Types.CHAR();
      expect(type.isValidValue('a')).toBe(true);
      expect(type.isValidValue('Z')).toBe(true);
      expect(type.isValidValue('1')).toBe(true);
      expect(type.isValidValue(' ')).toBe(true);
    });

    it('returns false for multi-character strings', () => {
      let type = Types.CHAR();
      expect(type.isValidValue('ab')).toBe(false);
      expect(type.isValidValue('hello')).toBe(false);
    });

    it('returns false for empty strings', () => {
      let type = Types.CHAR();
      expect(type.isValidValue('')).toBe(false);
    });

    it('returns false for non-string values', () => {
      let type = Types.CHAR();
      expect(type.isValidValue(1)).toBe(false);
      expect(type.isValidValue(null)).toBe(false);
      expect(type.isValidValue(undefined)).toBe(false);
      expect(type.isValidValue({})).toBe(false);
    });
  });
});
