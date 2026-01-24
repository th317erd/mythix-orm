/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Types, ConnectionBase } = require('../../../lib');

describe('TextType', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('getDisplayName', () => {
    it('returns TEXT', () => {
      expect(Types.TextType.getDisplayName()).toEqual('TEXT');
    });

    it('can be called on instance', () => {
      let type = new Types.TextType();
      expect(type.getDisplayName()).toEqual('TEXT');
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.TextType();
      expect(type.toConnectionType(connection)).toEqual('TEXT');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.TextType();
      expect(type.toConnectionType()).toEqual('TEXT');
    });
  });

  describe('toString', () => {
    it('can convert to string when connection is defined', () => {
      let type = new Types.TextType();
      expect(type.toString(connection)).toEqual('TEXT');
    });

    it('can convert to string when connection is undefined', () => {
      let type = new Types.TextType();
      expect(type.toString()).toEqual('TEXT');
    });
  });

  describe('constructor', () => {
    it('can construct from class', () => {
      let type = new Types.TextType(100000);
      expect(type.toString()).toEqual('TEXT');
      expect(type.length).toBe(100000);
    });

    it('can construct from type helper', () => {
      let type = Types.TEXT(200000);
      expect(type.toString()).toEqual('TEXT');
      expect(type.length).toBe(200000);
    });

    it('defaults length to 65565 when not provided', () => {
      let type = new Types.TextType();
      expect(type.length).toBe(65565);
    });

    it('defaults length to 65565 when type helper used without args', () => {
      let type = Types.TEXT();
      expect(type.length).toBe(65565);
    });
  });

  describe('castToType', () => {
    it('converts numbers to string', () => {
      let type = Types.TEXT();
      let value = type.castToType({ value: 123 });
      expect(typeof value).toEqual('string');
      expect(value).toEqual('123');
    });

    it('converts objects to string', () => {
      let type = Types.TEXT();
      let value = type.castToType({ value: {} });
      expect(typeof value).toEqual('string');
      expect(value).toEqual('[object Object]');
    });

    it('passes through strings', () => {
      let type = Types.TEXT();
      let value = type.castToType({ value: 'hello world' });
      expect(value).toEqual('hello world');
    });

    it('handles large text content', () => {
      let type = Types.TEXT();
      let largeText = 'a'.repeat(100000);
      let value = type.castToType({ value: largeText });
      expect(value).toEqual(largeText);
      expect(value.length).toBe(100000);
    });

    it('returns undefined when value is undefined', () => {
      let type = Types.TEXT();
      let value = type.castToType({ value: undefined });
      expect(value).toBe(undefined);
    });

    it('returns null when value is null', () => {
      let type = Types.TEXT();
      let value = type.castToType({ value: null });
      expect(value).toBe(null);
    });
  });

  describe('isValidValue', () => {
    it('returns true for strings', () => {
      let type = Types.TEXT();
      expect(type.isValidValue('test')).toBe(true);
      expect(type.isValidValue('')).toBe(true);
      expect(type.isValidValue('   ')).toBe(true);
      expect(type.isValidValue('a'.repeat(100000))).toBe(true);
    });

    it('returns true for String objects', () => {
      let type = Types.TEXT();
      // eslint-disable-next-line no-new-wrappers
      expect(type.isValidValue(new String('test'))).toBe(true);
    });

    it('returns false for non-string values', () => {
      let type = Types.TEXT();
      expect(type.isValidValue(123)).toBe(false);
      expect(type.isValidValue(null)).toBe(false);
      expect(type.isValidValue(undefined)).toBe(false);
      expect(type.isValidValue({})).toBe(false);
      expect(type.isValidValue([])).toBe(false);
    });
  });
});
