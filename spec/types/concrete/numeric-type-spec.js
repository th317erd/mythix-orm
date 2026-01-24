/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Types, ConnectionBase } = require('../../../lib');

describe('NumericType', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('getDisplayName', () => {
    it('returns NUMERIC', () => {
      expect(Types.NumericType.getDisplayName()).toEqual('NUMERIC');
    });

    it('can be called on instance', () => {
      let type = new Types.NumericType();
      expect(type.getDisplayName()).toEqual('NUMERIC');
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.NumericType();
      expect(type.toConnectionType(connection)).toEqual('NUMERIC(20, 6)');
    });

    it('can convert to connection type with custom precision and scale', () => {
      let type = new Types.NumericType(10, 2);
      expect(type.toConnectionType(connection)).toEqual('NUMERIC(10, 2)');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.NumericType();
      expect(type.toConnectionType()).toEqual('NUMERIC');
    });
  });

  describe('toString', () => {
    it('can convert to string when connection is defined', () => {
      let type = new Types.NumericType();
      expect(type.toString(connection)).toEqual('NUMERIC(20, 6)');
    });

    it('can convert to string when connection is undefined', () => {
      let type = new Types.NumericType();
      expect(type.toString()).toEqual('NUMERIC');
    });
  });

  describe('constructor', () => {
    it('can construct from class with precision and scale', () => {
      let type = new Types.NumericType(15, 4);
      expect(type.precision).toBe(15);
      expect(type.scale).toBe(4);
    });

    it('can construct from type helper with precision and scale', () => {
      let type = Types.NUMERIC(12, 3);
      expect(type.precision).toBe(12);
      expect(type.scale).toBe(3);
    });

    it('defaults precision to 20 when not provided', () => {
      let type = new Types.NumericType();
      expect(type.precision).toBe(20);
    });

    it('defaults scale to 6 when not provided', () => {
      let type = new Types.NumericType();
      expect(type.scale).toBe(6);
    });

    it('defaults both precision and scale when type helper used without args', () => {
      let type = Types.NUMERIC();
      expect(type.precision).toBe(20);
      expect(type.scale).toBe(6);
    });
  });

  describe('castToType', () => {
    it('converts numeric strings to numbers', () => {
      let type = Types.NUMERIC();
      let value = type.castToType({ value: '123.456' });
      expect(typeof value).toEqual('number');
      expect(value).toBeCloseTo(123.456, 6);
    });

    it('passes through numbers', () => {
      let type = Types.NUMERIC();
      let value = type.castToType({ value: 99.99 });
      expect(value).toBeCloseTo(99.99, 6);
    });

    it('handles integer values', () => {
      let type = Types.NUMERIC();
      let value = type.castToType({ value: 42 });
      expect(value).toBe(42);
    });

    it('handles very large numbers', () => {
      let type = Types.NUMERIC();
      let value = type.castToType({ value: 12345678901234567890 });
      expect(typeof value).toEqual('number');
    });

    it('handles very small decimal values', () => {
      let type = Types.NUMERIC();
      let value = type.castToType({ value: 0.000001 });
      expect(value).toBeCloseTo(0.000001, 6);
    });

    it('handles negative numbers', () => {
      let type = Types.NUMERIC();
      let value = type.castToType({ value: -123.456 });
      expect(value).toBeCloseTo(-123.456, 6);
    });

    it('returns undefined when value is undefined', () => {
      let type = Types.NUMERIC();
      let value = type.castToType({ value: undefined });
      expect(value).toBe(undefined);
    });

    it('returns null when value is null', () => {
      let type = Types.NUMERIC();
      let value = type.castToType({ value: null });
      expect(value).toBe(null);
    });

    it('throws error for NaN values', () => {
      let type = Types.NUMERIC();
      expect(() => type.castToType({ value: NaN })).toThrow();
    });

    it('throws error for Infinity', () => {
      let type = Types.NUMERIC();
      expect(() => type.castToType({ value: Infinity })).toThrow();
      expect(() => type.castToType({ value: -Infinity })).toThrow();
    });

    it('throws error for non-numeric strings', () => {
      let type = Types.NUMERIC();
      expect(() => type.castToType({ value: 'not a number' })).toThrow();
    });

    it('has correct error message mentioning NumericType', () => {
      let type = Types.NUMERIC();
      try {
        type.castToType({ value: 'invalid' });
        fail('Expected to throw');
      } catch (e) {
        expect(e.message).toContain('NumericType::castToType');
      }
    });
  });

  describe('isValidValue', () => {
    it('returns true for finite numbers', () => {
      let type = Types.NUMERIC();
      expect(type.isValidValue(123)).toBe(true);
      expect(type.isValidValue(123.456)).toBe(true);
      expect(type.isValidValue(-99.99)).toBe(true);
      expect(type.isValidValue(0)).toBe(true);
    });

    it('returns false for NaN', () => {
      let type = Types.NUMERIC();
      expect(type.isValidValue(NaN)).toBe(false);
    });

    it('returns false for Infinity', () => {
      let type = Types.NUMERIC();
      expect(type.isValidValue(Infinity)).toBe(false);
      expect(type.isValidValue(-Infinity)).toBe(false);
    });

    it('returns false for non-number values', () => {
      let type = Types.NUMERIC();
      expect(type.isValidValue('123')).toBe(false);
      expect(type.isValidValue(null)).toBe(false);
      expect(type.isValidValue(undefined)).toBe(false);
      expect(type.isValidValue({})).toBe(false);
    });
  });
});
