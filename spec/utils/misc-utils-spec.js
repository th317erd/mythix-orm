/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { DateTime } = require('luxon');
const { Utils } = require('../../lib');
const { MiscUtils } = Utils;

describe('MiscUtils', () => {
  describe('collect', () => {
    it('collects items from async iterator', async () => {
      async function* generator() {
        yield 1;
        yield 2;
        yield 3;
      }

      let result = await MiscUtils.collect(generator());
      expect(result).toEqual([ 1, 2, 3 ]);
    });

    it('returns empty array for empty iterator', async () => {
      async function* generator() {
        // empty
      }

      let result = await MiscUtils.collect(generator());
      expect(result).toEqual([]);
    });

    it('collects objects from async iterator', async () => {
      async function* generator() {
        yield { id: 1 };
        yield { id: 2 };
      }

      let result = await MiscUtils.collect(generator());
      expect(result).toEqual([ { id: 1 }, { id: 2 } ]);
    });
  });

  describe('valueToDateTime', () => {
    it('returns DateTime unchanged if already a DateTime', () => {
      let dt = DateTime.now();
      let result = MiscUtils.valueToDateTime(dt);
      expect(result).toBe(dt);
    });

    it('converts number timestamp to DateTime', () => {
      let timestamp = 1704067200000; // 2024-01-01 00:00:00 UTC
      let result = MiscUtils.valueToDateTime(timestamp);
      expect(DateTime.isDateTime(result)).toBe(true);
      expect(result.toMillis()).toBe(timestamp);
    });

    it('converts bigint timestamp to DateTime', () => {
      let timestamp = BigInt(1704067200000);
      let result = MiscUtils.valueToDateTime(timestamp);
      expect(DateTime.isDateTime(result)).toBe(true);
      expect(result.toMillis()).toBe(Number(timestamp));
    });

    it('converts Date instance to DateTime', () => {
      let date = new Date('2024-01-01T00:00:00Z');
      let result = MiscUtils.valueToDateTime(date);
      expect(DateTime.isDateTime(result)).toBe(true);
      expect(result.toJSDate().getTime()).toBe(date.getTime());
    });

    it('converts ISO string to DateTime', () => {
      let isoString = '2024-01-01T12:30:00.000Z';
      let result = MiscUtils.valueToDateTime(isoString);
      expect(DateTime.isDateTime(result)).toBe(true);
      expect(result.toISO()).toContain('2024-01-01');
    });

    it('converts numeric string to DateTime', () => {
      let numericString = '1704067200000';
      let result = MiscUtils.valueToDateTime(numericString);
      expect(DateTime.isDateTime(result)).toBe(true);
      expect(result.toMillis()).toBe(1704067200000);
    });

    it('converts string with custom format to DateTime', () => {
      let dateString = '01-15-2024';
      let format = 'MM-dd-yyyy';
      let result = MiscUtils.valueToDateTime(dateString, format);
      expect(DateTime.isDateTime(result)).toBe(true);
      expect(result.month).toBe(1);
      expect(result.day).toBe(15);
      expect(result.year).toBe(2024);
    });

    it('throws error for invalid value', () => {
      expect(() => MiscUtils.valueToDateTime({})).toThrow();
      expect(() => MiscUtils.valueToDateTime(null)).toThrow();
      expect(() => MiscUtils.valueToDateTime(undefined)).toThrow();
    });

    it('error message mentions MiscUtils', () => {
      try {
        MiscUtils.valueToDateTime({});
        fail('Expected to throw');
      } catch (e) {
        expect(e.message).toContain('MiscUtils::valueToDateTime');
      }
    });
  });
});
