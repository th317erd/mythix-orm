/* eslint-disable new-cap */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const {
  FLAG_ON_INITIALIZE,
  FLAG_ON_CREATE,
  FLAG_ON_UPDATE,
  FLAG_ON_STORE,
  FLAG_LITERAL,
  defaultValueFlags,
  AUTO_INCREMENT,
  DATETIME_NOW,
  DATE_NOW,
} = require('../../src/helpers/default-helpers');

describe('DefaultHelpers', () => {
  it('can set initialize flag on a default method', () => {
    let func = defaultValueFlags(function() {});
    expect(func.mythixFlags).toEqual(FLAG_ON_INITIALIZE);
  });

  it('can set create flag on a default method', () => {
    let func = defaultValueFlags(function() {}, { onCreate: true });
    expect(func.mythixFlags).toEqual(FLAG_ON_CREATE);
  });

  it('can set update flag on a default method', () => {
    let func = defaultValueFlags(function() {}, { onUpdate: true });
    expect(func.mythixFlags).toEqual(FLAG_ON_UPDATE);
  });

  it('can set store flag on a default method', () => {
    let func = defaultValueFlags(function() {}, { onStore: true });
    expect(func.mythixFlags).toEqual(FLAG_ON_STORE);
  });

  it('can set remote flag on a default method', () => {
    let func = defaultValueFlags(function() {}, { literal: true });
    expect(func.mythixFlags).toEqual(FLAG_LITERAL);
  });

  it('can remove initialize flag on a default method', () => {
    let func = defaultValueFlags(function() {}, { onInitialize: false });
    expect(func.mythixFlags).toEqual(0);
  });

  describe('AUTO_INCREMENT', () => {
    it('should have the correct flags set', () => {
      expect(AUTO_INCREMENT.mythixFlags).toEqual(FLAG_LITERAL);
    });

    it('should call connection to get value', () => {
      let context = {
        connection: {
          getDefaultFieldValue: (type) => {
            return type;
          },
        },
      };

      let connectionDefaultType = AUTO_INCREMENT(context);
      expect(connectionDefaultType).toEqual('AUTO_INCREMENT');
    });
  });

  describe('DATETIME_NOW', () => {
    it('should have the correct flags set', () => {
      expect(DATETIME_NOW.mythixFlags).toEqual(FLAG_LITERAL);
    });

    it('should call connection to get value', () => {
      let context = {
        connection: {
          getDefaultFieldValue: (type) => {
            return type;
          },
        },
      };

      let connectionDefaultType = DATETIME_NOW(context);
      expect(connectionDefaultType).toEqual('DATETIME_NOW');
    });
  });

  describe('DATE_NOW', () => {
    it('should have the correct flags set', () => {
      expect(DATE_NOW.mythixFlags).toEqual(FLAG_LITERAL);
    });

    it('should call connection to get value', () => {
      let context = {
        connection: {
          getDefaultFieldValue: (type) => {
            return type;
          },
        },
      };

      let connectionDefaultType = DATE_NOW(context);
      expect(connectionDefaultType).toEqual('DATE_NOW');
    });
  });
});
