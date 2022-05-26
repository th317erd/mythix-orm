/* eslint-disable new-cap */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const {
  FLAG_ON_INITIALIZE,
  FLAG_ON_CREATE,
  FLAG_ON_UPDATE,
  FLAG_ON_STORE,
  FLAG_REMOTE,
  defaultValueFlags,
  AUTO_INCREMENT,
  NOW,
} = require('../../src/helpers/default-helpers');

describe('DefaultHelpers', () => {
  it('can set initialize flag on a default method', () => {
    let func = defaultValueFlags(function() {});
    expect(func.mythixFlags).toEqual(FLAG_ON_INITIALIZE);
  });

  it('can set create flag on a default method', () => {
    let func = defaultValueFlags(function() {}, { onCreate: true });
    expect(func.mythixFlags).toEqual(FLAG_ON_INITIALIZE | FLAG_ON_CREATE);
  });

  it('can set update flag on a default method', () => {
    let func = defaultValueFlags(function() {}, { onUpdate: true });
    expect(func.mythixFlags).toEqual(FLAG_ON_INITIALIZE | FLAG_ON_UPDATE);
  });

  it('can set store flag on a default method', () => {
    let func = defaultValueFlags(function() {}, { onStore: true });
    expect(func.mythixFlags).toEqual(FLAG_ON_INITIALIZE | FLAG_ON_STORE);
  });

  it('can set remote flag on a default method', () => {
    let func = defaultValueFlags(function() {}, { remote: true });
    expect(func.mythixFlags).toEqual(FLAG_ON_INITIALIZE | FLAG_REMOTE);
  });

  it('can remove initialize flag on a default method', () => {
    let func = defaultValueFlags(function() {}, { onCreate: true, onInitialize: false });
    expect(func.mythixFlags).toEqual(FLAG_ON_CREATE);
  });

  describe('AUTO_INCREMENT', () => {
    it('should have the correct flags set', () => {
      expect(AUTO_INCREMENT.mythixFlags).toEqual(FLAG_REMOTE | FLAG_ON_INITIALIZE);
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

  describe('NOW', () => {
    it('should have the correct flags set', () => {
      expect(NOW.mythixFlags).toEqual(FLAG_REMOTE | FLAG_ON_INITIALIZE);
    });

    it('should call connection to get value', () => {
      let context = {
        connection: {
          getDefaultFieldValue: (type) => {
            return type;
          },
        },
      };

      let connectionDefaultType = NOW(context);
      expect(connectionDefaultType).toEqual('NOW');
    });
  });
});
