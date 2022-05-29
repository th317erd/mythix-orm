/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types }       = require('../../../src');
const { UUID_REGEXP } = require('../../support/test-helpers');

describe('UUIDV1Type', () => {
  describe('toConnectionType', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.UUIDV1Type();
      expect(type.toConnectionType({ dialect: 'sqlite' })).toEqual('VARCHAR(36)');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.UUIDV1Type();
      expect(type.toConnectionType({ dialect: undefined })).toEqual('VARCHAR(36)');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when dialect is "sqlite"', () => {
      let type = new Types.UUIDV1Type();
      expect(type.toString({ dialect: 'sqlite' })).toEqual('VARCHAR(36)');
    });

    it('can convert to connection type when dialect is undefined', () => {
      let type = new Types.UUIDV1Type();
      expect(type.toString({ dialect: undefined })).toEqual('VARCHAR(36)');
    });
  });

  it('can construct from class', () => {
    let type = new Types.UUIDV1Type();
    expect(type.toString()).toEqual('VARCHAR(36)');
  });

  it('can construct from type helper', () => {
    let type = Types.UUIDV1();
    expect(type.toString()).toEqual('VARCHAR(36)');
  });

  it('can cast to type', () => {
    expect(() => Types.UUIDV1.castToType({ value: '1234' })).toThrow(new TypeError('UUIDV1Type::castToType: Provided value "1234" is not a valid UUID.'));

    let uuid = Types.UUIDV1.Default.UUIDV1();
    expect(uuid).toMatch(UUID_REGEXP);
    expect(Types.UUIDV1.castToType({ value: uuid })).toEqual(uuid);

    expect(Types.UUIDV1.castToType({ value: undefined })).toBe(undefined);
    expect(Types.UUIDV1.castToType({ value: null })).toBe(null);
  });
});
