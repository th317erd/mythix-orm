/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types }       = require('../../../src');
const { UUID_REGEXP } = require('../../support/test-helpers');

describe('UUIDV4Type', () => {
  it('can construct from class', () => {
    let type = new Types.UUIDV4Type();
    expect(type.toString()).toEqual('VARCHAR(36)');
  });

  it('can construct from type helper', () => {
    let type = Types.UUIDV4();
    expect(type.toString()).toEqual('VARCHAR(36)');
  });

  it('can cast to type', () => {
    expect(() => Types.UUIDV4.castToType({ value: '1234' })).toThrow(new TypeError('UUIDV4Type::castToType: Provided value "1234" is not a valid UUID.'));

    let uuid = Types.UUIDV4.Default.UUIDV4();
    expect(uuid).toMatch(UUID_REGEXP);
    expect(Types.UUIDV4.castToType({ value: uuid })).toEqual(uuid);

    expect(Types.UUIDV4.castToType({ value: undefined })).toBe(undefined);
    expect(Types.UUIDV4.castToType({ value: null })).toBe(null);
  });
});
