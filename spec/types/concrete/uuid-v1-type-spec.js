/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Types, ConnectionBase } = require('../../../lib');
const { UUID_REGEXP }           = require('../../support/test-helpers');

describe('UUIDV1Type', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.UUIDV1Type();
      expect(type.toConnectionType(connection)).toEqual('VARCHAR(36)');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.UUIDV1Type();
      expect(type.toConnectionType()).toEqual('VARCHAR(36)');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.UUIDV1Type();
      expect(type.toString(connection)).toEqual('VARCHAR(36)');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.UUIDV1Type();
      expect(type.toString()).toEqual('VARCHAR(36)');
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
    let type = Types.UUIDV1({
      node:     [ 0x01, 0x23, 0x45, 0x67, 0x89, 0xab ],
      clockseq: 0x1234,
      msecs:    new Date('2011-11-01').getTime(),
      nsecs:    5678,
      random:   new Array(16),
    });

    expect(() => type.castToType({ value: '1234' })).toThrow(new TypeError('UUIDV1Type::castToType: Provided value "1234" is not a valid UUID.'));

    let uuid = Types.UUIDV1.Default.UUIDV1({ field: { type } });

    expect(uuid).toMatch(UUID_REGEXP);
    expect(type.castToType({ value: uuid })).toEqual(uuid);

    expect(type.castToType({ value: undefined })).toBe(undefined);
    expect(type.castToType({ value: null })).toBe(null);
  });
});
