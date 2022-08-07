/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Types, ConnectionBase } = require('../../../lib');
const { UUID_REGEXP }           = require('../../support/test-helpers');

describe('UUIDV4Type', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.UUIDV4Type();
      expect(type.toConnectionType(connection)).toEqual('VARCHAR(36)');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.UUIDV4Type();
      expect(type.toConnectionType()).toEqual('VARCHAR(36)');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.UUIDV4Type();
      expect(type.toString(connection)).toEqual('VARCHAR(36)');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.UUIDV4Type();
      expect(type.toString()).toEqual('VARCHAR(36)');
    });
  });

  it('can construct from class', () => {
    let type = new Types.UUIDV4Type();
    expect(type.toString()).toEqual('VARCHAR(36)');
  });

  it('can construct from type helper', () => {
    let type = Types.UUIDV4();
    expect(type.toString()).toEqual('VARCHAR(36)');
  });

  it('can cast to type', () => {
    let type = Types.UUIDV4();
    expect(() => type.castToType({ value: '1234' })).toThrow(new TypeError('UUIDV4Type::castToType: Provided value "1234" is not a valid UUID.'));

    let uuid = Types.UUIDV4.Default.UUIDV4()();
    expect(uuid).toMatch(UUID_REGEXP);
    expect(type.castToType({ value: uuid })).toEqual(uuid);

    expect(type.castToType({ value: undefined })).toBe(undefined);
    expect(type.castToType({ value: null })).toBe(null);
  });
});
