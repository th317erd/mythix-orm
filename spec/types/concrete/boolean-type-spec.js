/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { Types, ConnectionBase } = require('../../../lib');

describe('BooleanType', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('toConnectionType', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.BooleanType();
      expect(type.toConnectionType(connection)).toEqual('BOOLEAN');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.BooleanType();
      expect(type.toConnectionType()).toEqual('BOOLEAN');
    });
  });

  describe('toString', () => {
    it('can convert to connection type when connection is defined', () => {
      let type = new Types.BooleanType();
      expect(type.toString(connection)).toEqual('BOOLEAN');
    });

    it('can convert to connection type when connection is undefined', () => {
      let type = new Types.BooleanType();
      expect(type.toString()).toEqual('BOOLEAN');
    });
  });

  it('can construct from class', () => {
    let type = new Types.BooleanType();
    expect(type.toString()).toEqual('BOOLEAN');
  });

  it('can construct from type helper', () => {
    let type = Types.BOOLEAN();
    expect(type.toString()).toEqual('BOOLEAN');
  });

  it('can cast to type', () => {
    let type = Types.BOOLEAN();
    let value = type.castToType({ value: 1 });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(true);

    value = type.castToType({ value: 0 });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(false);

    value = type.castToType({ value: -1 });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(true);

    value = type.castToType({ value: false });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(false);

    value = type.castToType({ value: true });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(true);

    value = type.castToType({ value: 'false' });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(false);

    value = type.castToType({ value: 'true' });
    expect(typeof value).toEqual('boolean');
    expect(value).toEqual(true);
  });
});
