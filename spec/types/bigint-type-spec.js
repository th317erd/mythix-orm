/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../src');

describe('BigIntType', () => {
  it('can construct from class', () => {
    let type = new Types.BigIntType();
    expect(type.toString()).toEqual('BIGINT');
    expect(type.length).toBe(null);
  });

  it('can construct from class with length', () => {
    let type = new Types.BigIntType(11);
    expect(type.toString()).toEqual('BIGINT(11)');
    expect(type.length).toEqual(11);
  });

  it('can construct from type helper', () => {
    let type = Types.BIGINT();
    expect(type.toString()).toEqual('BIGINT');
    expect(type.length).toBe(null);
  });

  it('can construct from type helper with length', () => {
    let type = Types.BIGINT(11);
    expect(type.toString()).toEqual('BIGINT(11)');
    expect(type.length).toEqual(11);
  });
});
