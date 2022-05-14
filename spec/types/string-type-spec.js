/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../src');

describe('StringType', () => {
  it('can construct from class', () => {
    let type = new Types.StringType(123);
    expect(type.toString()).toEqual('VARCHAR(123)');
    expect(type.length).toBe(123);
  });

  it('can construct from type helper', () => {
    let type = Types.STRING(123);
    expect(type.toString()).toEqual('VARCHAR(123)');
    expect(type.length).toBe(123);
  });
});
