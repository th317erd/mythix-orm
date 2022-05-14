/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../src');

describe('IntegerType', () => {
  it('can construct from class', () => {
    let type = new Types.IntegerType();
    expect(type.toString()).toEqual('INTEGER');
  });

  it('can construct from type helper', () => {
    let type = Types.INTEGER();
    expect(type.toString()).toEqual('INTEGER');
  });
});
