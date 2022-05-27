/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../src');

describe('ModelType', () => {
  it('can construct from class', () => {
    let type = new Types.ModelType('Role:userID');
    expect(type.toString()).toEqual('');
  });

  it('can construct from type helper', () => {
    let type = Types.MODEL('Role:userID');
    expect(type.toString()).toEqual('');
  });

  it('will throw error on attempt to cast without a type instance', () => {
    expect(() => Types.MODEL.castToType({})).toThrow(new TypeError('ModelType::castToType: Type instance is required to cast.'));
  });
});
