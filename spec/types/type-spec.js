/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Types } = require('../../src');

class CustomType extends Types.Type {
  constructor(...args) {
    super(...args);
  }
}

describe('Type', () => {
  it('will store arguments', () => {
    let type = new CustomType('test', 123, true);
    expect(type._args).toEqual([ 'test', 123, true ]);
  });
});
