/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Utils } = require('../../lib');

describe('Utils::misc', () => {
  describe('collect', () => {
    async function *test() {
      for (let i = 0; i < 5; i++)
        yield i;
    }

    it('can collect from an async generator', async () => {
      expect(await Utils.collect(test())).toEqual([ 0, 1, 2, 3, 4 ]);
    });
  });
});
