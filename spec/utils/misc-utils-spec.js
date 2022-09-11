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

  describe('objectAssignSpecial', () => {
    it('can copy object properties', () => {
      let source = { test: true, hello: 'world' };
      let copy = Utils.objectAssignSpecial(source);

      expect(copy).not.toBe(source);
      expect(copy).toEqual(source);
    });

    it('can specify a prototype', () => {
      let proto = { derp: 'wow', cool: 'beans' };
      let source = { test: true, hello: 'world' };
      let copy = Utils.objectAssignSpecial(source, proto);

      expect(copy).not.toBe(source);
      expect(copy).toEqual(source);
      expect(copy.derp).toEqual('wow');
      expect(copy.cool).toEqual('beans');
    });

    it('can skip specified keys', () => {
      let proto = { derp: 'wow', cool: 'beans' };
      let source = { test: true, hello: 'world', one: 1, two: 2, three: 3 };
      let copy = Utils.objectAssignSpecial(source, proto, [ 'one', 'three' ]);

      expect(copy).not.toBe(source);
      expect(copy).toEqual({ test: true, hello: 'world', two: 2 });
      expect(copy.derp).toEqual('wow');
      expect(copy.cool).toEqual('beans');
    });
  });
});
