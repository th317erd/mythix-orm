/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Utils } = require('../../src');

describe('Utils::misc', () => {
  describe('copyClassStaticProps', () => {
    it('can copy static props from a class', () => {
      class Test {
        static testFunc() {

        }

        static fields = [];
      }

      function test(arg1, arg2, arg3) {}

      Utils.copyClassStaticProps(Test, test);

      expect(typeof test.testFunc).toEqual('function');
      expect(typeof test.fields).toEqual('object');
      expect(test.fields).toBeInstanceOf(Array);
      expect(test.length).toEqual(3);
    });
  });
});
