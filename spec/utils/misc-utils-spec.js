/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Utils } = require('../../src');

describe('Utils::misc', () => {
  describe('iterateStaticProps', () => {
    it('should throw an error if a function is not provided', () => {
      expect(() => Utils.iterateStaticProps({}, () => {})).toThrow(new TypeError('Utils::iterateStaticProps: "Klass" argument must be a function.'));
    });

    it('can copy static props from a class', () => {
      class Test {
        static testFunc() {

        }

        static fields = [];
      }

      let keys = [];
      Utils.iterateStaticProps(Test, ({ key }) => {
        keys.push(key);
      });

      expect(keys.sort()).toEqual([
        'fields',
        'testFunc',
      ]);
    });
  });

  describe('copyStaticProps', () => {
    it('should throw an error if a function is not provided', () => {
      expect(() => Utils.copyStaticProps({}, () => {})).toThrow(new TypeError('Utils::copyStaticProps: "Klass" argument must be a function.'));
    });

    it('can copy static props from a class', () => {
      class Test {
        static testFunc() {

        }

        static fields = [];
      }

      // eslint-disable-next-line no-unused-vars
      function test(arg1, arg2, arg3) {}

      Utils.copyStaticProps(Test, test);

      expect(typeof test.testFunc).toEqual('function');
      expect(typeof test.fields).toEqual('object');
      expect(test.fields).toBeInstanceOf(Array);
      expect(test.length).toEqual(3);
    });
  });
});
