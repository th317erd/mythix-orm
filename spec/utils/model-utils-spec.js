/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Utils } = require('../../src');

describe('Utils::model', () => {
  describe('sanitizeFieldString', () => {
    it('should strip all but certain characters', () => {
      expect(Utils.sanitizeFieldString('"User"::  derp-y ... _field !$%%^#@$^ 2')).toEqual('User:derpy._field2');
    });
  });

  describe('parseQualifiedName', () => {
    it('should be able to parse model name and fields', () => {
      expect(Utils.parseQualifiedName('"User"::  derp-y ... _field !$%%^#@$^ 2')).toEqual({
        modelName:  'User',
        fields:     [ 'derpy', '_field2' ],
      });

      expect(Utils.parseQualifiedName('  ::  derp-y ... _field !$%%^#@$^ 2')).toEqual({
        modelName:  undefined,
        fields:     [ 'derpy', '_field2' ],
      });

      expect(Utils.parseQualifiedName('field')).toEqual({
        modelName:  undefined,
        fields:     [ 'field' ],
      });

      expect(Utils.parseQualifiedName('field.id')).toEqual({
        modelName:  undefined,
        fields:     [ 'field', 'id' ],
      });
    });
  });
});
