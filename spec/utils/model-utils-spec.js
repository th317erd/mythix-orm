/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const UUID            = require('uuid');
const { Utils }       = require('../../lib');
const { UUID_REGEXP } = require('../support/test-helpers');

describe('Utils::model', () => {
  describe('isUUID', () => {
    it('should be able to generate and verify UUID V1', () => {
      let value = UUID.v1();
      expect(value).toMatch(UUID_REGEXP);
      expect(Utils.isUUID(value)).toEqual(true);
    });

    it('should be able to generate and verify UUID V4', () => {
      let value = UUID.v4();
      expect(value).toMatch(UUID_REGEXP);
      expect(Utils.isUUID(value)).toEqual(true);
    });
  });

  describe('sanitizeFieldString', () => {
    it('should strip all but certain characters', () => {
      expect(Utils.sanitizeFieldString('"User"::  derp-y ... _field !$%%^#@$^ 2')).toEqual('User:derpy._field2');
    });
  });

  describe('parseQualifiedName', () => {
    it('should be able to parse model name and fieldNames', () => {
      expect(Utils.parseQualifiedName('"User"::  derp-y ... _field !$%%^#@$^ 2')).toEqual({
        modelName:  'User',
        fieldNames: [ 'derpy', '_field2' ],
      });

      expect(Utils.parseQualifiedName('  ::  derp-y ... _field !$%%^#@$^ 2')).toEqual({
        modelName:  undefined,
        fieldNames: [ 'derpy', '_field2' ],
      });

      expect(Utils.parseQualifiedName('field')).toEqual({
        modelName:  undefined,
        fieldNames: [ 'field' ],
      });

      expect(Utils.parseQualifiedName('field.id')).toEqual({
        modelName:  undefined,
        fieldNames: [ 'field', 'id' ],
      });
    });
  });
});
