/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Errors } = require('../../lib');
const { MythixORMBaseError } = Errors;

describe('MythixORMBaseError', () => {
  describe('construction', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMBaseError('Test error message');
      expect(error.message).toEqual('Test error message');
      expect(error.name).toEqual('MythixORMBaseError');
      expect(error.original).toBeUndefined();
    });

    it('can be constructed with an Error object', () => {
      const originalError = new Error('Original error');
      const error = new MythixORMBaseError(originalError);
      expect(error.message).toEqual('Original error');
      expect(error.name).toEqual('MythixORMBaseError');
      expect(error.original).toBe(originalError);
    });

    it('preserves the original error when constructed with Error', () => {
      const originalError = new TypeError('Type mismatch');
      originalError.code = 'TYPE_MISMATCH';
      const error = new MythixORMBaseError(originalError);
      expect(error.original).toBe(originalError);
      expect(error.original.code).toEqual('TYPE_MISMATCH');
    });
  });

  describe('instanceof chain', () => {
    it('is an instance of Error', () => {
      const error = new MythixORMBaseError('Test');
      expect(error instanceof Error).toBe(true);
    });

    it('is an instance of MythixORMBaseError', () => {
      const error = new MythixORMBaseError('Test');
      expect(error instanceof MythixORMBaseError).toBe(true);
    });
  });

  describe('name property', () => {
    it('has the correct name property', () => {
      const error = new MythixORMBaseError('Test');
      expect(error.name).toEqual('MythixORMBaseError');
    });
  });

  describe('stack trace', () => {
    it('has a stack trace', () => {
      const error = new MythixORMBaseError('Test');
      expect(error.stack).toBeDefined();
      expect(typeof error.stack).toEqual('string');
    });
  });
});
