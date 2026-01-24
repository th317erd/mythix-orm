/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Errors } = require('../../lib');
const {
  MythixORMBaseError,
  MythixORMConnectionBaseError,
  MythixORMAccessDeniedError,
  MythixORMConnectionAcquireTimeoutError,
  MythixORMConnectionRefusedError,
  MythixORMConnectionTimedOutError,
  MythixORMHostNotFoundError,
  MythixORMHostNotReachableError,
  MythixORMInvalidConnectionError,
} = Errors;

describe('Connection Errors', () => {
  describe('MythixORMConnectionBaseError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMConnectionBaseError('Connection error');
      expect(error.message).toEqual('Connection error');
      expect(error.name).toEqual('MythixORMConnectionBaseError');
    });

    it('can be constructed with an Error object', () => {
      const originalError = new Error('Original connection error');
      const error = new MythixORMConnectionBaseError(originalError);
      expect(error.message).toEqual('Original connection error');
      expect(error.original).toBe(originalError);
    });

    it('is an instance of Error and MythixORMBaseError', () => {
      const error = new MythixORMConnectionBaseError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMConnectionBaseError).toBe(true);
    });
  });

  describe('MythixORMAccessDeniedError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMAccessDeniedError('Access denied');
      expect(error.message).toEqual('Access denied');
      expect(error.name).toEqual('MythixORMAccessDeniedError');
    });

    it('can be constructed with an Error object', () => {
      const originalError = new Error('Invalid credentials');
      const error = new MythixORMAccessDeniedError(originalError);
      expect(error.message).toEqual('Invalid credentials');
      expect(error.original).toBe(originalError);
    });

    it('has correct instanceof chain', () => {
      const error = new MythixORMAccessDeniedError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMConnectionBaseError).toBe(true);
      expect(error instanceof MythixORMAccessDeniedError).toBe(true);
    });
  });

  describe('MythixORMConnectionAcquireTimeoutError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMConnectionAcquireTimeoutError('Timeout acquiring connection');
      expect(error.message).toEqual('Timeout acquiring connection');
      expect(error.name).toEqual('MythixORMConnectionAcquireTimeoutError');
    });

    it('has correct instanceof chain', () => {
      const error = new MythixORMConnectionAcquireTimeoutError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMConnectionBaseError).toBe(true);
      expect(error instanceof MythixORMConnectionAcquireTimeoutError).toBe(true);
    });
  });

  describe('MythixORMConnectionRefusedError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMConnectionRefusedError('Connection refused');
      expect(error.message).toEqual('Connection refused');
      expect(error.name).toEqual('MythixORMConnectionRefusedError');
    });

    it('has correct instanceof chain', () => {
      const error = new MythixORMConnectionRefusedError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMConnectionBaseError).toBe(true);
      expect(error instanceof MythixORMConnectionRefusedError).toBe(true);
    });
  });

  describe('MythixORMConnectionTimedOutError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMConnectionTimedOutError('Connection timed out');
      expect(error.message).toEqual('Connection timed out');
      expect(error.name).toEqual('MythixORMConnectionTimedOutError');
    });

    it('has correct instanceof chain', () => {
      const error = new MythixORMConnectionTimedOutError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMConnectionBaseError).toBe(true);
      expect(error instanceof MythixORMConnectionTimedOutError).toBe(true);
    });
  });

  describe('MythixORMHostNotFoundError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMHostNotFoundError('Host not found');
      expect(error.message).toEqual('Host not found');
      expect(error.name).toEqual('MythixORMHostNotFoundError');
    });

    it('has correct instanceof chain', () => {
      const error = new MythixORMHostNotFoundError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMConnectionBaseError).toBe(true);
      expect(error instanceof MythixORMHostNotFoundError).toBe(true);
    });
  });

  describe('MythixORMHostNotReachableError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMHostNotReachableError('Host not reachable');
      expect(error.message).toEqual('Host not reachable');
      expect(error.name).toEqual('MythixORMHostNotReachableError');
    });

    it('has correct instanceof chain', () => {
      const error = new MythixORMHostNotReachableError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMConnectionBaseError).toBe(true);
      expect(error instanceof MythixORMHostNotReachableError).toBe(true);
    });
  });

  describe('MythixORMInvalidConnectionError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMInvalidConnectionError('Invalid connection');
      expect(error.message).toEqual('Invalid connection');
      expect(error.name).toEqual('MythixORMInvalidConnectionError');
    });

    it('has correct instanceof chain', () => {
      const error = new MythixORMInvalidConnectionError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMConnectionBaseError).toBe(true);
      expect(error instanceof MythixORMInvalidConnectionError).toBe(true);
    });
  });
});
