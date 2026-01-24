/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Errors } = require('../../lib');
const {
  MythixORMBaseError,
  MythixORMDatabaseBaseError,
  MythixORMExclusionConstraintError,
  MythixORMForeignKeyConstraintError,
  MythixORMTimeoutError,
  MythixORMUnknownConstraintError,
} = Errors;

describe('Database Errors', () => {
  describe('MythixORMDatabaseBaseError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMDatabaseBaseError('Database error');
      expect(error.message).toEqual('Database error');
      expect(error.name).toEqual('MythixORMDatabaseBaseError');
      expect(error.sql).toBeUndefined();
    });

    it('can be constructed with an Error object', () => {
      const originalError = new Error('Original database error');
      const error = new MythixORMDatabaseBaseError(originalError);
      expect(error.message).toEqual('Original database error');
      expect(error.original).toBe(originalError);
    });

    it('extracts SQL from original error when present', () => {
      const originalError = new Error('Query failed');
      originalError.sql = 'SELECT * FROM users WHERE id = 1';
      const error = new MythixORMDatabaseBaseError(originalError);
      expect(error.sql).toEqual('SELECT * FROM users WHERE id = 1');
    });

    it('is an instance of Error and MythixORMBaseError', () => {
      const error = new MythixORMDatabaseBaseError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMDatabaseBaseError).toBe(true);
    });
  });

  describe('MythixORMExclusionConstraintError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMExclusionConstraintError('Exclusion constraint violated');
      expect(error.message).toEqual('Exclusion constraint violated');
      expect(error.name).toEqual('MythixORMExclusionConstraintError');
    });

    it('can be constructed with an Error object and extracts SQL', () => {
      const originalError = new Error('Constraint violation');
      originalError.sql = 'INSERT INTO users (email) VALUES ($1)';
      const error = new MythixORMExclusionConstraintError(originalError);
      expect(error.message).toEqual('Constraint violation');
      expect(error.original).toBe(originalError);
      expect(error.sql).toEqual('INSERT INTO users (email) VALUES ($1)');
    });

    it('has correct instanceof chain', () => {
      const error = new MythixORMExclusionConstraintError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMDatabaseBaseError).toBe(true);
      expect(error instanceof MythixORMExclusionConstraintError).toBe(true);
    });
  });

  describe('MythixORMForeignKeyConstraintError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMForeignKeyConstraintError('Foreign key constraint violated');
      expect(error.message).toEqual('Foreign key constraint violated');
      expect(error.name).toEqual('MythixORMForeignKeyConstraintError');
    });

    it('can be constructed with an Error object and extracts SQL', () => {
      const originalError = new Error('FK violation');
      originalError.sql = 'DELETE FROM users WHERE id = 1';
      const error = new MythixORMForeignKeyConstraintError(originalError);
      expect(error.message).toEqual('FK violation');
      expect(error.original).toBe(originalError);
      expect(error.sql).toEqual('DELETE FROM users WHERE id = 1');
    });

    it('has correct instanceof chain', () => {
      const error = new MythixORMForeignKeyConstraintError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMDatabaseBaseError).toBe(true);
      expect(error instanceof MythixORMForeignKeyConstraintError).toBe(true);
    });
  });

  describe('MythixORMTimeoutError', () => {
    it('can be constructed with a string message', () => {
      const error = new MythixORMTimeoutError('Query timeout');
      expect(error.message).toEqual('Query timeout');
      expect(error.name).toEqual('MythixORMTimeoutError');
    });

    it('can be constructed with an Error object and extracts SQL', () => {
      const originalError = new Error('Query timed out');
      originalError.sql = 'SELECT * FROM large_table';
      const error = new MythixORMTimeoutError(originalError);
      expect(error.message).toEqual('Query timed out');
      expect(error.original).toBe(originalError);
      expect(error.sql).toEqual('SELECT * FROM large_table');
    });

    it('has correct instanceof chain', () => {
      const error = new MythixORMTimeoutError('Test');
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMDatabaseBaseError).toBe(true);
      expect(error instanceof MythixORMTimeoutError).toBe(true);
    });
  });

  describe('MythixORMUnknownConstraintError', () => {
    it('can be constructed with options containing parent error', () => {
      const parentError = new Error('Unknown constraint');
      parentError.sql = 'INSERT INTO table (col) VALUES ($1)';
      const error = new MythixORMUnknownConstraintError({ parent: parentError });
      expect(error.message).toEqual('Unknown constraint');
      expect(error.name).toEqual('MythixORMUnknownConstraintError');
      expect(error.sql).toEqual('INSERT INTO table (col) VALUES ($1)');
    });

    it('has correct instanceof chain', () => {
      const error = new MythixORMUnknownConstraintError({ parent: new Error('Test') });
      expect(error instanceof Error).toBe(true);
      expect(error instanceof MythixORMBaseError).toBe(true);
      expect(error instanceof MythixORMDatabaseBaseError).toBe(true);
      expect(error instanceof MythixORMUnknownConstraintError).toBe(true);
    });
  });
});
