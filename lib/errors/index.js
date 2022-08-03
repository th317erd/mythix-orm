'use strict';

const MythixORMBaseError            = require('./base-error');
const MythixORMConnectionBaseError  = require('./connection-base-error');
const MythixORMDatabaseBaseError    = require('./database-base-error');

const {
  MythixORMAccessDeniedError,
  MythixORMConnectionAcquireTimeoutError,
  MythixORMConnectionRefusedError,
  MythixORMConnectionTimedOutError,
  MythixORMHostNotFoundError,
  MythixORMHostNotReachableError,
  MythixORMInvalidConnectionError,
} = require('./connection');

const {
  MythixORMExclusionConstraintError,
  MythixORMForeignKeyConstraintError,
  MythixORMTimeoutError,
  MythixORMUnknownConstraintError,
} = require('./database');

module.exports = {
  // Base
  MythixORMBaseError,
  MythixORMConnectionBaseError,
  MythixORMDatabaseBaseError,

  // Connection
  MythixORMAccessDeniedError,
  MythixORMConnectionAcquireTimeoutError,
  MythixORMConnectionRefusedError,
  MythixORMConnectionTimedOutError,
  MythixORMHostNotFoundError,
  MythixORMHostNotReachableError,
  MythixORMInvalidConnectionError,

  // Database
  MythixORMExclusionConstraintError,
  MythixORMForeignKeyConstraintError,
  MythixORMTimeoutError,
  MythixORMUnknownConstraintError,
};
