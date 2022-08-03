'use strict';

const MythixORMAccessDeniedError              = require('./access-denied-error');
const MythixORMConnectionAcquireTimeoutError  = require('./connection-acquire-timeout-error');
const MythixORMConnectionRefusedError         = require('./connection-refused-error');
const MythixORMConnectionTimedOutError        = require('./connection-timed-out-error');
const MythixORMHostNotFoundError              = require('./host-not-found-error');
const MythixORMHostNotReachableError          = require('./host-not-reachable-error');
const MythixORMInvalidConnectionError         = require('./invalid-connection-error');

module.exports = {
  MythixORMAccessDeniedError,
  MythixORMConnectionAcquireTimeoutError,
  MythixORMConnectionRefusedError,
  MythixORMConnectionTimedOutError,
  MythixORMHostNotFoundError,
  MythixORMHostNotReachableError,
  MythixORMInvalidConnectionError,
};
