'use strict';

const MythixORMConnectionBaseError = require('../connection-base-error');

class MythixORMHostNotReachableError extends MythixORMConnectionBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMHostNotReachableError';
  }
}

module.exports = MythixORMHostNotReachableError;
