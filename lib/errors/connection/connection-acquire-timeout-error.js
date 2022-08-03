'use strict';

const MythixORMConnectionBaseError = require('../connection-base-error');

class MythixORMConnectionAcquireTimeoutError extends MythixORMConnectionBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMConnectionAcquireTimeoutError';
  }
}

module.exports = MythixORMConnectionAcquireTimeoutError;
