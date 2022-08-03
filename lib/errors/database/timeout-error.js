'use strict';

const MythixORMDatabaseBaseError = require('../database-base-error');

class MythixORMTimeoutError extends MythixORMDatabaseBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMTimeoutError';
  }
}

module.exports = MythixORMTimeoutError;
