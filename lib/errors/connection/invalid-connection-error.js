'use strict';

const MythixORMConnectionBaseError = require('../connection-base-error');

class MythixORMInvalidConnectionError extends MythixORMConnectionBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMInvalidConnectionError';
  }
}

module.exports = MythixORMInvalidConnectionError;
