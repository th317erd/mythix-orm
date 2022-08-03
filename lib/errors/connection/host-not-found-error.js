'use strict';

const MythixORMConnectionBaseError = require('../connection-base-error');

class MythixORMHostNotFoundError extends MythixORMConnectionBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMHostNotFoundError';
  }
}

module.exports = MythixORMHostNotFoundError;
