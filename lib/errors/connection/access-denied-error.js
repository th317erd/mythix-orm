'use strict';

const MythixORMConnectionBaseError = require('../connection-base-error');

class MythixORMAccessDeniedError extends MythixORMConnectionBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMAccessDeniedError';
  }
}

module.exports = MythixORMAccessDeniedError;
