'use strict';

const MythixORMConnectionBaseError = require('../connection-base-error');

class MythixORMConnectionTimedOutError extends MythixORMConnectionBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMConnectionTimedOutError';
  }
}

module.exports = MythixORMConnectionTimedOutError;
