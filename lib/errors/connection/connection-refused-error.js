'use strict';

const MythixORMConnectionBaseError = require('../connection-base-error');

class MythixORMConnectionRefusedError extends MythixORMConnectionBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMConnectionRefusedError';
  }
}

module.exports = MythixORMConnectionRefusedError;
