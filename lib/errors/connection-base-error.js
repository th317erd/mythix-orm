'use strict';

const MythixORMBaseError = require('./base-error');

class MythixORMConnectionBaseError extends MythixORMBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMConnectionBaseError';
  }
}

module.exports = MythixORMConnectionBaseError;
