'use strict';

const MythixORMBaseError = require('./base-error');

class MythixORMDatabaseBaseError extends MythixORMBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMDatabaseBaseError';

    let originalError = args[0];
    if (originalError instanceof Error)
      this.sql = originalError.sql;
  }
}

module.exports = MythixORMDatabaseBaseError;
