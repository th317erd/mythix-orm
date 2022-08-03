'use strict';

const MythixORMDatabaseBaseError = require('../database-base-error');

class MythixORMForeignKeyConstraintError extends MythixORMDatabaseBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMForeignKeyConstraintError';
  }
}

module.exports = MythixORMForeignKeyConstraintError;
