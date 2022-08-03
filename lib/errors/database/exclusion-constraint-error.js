'use strict';

const MythixORMDatabaseBaseError = require('../database-base-error');

class MythixORMExclusionConstraintError extends MythixORMDatabaseBaseError {
  constructor(...args) {
    super(...args);

    this.name = 'MythixORMExclusionConstraintError';
  }
}

module.exports = MythixORMExclusionConstraintError;
