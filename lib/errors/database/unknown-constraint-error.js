'use strict';

const MythixORMDatabaseBaseError = require('../database-base-error');

class MythixORMUnknownConstraintError extends MythixORMDatabaseBaseError {
  constructor(options) {
    super(options.parent);

    this.name = 'MythixORMUnknownConstraintError';
  }
}

module.exports = MythixORMUnknownConstraintError;
