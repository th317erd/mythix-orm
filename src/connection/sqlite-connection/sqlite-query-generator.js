'use strict';

const QueryGeneratorBase = require('../query-generator-base');

class SQLiteQueryGenerator extends QueryGeneratorBase {
  // eslint-disable-next-line no-unused-vars
  generateSQLJoinTypeFromQueryEngineJoinType(joinType, options) {
    if (joinType === 'LEFT INNER JOIN')
      return 'INNER JOIN';

    return joinType;
  }
}

module.exports = SQLiteQueryGenerator;
