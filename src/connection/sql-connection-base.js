'use strict';

const ConnectionBase  = require('./connection-base');

class SQLConnectionBase extends ConnectionBase {
  constructor(_options) {
    super(_options);

    Object.defineProperties(this, {
      'queryGenerator': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
    });
  }

  getQueryGenerator() {
    return this.queryGenerator;
  }

  setQueryGenerator(queryGenerator) {
    this.queryGenerator = queryGenerator;
  }
}

module.exports = SQLConnectionBase;
