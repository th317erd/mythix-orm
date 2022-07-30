'use strict';

const ConnectionBase  = require('./connection-base');

class MemoryConnection extends ConnectionBase {
  constructor(...args) {
    super(...args);
  }

  start() {
  }

  stop() {
  }
}

module.exports = MemoryConnection;
