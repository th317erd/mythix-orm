'use strict';

class SQLLiteralBase {
  constructor(literal) {
    Object.defineProperties(this, {
      'literal': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        literal,
      },
    });
  }

  toString() {
    return this.literal;
  }
}

module.exports = SQLLiteralBase;
