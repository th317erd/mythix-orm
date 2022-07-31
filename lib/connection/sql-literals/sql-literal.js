'use strict';

const SQLLiteralBase  = require('./sql-literal-base');

class SQLLiteral extends SQLLiteralBase {
  constructor(literal, options) {
    super(literal, options);
  }
}

module.exports = SQLLiteral;
