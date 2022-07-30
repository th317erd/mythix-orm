'use strict';

const SQLLiteralBase  = require('./sql-literal-base');

class SQLLiteral extends SQLLiteralBase {
  constructor(literal) {
    super(literal);
  }
}

module.exports = SQLLiteral;
