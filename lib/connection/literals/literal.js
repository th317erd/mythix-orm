'use strict';

const LiteralBase  = require('./literal-base');

class Literal extends LiteralBase {
  constructor(literal, options) {
    super(literal, options);
  }
}

module.exports = Literal;
