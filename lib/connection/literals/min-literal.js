'use strict';

const LiteralFieldBase = require('./literal-field-base');

class MinLiteral extends LiteralFieldBase {
  static isAggregate() {
    return true;
  }

  toString(connection, options) {
    if (!connection)
      return `${this.constructor.name} {}`;

    return connection.literalToString(this, options);
  }
}

module.exports = MinLiteral;
