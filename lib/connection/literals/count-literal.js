'use strict';

const LiteralFieldBase = require('./literal-field-base');

class CountLiteral extends LiteralFieldBase {
  static isFieldRequired() {
    return false;
  }

  toString(connection) {
    if (!connection)
      return `${this.constructor.name} {}`;

    return connection.literalToString(this);
  }
}

module.exports = CountLiteral;
