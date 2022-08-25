'use strict';

const LiteralFieldBase = require('./literal-field-base');

class CountLiteral extends LiteralFieldBase {
  static isFieldRequired() {
    return false;
  }

  toString(connection, options) {
    if (!connection)
      return `${this.constructor.name} {}`;

    return connection.literalToString(this, options);
  }
}

module.exports = CountLiteral;
