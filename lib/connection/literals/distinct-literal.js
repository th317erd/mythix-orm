'use strict';

const LiteralFieldBase = require('./literal-field-base');

class DistinctLiteral extends LiteralFieldBase {
  toString(connection) {
    if (!connection)
      return `${this.constructor.name} {}`;

    return connection.literalToString(this);
  }
}

module.exports = DistinctLiteral;
