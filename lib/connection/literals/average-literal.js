'use strict';

const LiteralFieldBase = require('./literal-field-base');

class AverageLiteral extends LiteralFieldBase {
  toString(connection) {
    if (!connection)
      return `${this.constructor.name} {}`;

    return connection.literalToString(this);
  }
}

module.exports = AverageLiteral;
