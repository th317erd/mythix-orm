'use strict';

const LiteralFieldBase = require('./literal-field-base');

class AverageLiteral extends LiteralFieldBase {
  toString(connection, options) {
    if (!connection)
      return `${this.constructor.name} {}`;

    return connection.literalToString(this, options);
  }
}

module.exports = AverageLiteral;
