'use strict';

const SQLLiteralBase      = require('./sql-literal-base');
const SQLLiteralFieldBase = require('./sql-literal-field-base');

class CountSQLLiteral extends SQLLiteralFieldBase {
  static isFieldRequired() {
    return false;
  }

  toString(connection) {
    if (!connection)
      return `${this.constructor.name} {}`;

    let field           = (this.definition) ? this.definitionToField(connection, this.definition) : null;
    let queryGenerator  = connection.getQueryGenerator();
    let escapedFieldName;

    if (field) {
      if (field instanceof SQLLiteralBase)
        escapedFieldName = field.toString(connection);
      else
        escapedFieldName = queryGenerator.getEscapedColumnName(field);
    } else {
      escapedFieldName = '*';
    }

    switch (connection.dialect) {
      default:
        return `COUNT(${escapedFieldName})`;
    }
  }
}

module.exports = CountSQLLiteral;
