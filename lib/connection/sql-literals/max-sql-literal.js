'use strict';

const SQLLiteralBase      = require('./sql-literal-base');
const SQLLiteralFieldBase = require('./sql-literal-field-base');

class MaxSQLLiteral extends SQLLiteralFieldBase {
  toString(connection) {
    if (!connection)
      return `${this.constructor.name} {}`;

    let field             = this.definitionToField(connection, this.definition);
    let queryGenerator    = connection.getQueryGenerator();
    let escapedFieldName;

    if (field instanceof SQLLiteralBase)
      escapedFieldName = field.toString(connection);
    else
      escapedFieldName = queryGenerator.getEscapedColumnName(field);

    switch (connection.dialect) {
      default:
        return `MAX(${escapedFieldName})`;
    }
  }
}

module.exports = MaxSQLLiteral;
