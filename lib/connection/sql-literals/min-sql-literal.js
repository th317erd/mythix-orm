'use strict';

const SQLLiteralBase      = require('./sql-literal-base');
const SQLLiteralFieldBase = require('./sql-literal-field-base');

class MinSQLLiteral extends SQLLiteralFieldBase {
  toString(connection) {
    if (!connection)
      return `${this.constructor.name} {}`;

    let field             = this.definitionToField(connection, this.definition);
    let queryGenerator    = connection.getQueryGenerator();
    let escapedFieldName;

    if (field instanceof SQLLiteralBase)
      escapedFieldName = field.toString(connection);
    else
      escapedFieldName = queryGenerator.getEscapedColumnName(field.Model, field, this.options);

    switch (connection.dialect) {
      default:
        return `MIN(${escapedFieldName})`;
    }
  }
}

module.exports = MinSQLLiteral;
