'use strict';

const SQLLiteralBase      = require('./sql-literal-base');
const SQLLiteralFieldBase = require('./sql-literal-field-base');

class DistinctSQLLiteral extends SQLLiteralFieldBase {
  toString(connection) {
    if (!connection)
      return `${this.constructor.name} {}`;

    let field           = this.definitionToField(connection, this.definition);
    let queryGenerator  = connection.getQueryGenerator();

    switch (connection.dialect) {
      default:
        if (field instanceof SQLLiteralBase)
          return `DISTINCT ${field.toString(connection)}`;

        return `DISTINCT ${queryGenerator.getEscapedProjectionName(field)}`;
    }
  }
}

module.exports = DistinctSQLLiteral;
