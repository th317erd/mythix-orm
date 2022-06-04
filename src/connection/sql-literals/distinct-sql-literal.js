'use strict';

const SQLLiteralBase  = require('./sql-literal-base');
const ModelUtils      = require('../../utils/model-utils');

class DistinctSQLLiteral extends SQLLiteralBase {
  constructor(fullyQualifiedName) {
    super();

    let definition = ModelUtils.parseQualifiedName(fullyQualifiedName);
    if (!definition || !definition.modelName || !definition.fieldNames.length)
      throw new TypeError(`DistinctSQLLiteral::constructor: Unable to find fully qualified name "${fullyQualifiedName}".`);

    Object.defineProperties(this, {
      'definition': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        definition,
      },
    });
  }

  toString(connection) {
    if (!connection)
      return `${this.constructor.name} {}`;

    let definition  = this.definition;
    let field       = connection.getField(definition.fieldNames[0], definition.modelName);
    if (!field)
      throw new Error(`DistinctSQLLiteral::toString: Unable to locate field "${definition.modelName}"."${definition.fieldNames[0]}".`);

    let escapedModelName  = connection.escapeID(field.Model.getModelName());
    let escapedFieldName  = connection.escapeID(field.fieldName);
    let escapedTableName  = connection.escapeID(field.Model.getTableName());
    let escapedColumnName = connection.escapeID(field.columnName);

    switch (connection.dialect) {
      default:
        return `DISTINCT ${escapedTableName}.${escapedColumnName} AS ${escapedModelName}.${escapedFieldName}`;
    }
  }
}

module.exports = DistinctSQLLiteral;
