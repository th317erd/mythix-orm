'use strict';

const Nife            = require('nife');
const { FLAG_REMOTE } = require('../helpers/default-helpers');

class QueryGeneratorBase {
  constructor(connection) {
    Object.defineProperties(this, {
      'connection': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        connection,
      },
    });
  }

  async generatorCreateTableStatement(Model/*, options */) {
    let fieldParts = [];

    Model.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      let columnName      = field.field || fieldName;
      let constraintParts = [];

      if (field.primaryKey) {
        constraintParts.push('PRIMARY KEY');
      } else {
        if (field.unique === true)
          constraintParts.push('UNIQUE');

        if (field.allowNull === false)
          constraintParts.push('NOT NULL');

        if (field.defaultValue !== undefined) {
          let defaultValue = field.defaultValue;
          if (typeof defaultValue === 'function') {
            defaultValue = defaultValue({ field, fieldName, connection: this });
            if (!((field.defaultValue.mythixFlags || 0) & FLAG_REMOTE))
              defaultValue = this.connection.escape(defaultValue);
          } else {
            defaultValue = this.connection.escape(defaultValue);
          }

          constraintParts.push(`DEFAULT ${defaultValue}`);
        }
      }

      constraintParts = constraintParts.join(' ');
      if (Nife.isNotEmpty(constraintParts))
        constraintParts = ` ${constraintParts}`;

      fieldParts.push(`  ${this.connection.escapeID(columnName)} ${field.type.toString(this)}${constraintParts}`);
    });

    return `CREATE TABLE IF NOT EXISTS ${this.connection.escapeID(Model.getTableName())} (${fieldParts.join(',\n')}\n);`;
  }
}

module.exports = QueryGeneratorBase;
