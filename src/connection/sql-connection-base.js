'use strict';

const Nife            = require('nife');
const ConnectionBase  = require('./connection-base');
const { FLAG_REMOTE } = require('../helpers/default-helpers');

class SQLConnectionBase extends ConnectionBase {
  constructor(_options) {
    super(_options);

    Object.defineProperties(this, {
      'queryGenerator': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
    });
  }

  getQueryGenerator() {
    return this.queryGenerator;
  }

  setQueryGenerator(queryGenerator) {
    this.queryGenerator = queryGenerator;
  }

  async generatorCreateTableStatement(Model, options) {
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
              defaultValue = this.escape(defaultValue);
          } else {
            defaultValue = this.escape(defaultValue);
          }

          constraintParts.push(`DEFAULT ${defaultValue}`);
        }
      }

      constraintParts = constraintParts.join(' ');
      if (Nife.isNotEmpty(constraintParts))
        constraintParts = ` ${constraintParts}`;

      fieldParts.push(`  ${this.escapeID(columnName)} ${field.type.toString(this)}${constraintParts}`);
    });

    return `CREATE TABLE IF NOT EXISTS ${this.escapeID(Model.getTableName())} (${fieldParts.join(',\n')}\n);`;
  }
}

module.exports = SQLConnectionBase;
