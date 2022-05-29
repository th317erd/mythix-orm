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

  async generateSelectQuery(queryEngine) {

  }

  getFieldDefaultValue(field, fieldName) {
    if (field.defaultValue === undefined)
      return;

    let defaultValue      = field.defaultValue;
    let useDefaultKeyword = true;

    if (typeof defaultValue === 'function') {
      defaultValue = defaultValue({ field, fieldName, connection: this.connection });
      if (!((field.defaultValue.mythixFlags || 0) & FLAG_REMOTE)) {
        defaultValue = this.connection.escape(defaultValue);
      } else if (typeof defaultValue === 'string' && defaultValue.charAt(0) === '!') {
        useDefaultKeyword = false;
        defaultValue = defaultValue.substring(1);
      }
    } else {
      defaultValue = this.connection.escape(defaultValue);
    }

    if (useDefaultKeyword)
      return `DEFAULT ${defaultValue}`;
    else
      return `${defaultValue}`;
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
      }

      let defaultValue = this.getFieldDefaultValue(field, fieldName);
      if (defaultValue !== undefined)
        constraintParts.push(defaultValue);

      constraintParts = constraintParts.join(' ');
      if (Nife.isNotEmpty(constraintParts))
        constraintParts = ` ${constraintParts}`;

      fieldParts.push(`  ${this.connection.escapeID(columnName)} ${field.type.toString(this)}${constraintParts}`);
    });

    return `CREATE TABLE IF NOT EXISTS ${this.connection.escapeID(Model.getTableName())} (${fieldParts.join(',\n')}\n);`;
  }
}

module.exports = QueryGeneratorBase;
