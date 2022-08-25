'use strict';

const Nife        = require('nife');
const LiteralBase = require('./literal-base');

class LiteralFieldBase extends LiteralBase {
  static isFieldRequired() {
    return true;
  }

  constructor(fullyQualifiedName) {
    super();

    let isRequired = this.constructor.isFieldRequired();
    let definition;

    if (isRequired || fullyQualifiedName) {
      try {
        definition = this.fullyQualifiedNameToDefinition(fullyQualifiedName);
      } catch (error) {
        if (isRequired)
          throw error;
      }
    }

    Object.defineProperties(this, {
      'definition': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        definition,
      },
    });
  }

  getFullyQualifiedFieldName() {
    let definition = this.definition || {};
    if (!definition.modelName || Nife.isEmpty(definition.fieldNames))
      return;

    return `${definition.modelName}:${definition.fieldNames[0]}`;
  }

  getField(connection) {
    if (!this.definition)
      return null;

    return this.definitionToField(connection, this.definition);
  }
}

module.exports = LiteralFieldBase;
