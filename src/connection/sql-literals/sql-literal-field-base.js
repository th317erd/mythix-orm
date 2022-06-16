'use strict';

const SQLLiteralBase  = require('./sql-literal-base');

class SQLLiteralFieldBase extends SQLLiteralBase {
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
}

module.exports = SQLLiteralFieldBase;
