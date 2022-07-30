'use strict';

const ModelUtils = require('../../utils/model-utils');

class SQLLiteralBase {
  constructor(literal) {
    Object.defineProperties(this, {
      'literal': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        literal,
      },
    });
  }

  fullyQualifiedNameToDefinition(fullyQualifiedName) {
    if (fullyQualifiedName instanceof SQLLiteralBase)
      return fullyQualifiedName;

    if (!fullyQualifiedName)
      throw new TypeError(`${this.constructor.name}::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "${fullyQualifiedName}".`);

    let definition;
    if (fullyQualifiedName.Model && fullyQualifiedName.fieldName) {
      definition = {
        modelName:  fullyQualifiedName.Model.getModelName(),
        fieldNames: [ fullyQualifiedName.fieldName ],
      };
    } else {
      definition = ModelUtils.parseQualifiedName(fullyQualifiedName);
    }

    if (!definition || !definition.modelName || !definition.fieldNames.length)
      throw new TypeError(`${this.constructor.name}::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "${fullyQualifiedName}".`);

    return definition;
  }

  definitionToField(connection, definition) {
    if (definition instanceof SQLLiteralBase)
      return definition;

    let field = connection.getField(definition.fieldNames[0], definition.modelName);
    if (!field)
      throw new Error(`${this.constructor.name}::definitionToField: Unable to locate field "${definition.modelName}"."${definition.fieldNames[0]}".`);

    return field;
  }

  toString() {
    return this.literal;
  }
}

module.exports = SQLLiteralBase;
