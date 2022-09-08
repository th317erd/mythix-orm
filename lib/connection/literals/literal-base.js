'use strict';

const ModelUtils = require('../../utils/model-utils');

class LiteralBase {
  static _isMythixLiteral = true;

  static isLiteralClass(value) {
    if (!value)
      return false;

    if (value.prototype instanceof LiteralBase)
      return true;

    if (value._isMythixLiteral)
      return true;

    return false;
  }

  static isLiteral(value) {
    if (!value)
      return false;

    if (value instanceof LiteralBase)
      return true;

    if (value.constructor && value.constructor._isMythixLiteral)
      return true;

    return false;
  }

  static isLiteralType(value) {
    return (this.isLiteral(value) && value.constructor && value.constructor.name === this.name);
  }

  constructor(literal, options) {
    Object.defineProperties(this, {
      'literal': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        literal,
      },
      'options': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        options || {},
      },
    });
  }

  fullyQualifiedNameToDefinition(fullyQualifiedName) {
    if (LiteralBase.isLiteral(fullyQualifiedName))
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
    if (LiteralBase.isLiteral(definition))
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

module.exports = LiteralBase;
