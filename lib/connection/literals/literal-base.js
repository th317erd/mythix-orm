'use strict';

const ModelUtils = require('../../utils/model-utils');
const Field = require('../../field');

/// `LiteralBase` is the class all other literals inherit from.
///
/// Literals are special types in Mythix ORM that are used to
/// define "literal values" for the underlying database.
///
/// This literal--being the top most ancestor of all other literals--
/// defines common behavior for literals.
///
/// See: AverageLiteral
///
/// See: CountLiteral
///
/// See: DistinctLiteral
///
/// See: FieldLiteral
///
/// See: Literal
///
/// See: MaxLiteral
///
/// See: MinLiteral
///
/// See: SumLiteral
///
class LiteralBase {
  /// Assist with type-checking
  static _isMythixLiteral = true;

  /// Use this method to check if a class
  /// is a Mythix ORM Literal. It will return
  /// `true` if the provided value is a class
  /// that inherits from <see>LiteralBase</see>, or
  /// if the provided value has an attribute
  /// named `_isMythixLiteral` that is truthy.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: Function
  ///     Value to check.
  static isLiteralClass(value) {
    if (!value)
      return false;

    if (value.prototype instanceof LiteralBase)
      return true;

    if (value._isMythixLiteral)
      return true;

    return false;
  }

  /// Check to see if the provided value is
  /// an *instance* of a Mythix ORM <see>LiteralBase</see>.
  /// Unlike <see>LiteralBase.static isLiteralClass</see>, which
  /// checks if a *class* is a <see>LiteralBase</see>, this will check
  /// to see if an *instance* is an instance of a
  /// Mythix ORM <see>LiteralBase</see>. It will return
  /// `true` if the provided value is an `instanceof`
  /// <see>LiteralBase</see>, or if the value's `constructor`
  /// property has a truthy `_isMythixLiteral` property
  /// (`value.constructor._isMythixLiteral`)
  ///
  /// Note:
  ///   This method is also a matching instance method.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     Value to check
  static isLiteral(value) {
    if (!value)
      return false;

    if (value instanceof LiteralBase)
      return true;

    if (value.constructor && value.constructor._isMythixLiteral)
      return true;

    return false;
  }

  isLiteral(value) {
    if (arguments.length === 0)
      return true;

    return this.constructor.isLiteral(value);
  }

  /// Check to see if the provided value is
  /// an *instance* of *`this`* literal type.
  /// Unlike <see>LiteralBase.static isLiteral</see>, which
  /// checks if an *instance* is any <see>LiteralBase</see>
  /// type, this will check if the provided literal is
  /// exactly *`this`* type of literal. It will return
  /// `true` if the provided value is an `instanceof`
  /// `this` literal class, or if the value's `constructor`
  /// property has a name that is equal to `this` literal's
  /// class name.
  /// (`value.constructor.name === this.name`)
  ///
  /// Example:
  ///   console.log(CountLiteral.isLiteralType(new AverageLiteral('User:age')))
  ///   // false
  ///
  ///   console.log(CountLiteral.isLiteralType(new CountLiteral('*')))
  ///   // true
  ///
  /// Note:
  ///   This method is also a matching instance method.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     Value to check
  static isLiteralType(value) {
    if (value && value instanceof this)
      return true;

    return (this.isLiteral(value) && value.constructor && value.constructor.name === this.name);
  }

  /// Used internally in the engine to know if
  /// a literal being operated upon is an aggregating
  /// literal or not.
  ///
  /// Note:
  ///   This method is also a matching instance method.
  ///
  /// Return: boolean
  ///   Return `true`, informing the caller that this literal is used for aggregate operations.
  static isAggregate() {
    return false;
  }

  isAggregate() {
    return this.constructor.isAggregate();
  }

  /// Construct a new literal.
  ///
  /// Arguments:
  ///   literal: string
  ///     The literal value you wish to insert into the query.
  ///   options?: object
  ///     Literal, connection, and operation specific options.
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

  /// Take the value provided as either a <see>Field</see>
  /// instance, or a fully qualified field name, then return the field
  /// definition for it.
  ///
  /// If a literal is provided instead, then simply return the literal
  /// without modifying it.
  ///
  /// Note:
  ///   This is the inverse operation of <see>LiteralBase.definitionToField</see>
  ///
  /// See: ModelUtils.parseQualifiedName
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

  /// Take a field definition and return the actual <see>Field</see>
  /// instance for it.
  ///
  /// If a literal is provided... or a value that isn't understood
  /// (such as a string), then it is simply returned unmodified.
  ///
  /// Note:
  ///   This is the inverse operation of <see>LiteralBase.fullyQualifiedNameToDefinition</see>
  ///
  /// See: ModelUtils.parseQualifiedName
  definitionToField(connection, definition) {
    if (LiteralBase.isLiteral(definition))
      return definition;

    if (!definition || !definition.fieldNames)
      return definition;

    let field = connection.getField(definition.fieldNames[0], definition.modelName);
    if (!field) {
      if (definition.fieldNames[0] && definition.modelName) {
        let Model = connection.getModel(definition.modelName);
        if (Model) {
          return new Field({
            ephemeral: true,
            fieldName: definition.fieldNames[0],
            Model,
          });
        }
      }

      throw new Error(`${this.constructor.name}::definitionToField: Unable to locate field "${definition.modelName}"."${definition.fieldNames[0]}".`);
    }

    return field;
  }

  /// Convert the literal value provided to the `constructor`
  /// to a string.
  ///
  /// Return: string
  ///   The value provided to the `constructor` as a string.
  toString() {
    if (!this.literal)
      return ('' + this.literal);

    return this.literal.toString();
  }

  /// Return the raw value provided to the `constructor`.
  ///
  /// Return: any
  ///   The raw value provided to the `constructor`.
  valueOf() {
    return this.literal;
  }
}

module.exports = LiteralBase;
