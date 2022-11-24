'use strict';

const Nife        = require('nife');
const LiteralBase = require('./literal-base');

/// LiteralFieldBase is the ancestor that all
/// literals dealing with fields inherit from
/// (which is nearly all literals). Its primary
/// function is to enforce being provided fields
/// when the child literal requires them, and to
/// fetch those fields when the engine needs them.
///
/// See: LiteralBase
///
/// See: Literal
class LiteralFieldBase extends LiteralBase {
  /// This is provided so each child class
  /// can overload it. By default it returns
  /// `true`. However, any literal that inherits
  /// from this literal may override it to instead
  /// return `false`. The <see>CountLiteral</see>
  /// does exactly this for example.
  ///
  /// Return: boolean
  ///   Returns `true` by default, children can change the return value.
  static isFieldRequired() {
    return true;
  }

  /// Construct a new field-based literal. The
  /// first argument should be a fully qualified field name,
  /// a <see>Field</see> instance, or another literal.
  ///
  /// Arguments:
  ///   field: string | <see>Field</see> | <see>LiteralBase</see>
  ///     The "field" that this literal is representing. If this is a
  ///     string, then it is expected to be a fully qualified field name.
  ///     If instead this is a <see>Field</see>, then that defines the
  ///     field. Finally, this can also be another literal.
  ///   options?: object
  ///     Connection, Literal, and operation specific options for this literal.
  constructor(fullyQualifiedName, options) {
    super(undefined, options);

    let isRequired = this.constructor.isFieldRequired();
    let definition;

    if (isRequired || fullyQualifiedName) {
      try {
        definition = this.fullyQualifiedNameToDefinition(fullyQualifiedName);
      } catch (error) {
        if (isRequired)
          throw error;

        definition = fullyQualifiedName;
      }
    }

    Object.defineProperties(this, {
      'definition': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        definition,
      },
    });
  }

  /// Get the fully qualified field name of the field
  /// that this literal is representing.
  ///
  /// Return: string
  ///   The fully qualified field name of the field this literal
  ///   is representing. This would be the same field provided to the
  ///   `constructor` when the literal was first created.
  ///
  /// See: ModelUtils.parseQualifiedName
  getFullyQualifiedFieldName() {
    let definition = this.definition || {};
    if (!definition.modelName || Nife.isEmpty(definition.fieldNames))
      return;

    return `${definition.modelName}:${definition.fieldNames[0]}`;
  }

  /// Get the field represented by this literal.
  ///
  /// Return: <see>Field</see>
  ///   The field that this literal is representing. This will
  ///   be the field given to the `constructor` when the literal
  ///   was first created.
  getField(connection) {
    if (!this.definition)
      return;

    return this.definitionToField(connection, this.definition);
  }

  /// Return the raw field definition for
  /// the provided field.
  ///
  /// See: ModelUtils.parseQualifiedName
  valueOf() {
    return this.definition;
  }
}

module.exports = LiteralFieldBase;
