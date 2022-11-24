'use strict';

const LiteralBase  = require('./literal-base');

/// This literal type simply defines a pure "literal".
/// It will represent any "raw" value you want to insert
/// directly into any query. This can be useful for custom
/// database operations, math, or anything else you need
/// in "raw" form in the database.
///
/// Note:
///   **Caution should be taken if using this for values. Values defined by this literal won't ever be escaped.**
class Literal extends LiteralBase {
  /// Construct a pure vanilla "literal" for the underlying database.
  ///
  /// Arguments:
  ///   literal: any
  ///     The literal value you want to stringify for the underlying database query engine.
  ///   options?: object
  ///     Connection, literal, and operation specific options.
  constructor(literal, options) {
    super(literal, options);
  }
}

module.exports = Literal;
