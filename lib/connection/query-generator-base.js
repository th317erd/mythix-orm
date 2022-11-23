'use strict';

/// The base query generator class.
///
/// Alias: QueryGenerator
class QueryGeneratorBase {
  constructor(connection) {
    Object.defineProperties(this, {
      'connection': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        connection,
      },
    });
  }

  stackAssign(obj, ...args) {
    return this.connection.stackAssign(obj, ...args);
  }

  escape(...args) {
    return this.connection.escape(...args);
  }

  escapeID(...args) {
    return this.connection.escapeID(...args);
  }

  // eslint-disable-next-line no-unused-vars
  _averageLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_averageLiteralToString: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  _countLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_countLiteralToString: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  _distinctLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_distinctLiteralToString: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  _fieldLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_fieldLiteralToString: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  _maxLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_maxLiteralToString: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  _minLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_minLiteralToString: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  _sumLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_sumLiteralToString: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  toConnectionString(queryEngine, options) {
    return '<not supported by connection>';
  }
}

module.exports = QueryGeneratorBase;
