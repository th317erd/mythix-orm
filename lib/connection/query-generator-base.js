'use strict';

/// The base query generator class.
///
/// A "query generator" is an interface that will take
/// parameters (usually a <see>QueryEngine</see> or a
/// <see>Model</see>) and generate database query statements
/// from the input. For SQL type databases this would mean
/// generating `SELECT`, `INSERT`, `UPDATE`, and `DELETE`
/// statements, as well as generators for creating and altering
/// tables, among other things.
///
/// The methods of this class are generally many, with the
/// design pattern for most generators being that nearly
/// all methods are split apart and added to the class, allowing
/// by deliberate design much finer control when using overloaded
/// methods, to modify or replace any parts of the generator.
///
/// Any connection can be provided a custom query generator
/// interface via the `queryGenerator` option that can be
/// used when instantiating a connection. Most connections
/// will supply their own by default.
///
/// A connection should always be bound to a query generator
/// instance. If not when first created, then at least when
/// provided to a connection. The connection is a required
/// part of the generator interface, and will do things, such
/// as for example, escaping values and ids using the connection
/// itself. The connection may be used for other operations as
/// well.
///
/// Note:
///   Some database drivers may not have a generator at all. Though
///   database drivers commonly do have a database statement generator,
///   a connection isn't required to have one.
///
/// Alias: QueryGenerator
class QueryGeneratorBase {
  /// Construct a new query generator
  ///
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     The connection that this interface is for. Sometimes the connection
  ///     isn't yet available when creating the query generator, so this argument
  ///     is optional. When provided to a <see>Connection</see>, the connection
  ///     will call <see>QueryGenerator.setConnection</see> with itself to set
  ///     the connection for the query generator.
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

  /// Get the <see>Connection</see> bound to this
  /// query generator.
  ///
  /// Return: <see>Connection</see>
  ///   The connection bound to this query generator. A connection
  ///   should always be bound before any generating methods of
  ///   the class are called.
  getConnection() {
    return this.connection;
  }

  /// Set the <see>Connection</see> bound to this
  /// query generator.
  ///
  /// Arguments:
  ///   connection: <see>Connection</see>
  ///     The connection to bind to this query generator.
  ///
  /// Return: <see>QueryGenerator</see>
  ///   Return `this` to allow for chaining.
  setConnection(connection) {
    this.connection = connection;
    return this;
  }

  /// This call proxies to <see>Connection.stackAssign</see>.
  /// Refer to the documentation of that method for more information.
  ///
  /// See: Connection.stackAssign
  stackAssign(obj, ...args) {
    return this.connection.stackAssign(obj, ...args);
  }

  /// This call proxies to <see>Connection.escape</see>.
  /// Refer to the documentation of that method for more information.
  ///
  /// See: Connection.escape
  escape(...args) {
    return this.connection.escape(...args);
  }

  /// This call proxies to <see>Connection.escapeID</see>.
  /// Refer to the documentation of that method for more information.
  ///
  /// See: Connection.escapeID
  escapeID(...args) {
    return this.connection.escapeID(...args);
  }

  /// Convert an <see>AverageLiteral</see> into a
  /// string representation for the underlying database.
  ///
  /// It is expected that each database driver will implement
  /// this method. By default it will simply throw an
  /// "unsupported" error.
  ///
  /// Refer to the specific documentation for your database
  /// driver for more information.
  ///
  /// Arguments:
  ///   literal: <see>AverageLiteral</see>
  ///     The literal to stringify for the underlying database.
  ///   options?: object
  ///     Options for the stringify process. These are often database
  ///     driver specific. However, one common option is the `as`
  ///     option, which will allow you to give your literal an alias.
  ///
  /// Return: string
  ///   The literal, converted into the proper string for the underlying
  ///   database.
  // eslint-disable-next-line no-unused-vars
  _averageLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_averageLiteralToString: This operation is not supported for this connection type.`);
  }

  /// Convert an <see>CountLiteral</see> into a
  /// string representation for the underlying database.
  ///
  /// It is expected that each database driver will implement
  /// this method. By default it will simply throw an
  /// "unsupported" error.
  ///
  /// Refer to the specific documentation for your database
  /// driver for more information.
  ///
  /// Arguments:
  ///   literal: <see>CountLiteral</see>
  ///     The literal to stringify for the underlying database.
  ///   options?: object
  ///     Options for the stringify process. These are often database
  ///     driver specific. However, one common option is the `as`
  ///     option, which will allow you to give your literal an alias.
  ///
  /// Return: string
  ///   The literal, converted into the proper string for the underlying
  ///   database.
  // eslint-disable-next-line no-unused-vars
  _countLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_countLiteralToString: This operation is not supported for this connection type.`);
  }

  /// Convert an <see>DistinctLiteral</see> into a
  /// string representation for the underlying database.
  ///
  /// It is expected that each database driver will implement
  /// this method. By default it will simply throw an
  /// "unsupported" error.
  ///
  /// Refer to the specific documentation for your database
  /// driver for more information.
  ///
  /// Arguments:
  ///   literal: <see>DistinctLiteral</see>
  ///     The literal to stringify for the underlying database.
  ///   options?: object
  ///     Options for the stringify process. These are often database
  ///     driver specific. However, one common option is the `as`
  ///     option, which will allow you to give your literal an alias.
  ///
  /// Return: string
  ///   The literal, converted into the proper string for the underlying
  ///   database.
  // eslint-disable-next-line no-unused-vars
  _distinctLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_distinctLiteralToString: This operation is not supported for this connection type.`);
  }

  /// Convert an <see>FieldLiteral</see> into a
  /// string representation for the underlying database.
  ///
  /// It is expected that each database driver will implement
  /// this method. By default it will simply throw an
  /// "unsupported" error.
  ///
  /// Refer to the specific documentation for your database
  /// driver for more information.
  ///
  /// Arguments:
  ///   literal: <see>FieldLiteral</see>
  ///     The literal to stringify for the underlying database.
  ///   options?: object
  ///     Options for the stringify process. These are often database
  ///     driver specific. However, one common option is the `as`
  ///     option, which will allow you to give your literal an alias.
  ///
  /// Return: string
  ///   The literal, converted into the proper string for the underlying
  ///   database.
  // eslint-disable-next-line no-unused-vars
  _fieldLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_fieldLiteralToString: This operation is not supported for this connection type.`);
  }

  /// Convert an <see>MaxLiteral</see> into a
  /// string representation for the underlying database.
  ///
  /// It is expected that each database driver will implement
  /// this method. By default it will simply throw an
  /// "unsupported" error.
  ///
  /// Refer to the specific documentation for your database
  /// driver for more information.
  ///
  /// Arguments:
  ///   literal: <see>MaxLiteral</see>
  ///     The literal to stringify for the underlying database.
  ///   options?: object
  ///     Options for the stringify process. These are often database
  ///     driver specific. However, one common option is the `as`
  ///     option, which will allow you to give your literal an alias.
  ///
  /// Return: string
  ///   The literal, converted into the proper string for the underlying
  ///   database.
  // eslint-disable-next-line no-unused-vars
  _maxLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_maxLiteralToString: This operation is not supported for this connection type.`);
  }

  /// Convert an <see>MinLiteral</see> into a
  /// string representation for the underlying database.
  ///
  /// It is expected that each database driver will implement
  /// this method. By default it will simply throw an
  /// "unsupported" error.
  ///
  /// Refer to the specific documentation for your database
  /// driver for more information.
  ///
  /// Arguments:
  ///   literal: <see>MinLiteral</see>
  ///     The literal to stringify for the underlying database.
  ///   options?: object
  ///     Options for the stringify process. These are often database
  ///     driver specific. However, one common option is the `as`
  ///     option, which will allow you to give your literal an alias.
  ///
  /// Return: string
  ///   The literal, converted into the proper string for the underlying
  ///   database.
  // eslint-disable-next-line no-unused-vars
  _minLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_minLiteralToString: This operation is not supported for this connection type.`);
  }

  /// Convert an <see>SumLiteral</see> into a
  /// string representation for the underlying database.
  ///
  /// It is expected that each database driver will implement
  /// this method. By default it will simply throw an
  /// "unsupported" error.
  ///
  /// Refer to the specific documentation for your database
  /// driver for more information.
  ///
  /// Arguments:
  ///   literal: <see>SumLiteral</see>
  ///     The literal to stringify for the underlying database.
  ///   options?: object
  ///     Options for the stringify process. These are often database
  ///     driver specific. However, one common option is the `as`
  ///     option, which will allow you to give your literal an alias.
  ///
  /// Return: string
  ///   The literal, converted into the proper string for the underlying
  ///   database.
  // eslint-disable-next-line no-unused-vars
  _sumLiteralToString(literal, options) {
    throw new Error(`${this.constructor.name}::_sumLiteralToString: This operation is not supported for this connection type.`);
  }

  /// Take a <see>QueryEngine</see> instance and
  /// convert it into a query. For SQL type databases
  /// this would turn a <see>QueryEngine</see> into a
  /// `SELECT` statement. For other types of databases,
  /// this should return a "fetch" query--or string representation
  /// of such a query--in the database's native query language.
  ///
  /// Arguments:
  ///   queryEngine: <see>QueryEngine</see>
  ///     The query engine instance to stringify.
  ///   options?: object
  ///     Connection and operation specific options. These
  ///     generally aren't needed, but are provided in case
  ///     the underlying connection needs them.
  ///
  /// Return: string
  ///   A "fetch" query in the databases native query language,
  ///   generated from the provided `queryEngine`.
  // eslint-disable-next-line no-unused-vars
  toConnectionString(queryEngine, options) {
    return '<not supported by connection>';
  }

  /// Get the "default value" for the given field
  /// for the underlying database. This is used
  /// primarily for "CREATE TABLE" statements.
  ///
  /// By default, the implementation of this method
  /// is empty. It is expected that each database driver
  /// will implement their own version of this method.
  ///
  /// Arguments:
  ///   field: <see>Field</see>
  ///     The field instance we are getting a "default value"
  ///     from.
  ///   fieldName: string
  ///     The name of the field that we are getting the "default value"
  ///     from. This should always be the same as `field.fieldName`.
  ///   options?: object
  ///     Options for the operation. These will likely be connection
  ///     specific. Please refer to the documentation of your specific
  ///     connection for more details.
  ///
  /// Return: any
  ///   Though in most cases this method will return a string for
  ///   most database drivers in most situations, it may return other
  ///   types as well, such as literals, or other raw values.
  ///   Please refer to the documentation of your specific
  ///   connection for more details.
  // eslint-disable-next-line no-unused-vars
  getFieldDefaultValue(field, fieldName, _options) {
  }
}

module.exports = QueryGeneratorBase;
