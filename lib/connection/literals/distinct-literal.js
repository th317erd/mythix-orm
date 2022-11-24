'use strict';

const LiteralFieldBase = require('./literal-field-base');

/// Define a "distinct" literal for the underlying database.
///
/// Literals are special types in Mythix ORM that are used to
/// define "literal values" for the underlying database.
///
/// This literal defines a "distinct" operation across a single
/// column, or entire rows. Distinct is a little different than
/// most other literals. Whereas most other literals are simply
/// used as "values"--either identifiers, methods, or values--this
/// `DistinctLiteral` defines an entire state for a query. It can modify
/// the projection, the order, or modify the query in other ways. On
/// some database drivers, it may cause the engine to take an entirely
/// different path to fetch the data requested. `DistinctLiteral` requires
/// a field, or another literal. It may or may not use this field in
/// underlying database operations. However, to remain
/// consistent in the interface and across databases, it is required.
/// There are some cases in which a `DistinctLiteral` may not alter how
/// a query is carried out. One of these is if it is used as an argument to another
/// literal. For example, if one were to use a `DistinctLiteral` inside a
/// `CountLiteral`, then it would count distinctly across rows, not modify how the
/// query is carried out: `new CountLiteral(new DistinctLiteral('User:id'))`.
///
/// There are two primary ways to access literals in Mythix ORM. The
/// first is to simply import them. The second way literals can be
/// accessed is via the connection class itself. All Mythix ORM connection
/// classes export all literals on the `static Literals` attribute of
/// the class. So for example, you could access literals like `SQLiteConnection.Literals.DistinctLiteral`.
///
/// All built-in Mythix ORM literals--except `Literal` and `LiteralBase`--accept a field
/// as their first argument. This field can be a fully qualified field name, an actual
/// <see>Field</see> instance, or another literal. The second argument to all literal constructors
/// is an `options` object, that generally contains connection-specific (and operation-specific) options...
/// however, there are common options that can be supplied, such as `as: string;` which allows you to
/// define an alias for the defined field, and `noProjectionAliases: boolean;`, which allows you to disable
/// the column alias entirely.
///
/// Note:
///   The `DistinctLiteral` is rarely used directly. Normally you would want to
///   call `query.DISTINCT` instead, which internally uses this literal.
///
/// Example:
///   const { Literals } = require('mythix-orm');
///   const { SQLiteConnection } = require('mythix-orm-sqlite');
///   let literal1 = new Literals.DistinctLiteral('User:firstName');
///   let literal2 = new SQLiteConnection.Literals.DistinctLiteral('User:firstName');
///
/// See: LiteralFieldBase
///
/// See: LiteralBase
class DistinctLiteral extends LiteralFieldBase {
  /// Convert this literal to a string to be used in a database query.
  ///
  /// This method proxies the conversion of this literal to the connection
  /// by calling <see>Connection.literalToString</see>. If no connection
  /// is provided when this is called, then the literal will be converted
  /// to a string representing it for debugging, i.e. `'CountLiteral {}'`.
  ///
  /// Note:
  ///   Ultimately, for most connections, this will end up calling
  ///   <see>QueryGenerator._distinctLiteralToString</see>.
  ///
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     The connection to use to stringify this literal. If none is provided,
  ///     then a string representing this object will be returned instead.
  ///   options?: object
  ///     A connection and operation specific set of options that can be provided.
  ///     This might for example be `{ as: 'name' }` to provided a field alias, or
  ///     `{ isProjection: true }` to define that this is being stringified for use
  ///     as a field in the query projection. Normally the end-user won't care about
  ///     any literal options, except `as`, which is commonly used to give your literal
  ///     an alias.
  toString(connection, options) {
    if (!connection)
      return `${this.constructor.name} {}`;

    return connection.literalToString(this, options);
  }
}

module.exports = DistinctLiteral;
