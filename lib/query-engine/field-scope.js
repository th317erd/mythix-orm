'use strict';

const Nife            = require('nife');
const ProxyClass      = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');

function addOperatorToQuery(name, inverseName, value, extraOptions) {
  if (Nife.instanceOf(value, 'object'))
    throw new Error(`QueryEngine::addOperatorToQuery: ${name}(${value}) makes no sense...`);

  let conditionalParams = {
    condition:       true,
    operator:        name,
    queryProp:       name,
    queryExtraArgs:  (extraOptions) ? [ extraOptions ] : [],
    inverseOperator: inverseName,
    value:           this._fetchOperatorValue(value),
    hasCondition:    true,
  };

  if (extraOptions)
    conditionalParams = Object.assign(conditionalParams, extraOptions);

  this._pushOperationOntoStack(conditionalParams);

  return this._fetchScope('model');
}

function wrapAnyAll(func) {
  const checkQueryProjection = (_query, subType) => {
    let query         = _query;
    let queryContext  = (QueryEngineBase.isQuery(query)) ? query.getOperationContext() : null;
    if (!queryContext || !queryContext.hasCondition)
      throw new Error(`QueryEngine::FieldScope::${subType}: Provided value must be a query with conditions.`);

    if (!queryContext.projection) {
      let Model   = queryContext.Model;
      let pkField = Model.getPrimaryKeyField();

      if (pkField)
        query = query.clone().PROJECT(pkField);
      else
        throw new Error(`QueryEngine::FieldScope::${subType}: Provided query must have only a single field projected.`);
    }

    return query;
  };

  func.ANY = (_query) => {
    return func.call(this, checkQueryProjection(_query, 'ANY'), { subType: 'ANY' });
  };

  func.ALL = (_query) => {
    return func.call(this, checkQueryProjection(_query, 'ANY'), { subType: 'ALL' });
  };

  return func;
}

/// `FieldScope` is the "field level" of a query.
/// It manages conditions, such as things like `EQ`, `NEQ`,
/// `GT`, `LT`, etc...
///
/// Any operator completed on this "field scope" will return
/// the previous `ModelScope` so the user can continue chaining
/// operations on the query. All operators at this level (except
/// `NOT`, `AND`, and `OR`) must be called and provided a value.
///
/// See: QueryEngineBase
///
/// See: QueryEngine
///
/// See: ModelScope
///
/// Note:
///   `FieldScope` is a sub-part of the `QueryEngine`, and so is generally referred to
///   simply as the `QueryEngine` as a whole. This is also the case for <see>QueryEngineBase</see>,
///   and <see>ModelScope</see>, which also make up the `QueryEngine` as sub-parts,
///   and so are also often referred to simply as "the query engine".
class FieldScope extends QueryEngineBase {
  /// Invert the logic of the following operator.
  ///
  /// This method does not need to be called.
  ///
  /// Unlike `AND` and `OR` operators, this is not a permanent toggle.
  /// As soon as a following operator is encountered, the `NOT` operation
  /// will be "turned off". `NOT` will invert any operation, so for example
  /// if you did a `User.where.id.NOT.EQ('test')` then this is the same as
  /// `User.where.id.NEQ('test')`--selecting everything NOT EQUAL TO `'test'`.
  /// `NOT` can also be used in combination with `EXISTS`, `ANY`, `ALL`, `IN`,
  /// etc... inverting any operator following `NOT`.
  ///
  /// Note:
  ///   `NOT` is only ever used once, and then it is toggled back off. For example,
  ///   if we did: `User.where.id.NOT.EQ('test').AND.lastName.EQ('Smith')`, then
  ///   we would have a query where `id != 'test' AND lastName = 'Smith'`. As you
  ///   can see, the `NOT` doesn't apply to the second `lastName` operator, as it
  ///   was turned off as soon as the first `EQ` operator was encountered on the `id`
  ///   field.
  ///
  /// SyntaxType: FunctionDeclaration
  NOT = ProxyClass.autoCall(function() {
    this._pushOperationOntoStack({ logical: true, operator: 'NOT', queryProp: 'NOT', not: !this.getOperationContext().not });
    return this[ProxyClass.PROXY];
  });

  /// This method assist in conversion of provided values to conditions.
  ///
  /// For example, if you provide a conditional operator (i.e. `EQ`) with
  /// a model, i.e. `Role.where.userID.EQ(User)`, then this method will
  /// look up the primary key field of the model, and use that to create
  /// a table-join.
  ///
  /// If instead you provided a model *instance* as a value, then the primary
  /// key field of that model will be looked up, and the resulting value from
  /// the instance will be used as the value. For example: `Role.where.userID.EQ(userInstance)`
  /// is equivalent to `Role.where.userID.EQ(userInstance.id)`.
  ///
  /// Aside from these two exceptions (model classes, and model instances)
  /// this method will simply return the value it was provided.
  ///
  /// This method is called on **every** value provided to **every** call
  /// to a conditional operator. It can be overloaded if you want to mutate
  /// specific values going into conditional operations.
  ///
  /// Arguments:
  ///   value: any
  ///     The value being provided to a conditional operator.
  ///
  /// Return: any
  ///   If `value` is a model class, then return a table-join query with that model. If
  ///   `value` is a model instance, then return the value of the model's primary key field.
  ///   Otherwise, return the `value` provided.
  _fetchOperatorValue(_value) {
    let value = _value;
    if (!value)
      return value;

    if (value._isMythixModel) {
      // If a model class was supplied, then get the model query engine
      // and the primary key of the model

      let pkFieldName = value.getPrimaryKeyFieldName();
      if (Nife.isEmpty(pkFieldName))
        throw new Error(`${this.constructor.name}::_fetchOperatorValue: Invalid operation: You asked me to match against a model class, but model has no primary key, so I do not know what to match against.`);

      return value.where(this.getConnection())[pkFieldName];
    } else if (value._mythixModelInstance) {
      // If a model instance was supplied
      // then get the model's PK value
      let pkFieldName = value.getPrimaryKeyFieldName();
      if (Nife.isEmpty(pkFieldName))
        throw new Error(`${this.constructor.name}::_fetchOperatorValue: Invalid operation: You asked me to match against a model instance, but model has no primary key, so I do not know what to match against.`);

      return value[pkFieldName];
    }

    return value;
  }

  /// "equals" condition.
  ///
  /// This will check equality with the value(s) provided.
  ///
  /// If `null`, `true`, or `false` are provided as a value,
  /// then the underlying database driver will convert it to
  /// the proper query. For example, given a SQL-type database
  /// driver, these values would be generated in the query as
  /// `"table"."column" IS NULL`, `"table"."column" IS TRUE`, or `"table"."column" IS FALSE`.
  ///
  /// If an array is provided as the value, then this stops being an "equals",
  /// and instead will turn into an `IN` operator (in SQL-type databases).
  /// For example, given the following query: `User.where.id.EQ([ 1, 2, 3, 4 ])`,
  /// the resulting SQL query would be `... WHERE "users"."id" IN(1,2,3,4)`. If a `null`,
  /// `true`, or `false` value is encountered in the array, then the condition
  /// will be grouped, and these values will be split out and handled separately.
  /// For example: `User.where.id.EQ([ 1, 2, 3, null, true, false ])` would result
  /// in the following SQL: `("users"."id" IS NULL OR "users"."id" IS TRUE OR "users"."id" IS FALSE OR "users"."id" IN(1,2,3))`.
  /// If the array provided is empty, than an exception will be thrown. This is for safety reasons.
  /// For example, if one were to do `User.where.id.EQ([]).destroy()`, then the result
  /// would be `DELETE FROM USERS`... truncating your entire table. This is obviously not
  /// at all ideal... so Mythix ORM will throw an exception if an empty array is encountered,
  /// requiring that the user be explicit.
  ///
  /// This condition also has two other variants, `EQ.ALL`, and `EQ.ANY` to check
  /// equality with a sub-query. For example, a `User.where.firstName.EQ.ANY(User.where.age.GTE(18).PROJECT('firstName'))`
  /// would generate the following SQL query: `... WHERE "users"."firstName" = ANY(SELECT "users"."firstName" FROM "users" WHERE "users"."age" >= 18)`,
  /// checking if the users first name matches any of the adult user names in the database.
  /// `ALL` is identical in how it functions, except it uses the `ALL` SQL operator instead of the `ANY` operator.
  /// `ANY` will check to see if any of the resulting rows match, `ALL` (for this conditional only) will also check
  /// if any of the resulting rows of the sub-query match. `ALL` behaves differently than `ANY` for `GT`, `GTE`, `LT`, or `LTE` operators.
  /// For `EQ` and `NEQ` operators, `ANY` and `ALL` behave identically.
  ///
  /// If a sub-query is provided as a value, then one of two things will happen:
  /// 1) If the sub-query has no conditions, then a table-join operation will be the result.
  /// 2) If the sub-query has conditions, then a sub-query will be executed using an `IN` operator.
  /// For example, given the following query: `User.where.id.EQ(User.where.firstName.EQ('Bob').PROJECT('id'))`,
  /// the sub-query **does have conditions** (`EQ('Bob')`), so the following SQL query that would be generated
  /// would be `... WHERE "users"."id" IN (SELECT "users"."id" FROM "users" WHERE "users"."firstName" = 'Bob')`.
  /// If instead a sub-query is provided as a value that **does not have conditions**, such as
  /// `User.where.id.EQ(Role.where.userID)`, then this is specifying a table join, joining on the `"roles"` table
  /// where `"users"."id" = "roles"."userID"`.
  ///
  /// Note:
  ///   Mythix ORM has no `IN` operator built into the query engine by design. It is expected
  ///   that you will use `EQ` or `NEQ` in combination with an array to get `IN` and `NOT IN`
  ///   functionality.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check against for equality. If an array is provided, then this operator will
  ///     turn into an `IN` operator instead. `null`, `true`, and `false` values are treated separately.
  ///   options?: object
  ///     Options to supply to the operation. This is not used by Mythix ORM, but is provided should
  ///     the user or underlying database driver need options.
  ///
  /// Return: <see>ModelScope</see>
  ///   Return a <see>ModelScope</see> to allow the user to continue chaining operations on the query.
  EQ = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'EQ', 'NEQ', value, options);
  });

  /// "not equals" condition.
  ///
  /// This will check inverse (not) equality with the value(s) provided.
  ///
  /// If `null`, `true`, or `false` are provided as a value,
  /// then the underlying database driver will convert it to
  /// the proper query. For example, given a SQL-type database
  /// driver, these values would be generated in the query as
  /// `"table"."column" IS NOT NULL`, `"table"."column" IS NOT TRUE`, or `"table"."column" IS NOT FALSE`.
  ///
  /// If an array is provided as the value, then this stops being a "not equals",
  /// and instead will turn into a `NOT IN` operator (in SQL-type databases).
  /// For example, given the following query: `User.where.id.NEQ([ 1, 2, 3, 4 ])`,
  /// the resulting SQL query would be `... WHERE "users"."id" NOT IN(1,2,3,4)`. If a `null`,
  /// `true`, or `false` value is encountered in the array, then the condition
  /// will be grouped, and these values will be split out and handled separately.
  /// For example: `User.where.id.NEQ([ 1, 2, 3, null, true, false ])` would result
  /// in the following SQL: `("users"."id" IS NOT NULL AND "users"."id" IS NOT TRUE AND "users"."id" IS NOT FALSE AND "users"."id" NOT IN(1,2,3))`.
  /// If the array provided is empty, than an exception will be thrown. This is for safety reasons.
  /// For example, if one were to do `User.where.id.NEQ([]).destroy()`, then the result
  /// would be `DELETE FROM USERS`... truncating your entire table. This is obviously not
  /// at all ideal... so Mythix ORM will throw an exception if an empty array is encountered,
  /// requiring that the user be explicit.
  ///
  /// This condition also has two other variants, `NEQ.ALL`, and `NEQ.ANY` to check
  /// equality with a sub-query. For example, a `User.where.firstName.NEQ.ANY(User.where.age.GTE(18).PROJECT('firstName'))`
  /// would generate the following SQL query: `... WHERE "users"."firstName" != ANY(SELECT "users"."firstName" FROM "users" WHERE "users"."age" >= 18)`,
  /// checking if the users first name does not match any of the adult user names in the database.
  /// `ALL` is identical in how it functions, except it uses the `ALL` SQL operator instead of the `ANY` operator.
  /// `ANY` will check to see if any of the resulting rows do not match, `ALL` (for this conditional only) will also check
  /// if any of the resulting rows of the sub-query do not match. `ALL` behaves differently than `ANY` for `GT`, `GTE`, `LT`, or `LTE` operators.
  /// For `EQ` and `NEQ` operators, `ANY` and `ALL` behave identically.
  ///
  /// If a sub-query is provided as a value, then one of two things will happen:
  /// 1) If the sub-query has no conditions, then a table-join operation will be the result.
  /// 2) If the sub-query has conditions, then a sub-query will be executed using an `IN` operator.
  /// For example, given the following query: `User.where.id.NEQ(User.where.firstName.EQ('Bob').PROJECT('id'))`,
  /// the sub-query **does have conditions** (`EQ('Bob')`), so the following SQL query that would be generated
  /// would be `... WHERE "users"."id" NOT IN (SELECT "users"."id" FROM "users" WHERE "users"."firstName" = 'Bob')`.
  /// If instead a sub-query is provided as a value that **does not have conditions**, such as
  /// `User.where.id.NEQ(Role.where.userID)`, then this is specifying a table join, joining on the `"roles"` table
  /// where `"users"."id" != "roles"."userID"`.
  ///
  /// Note:
  ///   Mythix ORM has no `IN` operator built into the query engine by design. It is expected
  ///   that you will use `EQ` or `NEQ` in combination with an array to get `IN` and `NOT IN`
  ///   functionality.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check against for inverse (not) equality. If an array is provided, then this operator will
  ///     turn into an `NOT IN` operator instead. `null`, `true`, and `false` values are treated separately.
  ///   options?: object
  ///     Options to supply to the operation. This is not used by Mythix ORM, but is provided should
  ///     the user or underlying database driver need options.
  ///
  /// Return: <see>ModelScope</see>
  ///   Return a <see>ModelScope</see> to allow the user to continue chaining operations on the query.
  NEQ = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'NEQ', 'EQ', value, options);
  });

  /// "greater than" condition.
  ///
  /// This will check if values in the database are greater than the value provided.
  ///
  /// This condition also has two other variants, `GT.ALL`, and `GT.ANY` to compare
  /// with a sub-query. For example, a `User.where.age.GT.ANY(User.where.age.GTE(18).PROJECT('age'))`
  /// would generate the following SQL query: `... WHERE "users"."age" > ANY(SELECT "users"."age" FROM "users" WHERE "users"."age" >= 18)`,
  /// checking if the users age is greater than any of the adult aged users in the database.
  /// `ALL` is identical in how it functions, except it uses the `ALL` SQL operator instead of the `ANY` operator.
  /// `ANY` will check to see if any of the resulting rows of the sub-query match the condition. `ALL` will compare all returned rows
  /// from the sub-query, and check to see if the value provided is greater than the *largest value* across all
  /// rows of the sub-query.
  ///
  /// If a sub-query is provided as a value, then one of two things will happen:
  /// 1) If the sub-query has no conditions, then a table-join operation will be the result.
  /// 2) If the sub-query has conditions, then any exception will be thrown.
  /// For example, if a sub-query is provided as a value that **does not have conditions**, such as
  /// `User.where.createdAt.GT(Role.where.createdAt)`, then this is specifying a table join, joining on the `"roles"` table
  /// where `"users"."createdAt" > "roles"."createdAt"`.
  ///
  /// Note:
  ///   If you want to compare "greater than" on a sub-query, use `GT.ANY`, or `GT.ALL`.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to compare against for a "greater than" operation. Providing an array, a `null`, a `true`, or
  ///     a `false` value will generally throw an exception... unless this means something to the underlying database driver.
  ///   options?: object
  ///     Options to supply to the operation. This is not used by Mythix ORM, but is provided should
  ///     the user or underlying database driver need options.
  ///
  /// Return: <see>ModelScope</see>
  ///   Return a <see>ModelScope</see> to allow the user to continue chaining operations on the query.
  GT = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'GT', 'LTE', value, options);
  });

  /// "greater than or equals to" condition.
  ///
  /// This will check if values in the database are greater than or equal to the value provided.
  ///
  /// This condition also has two other variants, `GTE.ALL`, and `GTE.ANY` to compare
  /// with a sub-query. For example, a `User.where.age.GTE.ANY(User.where.age.GTE(18).PROJECT('age'))`
  /// would generate the following SQL query: `... WHERE "users"."age" >= ANY(SELECT "users"."age" FROM "users" WHERE "users"."age" >= 18)`,
  /// checking if the users age is greater than or equal to any of the adult aged users in the database.
  /// `ALL` is identical in how it functions, except it uses the `ALL` SQL operator instead of the `ANY` operator.
  /// `ANY` will check to see if any of the resulting rows of the sub-query match the condition. `ALL` will compare all returned rows
  /// from the sub-query, and check to see if the value provided is greater than or equal to the *largest value* across all
  /// rows of the sub-query.
  ///
  /// If a sub-query is provided as a value, then one of two things will happen:
  /// 1) If the sub-query has no conditions, then a table-join operation will be the result.
  /// 2) If the sub-query has conditions, then any exception will be thrown.
  /// For example, if a sub-query is provided as a value that **does not have conditions**, such as
  /// `User.where.createdAt.GTE(Role.where.createdAt)`, then this is specifying a table join, joining on the `"roles"` table
  /// where `"users"."createdAt" >= "roles"."createdAt"`.
  ///
  /// Note:
  ///   If you want to compare "greater than or equal to" on a sub-query, use `GTE.ANY`, or `GTE.ALL`.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to compare against for a "greater than or equal to" operation. Providing an array, a `null`, a `true`, or
  ///     a `false` value will generally throw an exception... unless this means something to the underlying database driver.
  ///   options?: object
  ///     Options to supply to the operation. This is not used by Mythix ORM, but is provided should
  ///     the user or underlying database driver need options.
  ///
  /// Return: <see>ModelScope</see>
  ///   Return a <see>ModelScope</see> to allow the user to continue chaining operations on the query.
  GTE = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'GTE', 'LT', value, options);
  });

  /// "less than" condition.
  ///
  /// This will check if values in the database are less than the value provided.
  ///
  /// This condition also has two other variants, `LT.ALL`, and `LT.ANY` to compare
  /// with a sub-query. For example, a `User.where.age.LT.ANY(User.where.age.GTE(18).PROJECT('age'))`
  /// would generate the following SQL query: `... WHERE "users"."age" < ANY(SELECT "users"."age" FROM "users" WHERE "users"."age" >= 18)`,
  /// checking if the users age is less than any of the adult aged users in the database.
  /// `ALL` is identical in how it functions, except it uses the `ALL` SQL operator instead of the `ANY` operator.
  /// `ANY` will check to see if any of the resulting rows of the sub-query match the condition. `ALL` will compare all returned rows
  /// from the sub-query, and check to see if the value provided is less than the *smallest value* across all
  /// rows of the sub-query.
  ///
  /// If a sub-query is provided as a value, then one of two things will happen:
  /// 1) If the sub-query has no conditions, then a table-join operation will be the result.
  /// 2) If the sub-query has conditions, then any exception will be thrown.
  /// For example, if a sub-query is provided as a value that **does not have conditions**, such as
  /// `User.where.createdAt.LT(Role.where.createdAt)`, then this is specifying a table join, joining on the `"roles"` table
  /// where `"users"."createdAt" < "roles"."createdAt"`.
  ///
  /// Note:
  ///   If you want to compare "less than" on a sub-query, use `LT.ANY`, or `LT.ALL`.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to compare against for a "less than" operation. Providing an array, a `null`, a `true`, or
  ///     a `false` value will generally throw an exception... unless this means something to the underlying database driver.
  ///   options?: object
  ///     Options to supply to the operation. This is not used by Mythix ORM, but is provided should
  ///     the user or underlying database driver need options.
  ///
  /// Return: <see>ModelScope</see>
  ///   Return a <see>ModelScope</see> to allow the user to continue chaining operations on the query.
  LT = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'LT', 'GTE', value, options);
  });

  /// "less than or equals to" condition.
  ///
  /// This will check if values in the database are less than or equal to the value provided.
  ///
  /// This condition also has two other variants, `LTE.ALL`, and `LTE.ANY` to compare
  /// with a sub-query. For example, a `User.where.age.LTE.ANY(User.where.age.GTE(18).PROJECT('age'))`
  /// would generate the following SQL query: `... WHERE "users"."age" <= ANY(SELECT "users"."age" FROM "users" WHERE "users"."age" >= 18)`,
  /// checking if the users age is less than or equal to any of the adult aged users in the database.
  /// `ALL` is identical in how it functions, except it uses the `ALL` SQL operator instead of the `ANY` operator.
  /// `ANY` will check to see if any of the resulting rows of the sub-query match the condition. `ALL` will compare all returned rows
  /// from the sub-query, and check to see if the value provided is less than or equal to the *smallest value* across all
  /// rows of the sub-query.
  ///
  /// If a sub-query is provided as a value, then one of two things will happen:
  /// 1) If the sub-query has no conditions, then a table-join operation will be the result.
  /// 2) If the sub-query has conditions, then any exception will be thrown.
  /// For example, if a sub-query is provided as a value that **does not have conditions**, such as
  /// `User.where.createdAt.LTE(Role.where.createdAt)`, then this is specifying a table join, joining on the `"roles"` table
  /// where `"users"."createdAt" <= "roles"."createdAt"`.
  ///
  /// Note:
  ///   If you want to compare "less than or equal to" on a sub-query, use `LTE.ANY`, or `LTE.ALL`.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to compare against for a "less than or equal to" operation. Providing an array, a `null`, a `true`, or
  ///     a `false` value will generally throw an exception... unless this means something to the underlying database driver.
  ///   options?: object
  ///     Options to supply to the operation. This is not used by Mythix ORM, but is provided should
  ///     the user or underlying database driver need options.
  ///
  /// Return: <see>ModelScope</see>
  ///   Return a <see>ModelScope</see> to allow the user to continue chaining operations on the query.
  LTE = wrapAnyAll.call(this, (value, options) => {
    return addOperatorToQuery.call(this, 'LTE', 'GT', value, options);
  });

  /// A "like" wildcard condition.
  ///
  /// This will check if values in the database are "like" the value provided, using wildcards and pattern matching.
  ///
  /// The `LIKE` operator in Mythix ORM follows PostgreSQL design, using `%` for a "zero or more" match, and an `_`
  /// for a "single character" match. All database engines are required to follow this pattern, even if the underlying
  /// database uses a different syntax. If that is the case, then the underlying database driver is required to behave
  /// correctly by translating the value to the input it expects. In short, the interface here in Mythix ORM is unified,
  /// and it is up to the underlying database driver to carry out the operation correctly.
  ///
  /// The "escape character" for this operation is always set to a backslash `\` for all database drivers. So if you
  /// need to match against a literal `%` or `_` character, you would do so by using the escape character, i.e. `LIKE('100\\%')`.
  /// Two escape characters are needed, because Javascript also interprets `\` as an escape character, so a double backslash
  /// `'\\'` will "escape" to a single backslash in Javascript.
  ///
  /// For all databases, the Mythix ORM `LIKE` is case-insensitive. For databases like PostgreSQL, a standard Mythix ORM `LIKE` operation
  /// is actually carried out as an `ILIKE` operation to follow this convention of case-insensitivity. There is
  /// a boolean `options` that can be supplied to this operator, named `caseSensitive`. If supplied, for example
  /// `LIKE('%something%', { caseSensitive: true })`, then the operation will be case-sensitive... but only if the underlying database
  /// supports case-sensitivity for `LIKE`... and not all databases do. `LIKE` operations in most SQL databases are case-insensitive by default.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to compare against for a "LIKE" operation. Providing any non-string value will
  ///     likely throw an exception... unless the underlying database driver supports other pattern
  ///     formats (i.e. `RegExp` is supported in Mongo).
  ///   options?: object
  ///     Options to supply to the operation. The only option supported for this operator is the boolean
  ///     option `caseSensitive`. If `true`, then the database driver will attempt to carry out a case-sensitive
  ///     operation. If not supported by the database, then the database driver should throw an exception.
  ///     If a `RegExp` value is used (for databases that support it), then this option will be ignored.
  ///
  /// Return: <see>ModelScope</see>
  ///   Return a <see>ModelScope</see> to allow the user to continue chaining operations on the query.
  LIKE(value, options) {
    let caseSensitive = ((options && options.caseSensitive) === true);
    return addOperatorToQuery.call(this, 'LIKE', 'NOT_LIKE', value, { caseSensitive });
  }

  /// A "not like" wildcard condition.
  ///
  /// This will check if values in the database are "not like" the value provided, using wildcards and pattern matching.
  ///
  /// The `LIKE` operator in Mythix ORM follows PostgreSQL design, using `%` for a "zero or more" match, and an `_`
  /// for a "single character" match. All database engines are required to follow this pattern, even if the underlying
  /// database uses a different syntax. If that is the case, then the underlying database driver is required to behave
  /// correctly by translating the value to the input it expects. In short, the interface here in Mythix ORM is unified,
  /// and it is up to the underlying database driver to carry out the operation correctly.
  ///
  /// The "escape character" for this operation is always set to a backslash `\` for all database drivers. So if you
  /// need to match against a literal `%` or `_` character, you would do so by using the escape character, i.e. `NOT LIKE('100\\%')`.
  /// Two escape characters are needed, because Javascript also interprets `\` as an escape character, so a double backslash
  /// `'\\'` will "escape" to a single backslash in Javascript.
  ///
  /// For all databases, the Mythix ORM `NOT_LIKE` is case-insensitive. For databases like PostgreSQL, a standard Mythix ORM `NOT_LIKE` operation
  /// is actually carried out as an `NOT ILIKE` operation to follow this convention of case-insensitivity. There is
  /// a boolean `options` that can be supplied to this operator, named `caseSensitive`. If supplied, for example
  /// `NOT_LIKE('%something%', { caseSensitive: true })`, then the operation will be case-sensitive... but only if the underlying database
  /// supports case-sensitivity for `NOT LIKE`... and not all databases do. `NOT LIKE` operations in most SQL databases are case-insensitive by default.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to compare against for a "NOT LIKE" operation. Providing any non-string value will
  ///     likely throw an exception... unless the underlying database driver supports other pattern
  ///     formats (i.e. `RegExp` is supported in Mongo).
  ///   options?: object
  ///     Options to supply to the operation. The only option supported for this operator is the boolean
  ///     option `caseSensitive`. If `true`, then the database driver will attempt to carry out a case-sensitive
  ///     operation. If not supported by the database, then the database driver should throw an exception.
  ///     If a `RegExp` value is used (for databases that support it), then this option will be ignored.
  ///
  /// Return: <see>ModelScope</see>
  ///   Return a <see>ModelScope</see> to allow the user to continue chaining operations on the query.
  NOT_LIKE(value, options) {
    let caseSensitive = ((options && options.caseSensitive) === true);
    return addOperatorToQuery.call(this, 'NOT_LIKE', 'LIKE', value, { caseSensitive });
  }

  [ProxyClass.MISSING](target, prop) {
    let lowerScope = this._fetchScope('model');
    return lowerScope[prop];
  }

  toString(...args) {
    return this.getOperationContext().queryEngineScope.toString(...args);
  }
}

module.exports = FieldScope;
