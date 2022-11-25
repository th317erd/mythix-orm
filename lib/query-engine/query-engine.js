'use strict';

const Nife            = require('nife');
const ProxyClass      = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');
const ModelScope      = require('./model-scope');
const FieldScope      = require('./field-scope');
const Utils           = require('../utils');

/// QueryEngine is the "root level" of a query.
/// It manages things like `MERGE`, `unscoped`,
/// and all the database interfaces, such as `all`,
/// `first`, `pluck`, etc...
///
/// Being a Proxy, it will "listen" for key access,
/// and lookup models if there is a key access where
/// the key name starts with an upper-case letter. In this
/// case, it will check the `connection` it owns to see
/// if such a model (by name) is registered with the
/// connection. If there is, then it will use the model
/// found to push a <see>ModelScope</see> onto the "operation stack",
/// and then return that to the user. Take the following example:
/// ```javascript
/// let queryRoot = new QueryEngine({ connection });
/// let userModelScope = queryRoot.User;
/// ```
/// When we attempt to access the key `'User'` on
/// the QueryEngine scope, we will find that no such key
/// exists on the class. Now that no such property is found
/// on the class, the <see>ProxyClass</see> with call the method
/// `MISSING` on the `QueryEngine`, and this `MISSING` method
/// will check the `connection` to see if there is a model
/// named `'User'`. The `connection` finds this model, and
/// returns it. The `QueryEngine` then takes this model class,
/// and uses it to create and return a <see>ModelScope</see>
/// using <see>QueryEngineBase._newModelScope</see>. Now
/// that we have a <see>ModelScope</see>, we can continue
/// chaining by looking up a field `queryRoot.User.id`, and
/// the process repeats on `ModelScope`, but this time looking
/// for a field from the `connection` instead of a model. The
/// model owning the field is already known, because we already
/// have a `Model` class on our "operation stack" for the query.
/// So the field `id` is looked up on the `User` model, and if
/// found, returned as a <see>FieldScope</see> using <see>QueryEngineBase._newFieldScope</see>.
///
/// See: QueryEngineBase
///
/// See: ModelScope
///
/// See: FieldScope
class QueryEngine extends QueryEngineBase {
  /// Get the `ModelScope` class for the
  /// query engine.
  ///
  /// See: QueryEngineBase.getModelScopeClass
  getModelScopeClass() {
    return ModelScope;
  }

  /// Get the `FieldScope` class for the
  /// query engine.
  ///
  /// See: QueryEngineBase.getFieldScopeClass
  getFieldScopeClass() {
    return FieldScope;
  }

  /// Construct a new QueryEngine instance.
  ///
  /// Arguments:
  ///   context: object
  ///     The "root" "operation context" for the query. This is required to contain a
  ///     `connection` property, with a valid <see>Connection</see> instance as a value. Besides that, you can apply
  ///     any properties you want here, and they will be available on all "operation contexts".
  constructor(_context) {
    let context = Object.assign(
      Object.create(_context || {}),
      {
        currentScopeName:        'queryEngine',
        isQueryOperationContext: true,
        contextID:               QueryEngineBase.generateID(),
      },
    );

    super(context);

    context.queryEngineScope = this;
  }

  /// This method will directly fetch a model by
  /// name and return a <see>ModelScope</see>.
  ///
  /// This can be useful when you have a name collision,
  /// or when you just want to go the direct route to get
  /// a model operation in place.
  ///
  /// Arguments:
  ///   modelName: string
  ///     The model name to fetch and use for the `ModelScope`.
  ///
  /// Return: <see>ModelScope</see>
  ///   A new `ModelScope` using the model found by the name provided.
  Model(modelName) {
    let model = this.getModel(modelName);
    if (!model)
      throw new Error(`QueryEngine::Model: Requested model "${modelName}" not found.`);

    return this._newModelScope(model);
  }

  /// This method will reset the query "operation stack"
  /// back to the root context--and, if possible, to the root
  /// model scope and root model projection.
  /// All other operations will be wiped, including
  /// the any <see>Model.defaultScope</see> that has been
  /// applied. One would generally call this as the very
  /// first thing on an query to ensure it has been reset
  /// back to its primary parts *before* you would
  /// apply any other operations. For example: `let users = await User.where.unscoped().lastName.EQ('Smith').all();`,
  /// would "ignore" any <see>Model.defaultScope</see> applied to
  /// the query, including any default `ORDER` that `defaultScope`
  /// applied. `User.where.lastName.EQ('Smith').unscoped()` would
  /// be equivalent to `User.where` (assuming no <see>Model.defaultScope</see>
  /// is in-play).
  ///
  /// Return: <see>QueryEngine</see>
  ///   A new query, reset back to only its root context, or the root model
  ///   scope (if there is a root model available on the query). If there
  ///   is a root model available, then its projection will also be applied
  ///   as usual.
  unscoped() {
    let context           = this.getOperationContext();
    let QueryEngineClass  = this.constructor;
    let queryEngine       = new QueryEngineClass({
      connection: this.getConnection(),
    });

    if (context.rootModelName)
      queryEngine = queryEngine[context.rootModelName];

    return queryEngine;
  }

  /// Stringify this query, using the underlying database
  /// connection. This is similar to a `toSQL` method in other ORMs.
  /// It isn't exactly that though... because for non-SQL
  /// database drivers, it might stringify the query into
  /// something that is non-SQL. By default the query will
  /// be turned into a string that represents a `SELECT`
  /// statement (or other type of fetch statement) for the
  /// underlying database driver.
  ///
  /// Arguments:
  ///   ...args?: Array<any>
  ///     Any arguments you want to pass off to the underlying
  ///     <see>QueryGenerator.toConnectionString</see> method that
  ///     is called to stringify the query.
  ///
  /// Return: string
  ///   The query, turned into a "fetch" representation for the underlying
  ///   database driver. For SQL-based databases this would be a `SELECT` statement.
  toString(...args) {
    let connection      = this.getConnection();
    let queryGenerator  = connection.getQueryGenerator();

    return queryGenerator.toConnectionString(this, ...args);
  }

  /// Merge one query onto another one.
  ///
  /// This method will merge two queries together
  /// by "replaying" the provided one onto `this`
  /// one. For example:
  /// ```javascript
  /// let userQuery = User.where.id.EQ(1);
  /// let roleQuery = Role.where.userID.EQ(User.where.id).name.EQ('admin');
  /// let finalQuery = userQuery.AND.MERGE(roleQuery);
  /// // finalQuery == User.where.id.EQ(1).AND.Role.where.userID.EQ(User.where.id).name.EQ('admin');
  /// ```
  ///
  /// This is almost like a "concatenate" operation, but there are a few notable differences:
  /// 1) The first "logical" operator encountered is always skipped
  /// 2) `PROJECT` operations are skipped by default (but can be merged with the use of `options`)
  /// 3) Any `GROUP_BY`, `ORDER`, or `PROJECT` are *merged* together, instead of replacing the previous operation.
  ///
  /// The first logical operator is skipped for a good reason. It is to allow one to `OR` merge
  /// the query, or `AND` merge it instead. If we were to simply "concatenate" the stacks together,
  /// then we might have contexts that look like (pseudo/concept code):
  /// `[ User, AND, id, EQ(1) ].OR.MERGE([ Role, AND, userID, EQ(...), ... ])`.
  /// The problem here should be obvious: The `OR` is (nearly) immediately followed by an `AND` in
  /// the query we are merging with. If we didn't skip the first logical operator, then this would
  /// nearly always be the case, especially since the `QueryEngine` turns on the `AND` operator
  /// by default for all new queries. This is why we skip the first logical operator, so that we
  /// are able to `.AND.MERGE(...)`, or `.OR.MERGE(...)` a query together... a distinction which
  /// sometimes can be vitally important.
  ///
  /// `PROJECT` operations are skipped by default unless you specify the `options` of `{ projections: true }`.
  /// This is simply because it often isn't desired to have projections merge over from sub-queries that
  /// are being merged in... causing extra data to be pulled from the database. Projections in Mythix ORM
  /// should **always** be deliberate and direct.
  /// On the other hand, `ORDER` and `GROUP_BY` operations are automatically merged for you by default. The
  /// reason should be fairly obvious, if two parts of the query need a specific order, or group-by fields,
  /// then they probably shouldn't be skipped. They can always be skipped if desired by passing the `options`
  /// of `{ orders: false }`, or `{ groupBys: false }` (or both).
  ///
  /// Note:
  ///   Because merges (and other operations) can get quite complex, it is recommend that you always
  ///   apply a projection, order, limit, and offset immediately before you use your query to interact
  ///   with the database.
  ///
  /// Arguments:
  ///   incomingQuery: <see>QueryEngine</see>
  ///     The query to merge/replay on-top a clone of `this` query.
  ///   options?: object
  ///     Options for the operation. See the table below for a list of possible options:
  ///     | Option | Type | Default Value | Description |
  ///     | ------------- | ---- | ------------- | ----------- |
  ///     | `projections` | `boolean` | `false` | If `true`, then also merge projections together. |
  ///     | `orders` | `boolean` | `true` | If `true`, then also merge order clauses together. |
  ///     | `groupBys` | `boolean` | `true` | If `true`, then also merge group-by clauses together. |
  ///     | `connection` | `Connection` | `this.getConnection()` | The connection to supply to the newly created `QueryEngine`. If none is supplied, then `this.getConnection()` is used to retrieve the connection. |
  ///
  /// Return: <see>QueryEngine</see>
  ///   Return the newly merged query.
  MERGE(_incomingQueryEngine, _options) {
    let incomingQueryEngine = _incomingQueryEngine;
    if (!incomingQueryEngine)
      return this;

    let options           = _options || {};
    let thisQueryContext  = this.getOperationContext();
    if (!QueryEngine.isQuery(incomingQueryEngine) && Nife.instanceOf(incomingQueryEngine, 'array', 'object', 'map', 'set'))
      incomingQueryEngine = Utils.generateQueryFromFilter(this.getConnection(options.connection), thisQueryContext.rootModel, incomingQueryEngine);

    let incomingOperationStack      = incomingQueryEngine.getOperationStack();
    let skippingLogical             = true;
    let logicalOperatorEncountered  = false;
    let queryEngine                 = this.clone();

    for (let i = 0, il = incomingOperationStack.length; i < il; i++) {
      let queryPart = incomingOperationStack[i];

      // For merges, we want to skip the first logical operators
      // found before any other operation.
      // This is because one might do a: Model.where.OR.MERGE(Model.AND.field.EQ()).
      // This query, once merged, would be: Model.where.OR.Model.AND.field.EQ(),
      // which is not what we want... instead we want: Model.where.OR.Model.field.EQ().
      // Since the result we want here is OR merge, not AND merge
      // we skip the first "AND" we encounter, leaving the "OR" as
      // the current logical operator.
      // Logical operators do not always come first in the query,
      // so we need to rely on "logicalOperatorEncountered" to
      // ensure we skip only the first one (or first few in a sequence).
      if (skippingLogical && Object.prototype.hasOwnProperty.call(queryPart, 'logical') && queryPart.logical) {
        if (queryPart.value == null && (queryPart.operator === 'AND' || queryPart.operator === 'OR')) {
          logicalOperatorEncountered = true;
          continue;
        }
      }

      if (logicalOperatorEncountered)
        skippingLogical = false;

      let value = queryPart.value;
      if (Object.prototype.hasOwnProperty.call(queryPart, 'control') && queryPart.control === true) {
        if (queryPart.operator === 'PROJECT' && options.projections !== true)
          continue;

        if (queryPart.operator === 'ORDER' && options.orders === false)
          continue;

        if (queryPart.operator === 'GROUP_BY' && options.groupBys === false)
          continue;

        if (queryPart.operator === 'PROJECT' || queryPart.operator === 'ORDER' || queryPart.operator === 'GROUP_BY')
          value = [ '+' ].concat(value);
      }

      queryEngine = queryEngine[queryPart.queryProp](value, ...(queryPart.queryExtraArgs || []));
    }

    return queryEngine;
  }

  /// Fetch all rows matching the query from the database.
  ///
  /// By default all rows fetched will be converted into
  /// <see>Model</see> instances.
  ///
  /// This method will fetch in batches. By default the batch
  /// size is `500`, but can be modified with the `{ batchSize: number; }`
  /// `options`. Even though it fetches from the database in batches,
  /// it won't return until all data has been fetched from the database.
  ///
  /// Note:
  ///   To stream from the database instead, use the
  ///   <see>QueryEngine.cursor</see> method.
  ///
  /// Arguments:
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation. This specific operation also has a `batchSize: number;`
  ///     option that can be used to specify the batch size for fetching.
  ///
  /// Return: Array<Model>
  ///   An array of all matching root model instances found by the query.
  async all(options) {
    let connection = this.getConnection(options && options.connection);

    if (options && options.stream)
      throw new TypeError('QueryEngine::all: "stream" option set to true. For streaming, please use the "cursor" method instead.');

    return await Utils.collect(connection.select(await this.finalizeQuery('read', options), options));
  }

  /// Fetch all rows matching the query from the database,
  /// by streaming them, using an async generator.
  ///
  /// By default all rows fetched will be converted into
  /// <see>Model</see> instances.
  ///
  /// This method will fetch in batches. By default the batch
  /// size is `500`, but can be modified with the `{ batchSize: number; }`
  /// `options`. All data being fetched will be "streamed" to the caller
  /// via an async generator return value.
  ///
  /// Note:
  ///   To collect all results at once instead of streaming,
  ///   use the <see>QueryEngine.all</see> method instead.
  ///
  /// Arguments:
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation. This specific operation also has a `batchSize: number;`
  ///     option that can be used to specify the batch size for fetching.
  ///
  /// Return: Array<Model>
  ///   An array of all matching root model instances found by the query.
  async *cursor(_options) {
    let options     = _options || {};
    let connection  = this.getConnection(options && options.connection);
    return yield *connection.select(await this.finalizeQuery('read', options), { ...options, stream: true });
  }

  /// Fetch the first matching row(s) from the database.
  ///
  /// By default all rows fetched will be converted into
  /// <see>Model</see> instances.
  ///
  /// This method will always apply a `LIMIT` of `limit`, but it
  /// won't modify the `OFFSET`... so it is possible to get
  /// the nth item(s) by supplying your own `OFFSET` on the query.
  ///
  /// Note:
  ///   An `ORDER` should be applied on the query before
  ///   calling this method... or you might get funky results.
  ///
  /// Arguments:
  ///   limit: number = 1
  ///     The number of rows to fetch from the database. The default is `1`.
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation.
  ///
  /// Return: Array<Model> | Model
  ///   Return just one `Model` if the `limit` is set to `1`, otherwise
  ///   return an `Array` of `Model`.
  async first(_limit, options) {
    let limit       = (_limit == null) ? 1 : _limit;
    let connection  = this.getConnection(options && options.connection);
    let query       = (await this.finalizeQuery('read', options)).clone().LIMIT(limit);

    let result = await Utils.collect(connection.select(query, options));
    return (_limit == null) ? result[0] : result;
  }

  /// Fetch the last matching row(s) from the database.
  ///
  /// By default all rows fetched will be converted into
  /// <see>Model</see> instances.
  ///
  /// This method will always apply a `LIMIT` of `limit`, but it
  /// won't modify the `OFFSET`... so it is possible to get
  /// the nth item(s) by supplying your own `OFFSET` on the query.
  ///
  /// This method works by forcing the query `ORDER` to invert itself,
  /// and then it selects the first `limit` rows from the top.
  ///
  /// Note:
  ///   An `ORDER` should be applied on the query before
  ///   calling this method... or you might get funky results.
  ///
  /// Arguments:
  ///   limit: number = 1
  ///     The number of rows to fetch from the database. The default is `1`.
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation.
  ///
  /// Return: Array<Model> | Model
  ///   Return just one `Model` if the `limit` is set to `1`, otherwise
  ///   return an `Array` of `Model`.
  async last(_limit, options) {
    let limit       = (_limit == null) ? 1 : _limit;
    let connection  = this.getConnection(options && options.connection);
    let query       = (await this.finalizeQuery('read', options)).clone().LIMIT(limit);

    let result = await Utils.collect(connection.select(query, Object.assign({}, options || {}, { reverseOrder: true })));
    return (_limit == null) ? result[0] : result.reverse();
  }

  /// Update all matching rows in the database with
  /// the attributes provided.
  ///
  /// This method will bulk-write to the rows matching
  /// the query, using the attributes provided to update
  /// matching rows. The attributes provided can be a model instance,
  /// but will generally just be an object of attributes
  /// (where the key names must match model `fieldName`s).
  ///
  /// Example:
  ///   // Update all users who are 'inactive' to now be 'active'
  ///   await User.where.status.EQ('inactive').updateAll({ status: 'active' });
  ///
  /// Note:
  ///   **This will not call onBefore* or onAfter* update hooks
  ///   on the model instances**. It is a direct bulk-write
  ///   operation to the underlying database.
  ///
  /// Note:
  ///   This operation will only ever update the "root model"
  ///   table. The "root model" of a query is the first model
  ///   used in the query. For example, `User` is the root model
  ///   in the query `User.where`.
  ///
  /// Arguments:
  ///   attributes: object | <see>Model</see>
  ///     The attributes to update across all rows. The key names of
  ///     this object must be `fieldName`s from the root model of the
  ///     query (**NOT** column names).
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation.
  ///
  /// Return: number
  ///   Return the number of rows that were updated.
  async updateAll(attributes, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.updateAll(await this.finalizeQuery('update', options), attributes, options);
  }

  /// Delete matching rows in the database.
  ///
  /// This method will delete all rows matching the query
  /// from the database (for only the root model).
  ///
  /// Note:
  ///   This operation will only ever update the "root model"
  ///   table. The "root model" of a query is the first model
  ///   used in the query. For example, `User` is the root model
  ///   in the query `User.where`.
  ///
  /// Arguments:
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation.
  ///
  /// Return: number
  ///   Return the number of rows that were updated.
  async destroy(options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.destroy(await this.finalizeQuery('delete', options), options);
  }

  /// Calculate the "average" of all matching rows for
  /// a single column in the database.
  ///
  /// Example:
  ///   let averageUserAge = await User.where.average('User:age');
  ///
  /// Arguments:
  ///   field: <see>Field</see> | string | <see>Literal</see>
  ///     A field, fully qualified field name, or literal to average across.
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation.
  ///
  /// Return: number
  ///   The "average" of all matching rows for a single column.
  async average(field, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.average(await this.finalizeQuery('read', options), field, options);
  }

  /// Count the number of matching rows.
  ///
  /// A `field` can be provided, which is used as the column to count on.
  /// If a `DISTINCT` operation is used in the query, then the `DISTINCT`
  /// field will be used for counting, regardless of what `field`
  /// you specify. If no `field` is provided, then all fields (`*`)
  /// is assumed.
  ///
  /// Note:
  ///   Due to database limitations, it can sometimes be difficult or
  ///   impossible in the underlying database to use an `ORDER`, `LIMIT`, or `OFFSET`,
  ///   with this operation. Because of this, any `ORDER` on the query
  ///   is simply ignored for now (help solving this problem would be
  ///   greatly appreciated!).
  ///
  /// Arguments:
  ///   field?: <see>Field</see> | string | literal
  ///     A field, fully qualified field name, or literal to count on. If
  ///     a `DISTINCT` operation is used in the query, then this argument
  ///     will be ignored, and the field defined on the `DISTINCT` operation
  ///     will be used instead.
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation.
  ///
  /// Return: number
  ///   The number of rows that match the query.
  async count(field, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.count(await this.finalizeQuery('read', options), field, options);
  }

  /// Calculate the "minimum" of all matching rows for
  /// a single column in the database.
  ///
  /// Example:
  ///   let youngestUserAge = await User.where.min('User:age');
  ///
  /// Arguments:
  ///   field: <see>Field</see> | string | <see>Literal</see>
  ///     A field, fully qualified field name, or literal to calculate on.
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation.
  ///
  /// Return: number
  ///   The "minimum" value of all matching rows for a single column.
  async min(field, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.min(await this.finalizeQuery('read', options), field, options);
  }

  /// Calculate the "maximum" of all matching rows for
  /// a single column in the database.
  ///
  /// Example:
  ///   let oldestUserAge = await User.where.max('User:age');
  ///
  /// Arguments:
  ///   field: <see>Field</see> | string | <see>Literal</see>
  ///     A field, fully qualified field name, or literal to calculate on.
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation.
  ///
  /// Return: number
  ///   The "maximum" value of all matching rows for a single column.
  async max(field, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.max(await this.finalizeQuery('read', options), field, options);
  }

  /// Calculate the "sum" of all matching rows for
  /// a single column in the database.
  ///
  /// Example:
  ///   let totalInventoryPrice = await Product.where.sum('Product:price');
  ///
  /// Arguments:
  ///   field: <see>Field</see> | string | <see>Literal</see>
  ///     A field, fully qualified field name, or literal to average across.
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation.
  ///
  /// Return: number
  ///   The "sum" of all matching rows for a single column.
  async sum(field, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.sum(await this.finalizeQuery('read', options), field, options);
  }

  /// Pluck specific fields directly from the database.
  /// These plucked fields will not be converted into
  /// model instances, but instead will be returned raw
  /// to the caller.
  ///
  /// If only a single field is provided for `fields` then
  /// an array of column values is returned, i.e. `[ ... ]`. If
  /// however more than one field is specified for `fields`,
  /// then an array of arrays containing column values is
  /// returned, i.e. `[ [ ... ], [ ... ], ... ]`.
  ///
  /// Arguments:
  ///   fields: <see>Field</see> | string | Array<<see>Field</see>> | Array<string>
  ///     The field(s) to pluck from the database. A single field may be specified.
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation. The option `{ mapToObjects: true }` can be specified to
  ///     map the results to objects, instead of returning arrays of raw values.
  ///
  /// Return: Array<any> | Array<Array<any>>
  ///   If a single field is specified for `fields`, then an array of column values
  ///   will be returned. If more than one field is specified, then an array of arrays
  ///   of column values will be returned. It matters not if you specify the single field
  ///   directly, or as an array, i.e. `.pluck('User:firstName')` is the same as `.pluck([ 'User:firstName' ])`.
  ///   The engine only cares if "one field" is used... it doesn't care how it receives
  ///   that single field.
  async pluck(fields, options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.pluck(await this.finalizeQuery('read', options), fields, options);
  }

  /// Check if any rows match the query.
  ///
  /// Arguments:
  ///   options?: object
  ///     Options to pass into the operation. These are generally connection
  ///     specific. However, one option that is always available is the `connection`
  ///     option, which can define the <see>Connection</see> to use for the
  ///     operation.
  ///
  /// Return: boolean
  ///   Return `true` if at least one row matches the query, or `false` otherwise.
  async exists(options) {
    let connection = this.getConnection(options && options.connection);
    return await connection.exists(await this.finalizeQuery('read', options), options);
  }

  /// Finalize the query before a database operation.
  ///
  /// This method simply proxies the request to <see>Connection.finalizeQuery</see>.
  /// If no connection can be found (for whatever reason), then `this`
  /// (the current query) is simply returned.
  ///
  /// Note:
  ///   This system was designed for "row level permissions" systems,
  ///   or simply to allow models themselves to control how queries are
  ///   carried out against them.
  ///
  /// Arguments:
  ///   type: string
  ///     The CRUD operation type being performed. Can be one of `'create'`, `'read'`,
  ///     `'update'`, or `'delete'`. The `'create'` operation is currently never used
  ///     by Mythix ORM, since no query is ever involved in an `INSERT` operation; however
  ///     it has been reserved for future use.
  ///   options?: object
  ///     The options that were provided by the user to the operation being executed.
  ///     These are the operation options themselves, but can contain properties
  ///     specifically targeted for custom down-stream code.
  ///
  /// Return: <see>QueryEngine</see>
  ///   Return `this` query, possibly altered by down-stream code.
  async finalizeQuery(crudOperation, options) {
    let connection = this.getConnection();
    if (connection && typeof connection.finalizeQuery === 'function')
      return await connection.finalizeQuery(crudOperation, this, options);

    return this;
  }

  [ProxyClass.MISSING](target, prop) {
    if (prop === 'debug') {
      this.currentContext.rootContext.debug = true;
      return this._fetchScope('model', 'queryEngine');
    }

    let model = this.getModel(prop);
    if (model) {
      return this._newModelScope(model).__call(function(...args) {
        let fieldName = args.find((arg) => (arg && arg.constructor && !arg.constructor._isMythixConnection));
        if (!fieldName)
          return this;

        return this.Field(fieldName);
      });
    }

    return this[prop];
  }
}

module.exports = QueryEngine;
