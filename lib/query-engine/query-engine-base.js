'use strict';

const ProxyClass = require('../proxy-class');

let uuid = 1;

function generateID() {
  return uuid++;
}

/// `QueryEngineBase` is the class that all other
/// query engine classes inherit from. It provides
/// "proxy" support for all class instances.
/// `QueryEngine`, `ModelScope`, and `FieldScope` all
/// inherit from this class.
///
/// Note:
///   `QueryEngineBase` is a sub-part of the `QueryEngine`, and so is generally referred to
///   simply as the `QueryEngine` as a whole. This is also the case for <see>ModelScope</see>,
///   and <see>FieldScope</see>, which also make up the `QueryEngine` as sub-parts,
///   and so are also often referred to simply as "the query engine".
class QueryEngineBase extends ProxyClass {
  /// Used to generate unique "operation ids"
  /// for the query operation stack.
  ///
  /// Return: number
  ///   A unique id.
  static generateID() {
    return uuid++;
  }

  /// Check if the provided `value` is an "operation context".
  ///
  /// The query engine works by creating an "operation stack",
  /// that is an array of "operation contexts". Any time an
  /// operation is carried out on the query engine, such as
  /// selecting a new model for example `new QueryEngine({ connection }).Model('User')`
  /// then an "operation" will be pushed onto the operation stack.
  /// Each operation on the stack sets its `prototype` to the previous
  /// operation on the stack using `Object.create`. This means that
  /// all "operation contexts" on the stack also include all attributes
  /// from all previous operations on the stack via the operation
  /// `prototype`.
  ///
  /// Use this method to check if any object is a valid "operation context"
  /// from the query engine.
  ///
  /// Arguments:
  ///   value: object
  ///     The value to check.
  ///
  /// Return: boolean
  ///   Return `true` if the provided `value` is a query engine "operation context",
  ///   or `false` otherwise.
  static isQueryOperationContext(value) {
    return !!(value && value.isQueryOperationContext);
  }

  /// Check to see if the provided value is
  /// an *instance* of a Mythix ORM <see>QueryEngineBase</see>.
  /// It will return `true` if the provided value is an `instanceof`
  /// <see>QueryEngineBase</see>, if the value's `constructor`
  /// property has a truthy `_isMythixQueryEngine` property
  /// (`value.constructor._isMythixQueryEngine`), or if the
  /// provided value has a `getOperationContext` method.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     Value to check
  static isQuery(value) {
    if (!value)
      return false;

    if (value instanceof QueryEngineBase)
      return true;

    if (value._isMythixQueryEngine)
      return true;

    if (typeof value.getOperationContext === 'function')
      return true;

    return false;
  }

  /// Get information about the query.
  ///
  /// This method will return an object
  /// containing certain keys that will
  /// report on the status of the query.
  ///
  /// Interface:
  ///   interface QueryInfo {
  ///     hasDistinct: boolean;
  ///     hasOrder: boolean;
  ///     hasProjection: boolean;
  ///     hasGroupBy: boolean;
  ///     hasHaving: boolean;
  ///     hasCondition: boolean;
  ///     hasTableJoins: boolean;
  ///     hasField: boolean;
  ///     hasModel: boolean;
  ///   }
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query to get information on.
  ///
  /// Return: QueryInfo
  ///   Return information relating to the query.
  static getQueryOperationInfo(query) {
    let contextParams = {
      hasDistinct:   false,
      hasOrder:      false,
      hasProjection: false,
      hasGroupBy:    false,
      hasHaving:     false,
      hasCondition:  false,
      hasTableJoins: false,
      hasField:      false,
      hasModel:      false,
    };

    if (!query)
      return contextParams;

    let queryContext = query.getOperationContext();

    if (queryContext.distinct)
      contextParams.hasDistinct = true;

    if (queryContext.order)
      contextParams.hasOrder = true;

    if (queryContext.projection)
      contextParams.hasProjection = true;

    if (queryContext.groupBy)
      contextParams.hasGroupBy = true;

    if (queryContext.having)
      contextParams.hasHaving = true;

    if (queryContext.condition)
      contextParams.hasCondition = true;

    if (queryContext.fieldName)
      contextParams.hasField = true;

    if (queryContext.Model)
      contextParams.hasModel = true;

    contextParams.hasTableJoins = query.queryHasJoins();

    return contextParams;
  }

  /// Get the `ModelScope` class that should be used
  /// for the query. By default this will be <see>ModelScope</see>.
  ///
  /// This is provided so that the end-user can overload the
  /// query engine, adding custom functionality. You could for
  /// example inherit from the default <see>ModelScope</see> class
  /// and add extra functionality to the query engine. To do so,
  /// you would overload this method on your own custom implementation
  /// of `QueryEngine`, and would return your own custom `ModelScope`
  /// class for use in the query engine.
  ///
  /// A "model scope" is a sub-scope of the `QueryEngine` that defines
  /// "model level operations". A model scope is returned by the query
  /// engine as soon as a model is accessed, i.e. `User.where` would
  /// return a `ModelScope` relating to the `User` model. A conditional
  /// operation, such as `EQ` will also return a `ModelScope`... ready
  /// for the next field to be specified. Scopes are used in the Mythix ORM
  /// query engine so that you can't do anything funky... such as `User.where.EQ('value')`...
  /// how can a model be equal to a value? It can't... so scopes are used
  /// in the engine to ensure you can't do anything silly. This scope defines
  /// all operations that can be applied to models.
  ///
  /// Return: class inherits <see>ModelScope</see>
  ///   The class to use for "model scopes".
  getModelScopeClass() {
    return this.getQueryEngineScope().getModelScopeClass();
  }

  /// Get the `FieldScope` class that should be used
  /// for the query. By default this will be <see>FieldScope</see>.
  ///
  /// This is provided so that the end-user can overload the
  /// query engine, adding custom functionality. You could for
  /// example inherit from the default <see>FieldScope</see> class
  /// and add extra functionality to the query engine. To do so,
  /// you would overload this method on your own custom implementation
  /// of `QueryEngine`, and would return your own custom `FieldScope`
  /// class for use in the query engine.
  ///
  /// A "field scope" is a sub-scope of the `QueryEngine` that defines
  /// "field level operations". A field scope is returned by the query
  /// engine as soon as a field is accessed, i.e. `User.where.id` would
  /// return a `FieldScope` relating to the `User` model (the `id` field).
  /// Scopes are used in the Mythix ORM query engine so that you can't do
  /// anything funky... such as `User.where.EQ('value')`...
  /// how can a model be equal to a value? It can't... so scopes are used
  /// in the engine to ensure you can't do anything silly. This scope defines
  /// all operations that can be applied to model fields.
  ///
  /// Return: class inherits <see>FieldScope</see>
  ///   The class to use for "field scopes".
  getFieldScopeClass() {
    return this.getQueryEngineScope().getFieldScopeClass();
  }

  /// Get the `QueryEngine` class that should be used
  /// for the query. By default this will be <see>QueryEngine</see>.
  ///
  /// This is provided so that the end-user can overload the
  /// query engine, adding custom functionality. You could for
  /// example inherit from the default <see>QueryEngine</see> class
  /// and add extra functionality to the query engine. To do so,
  /// you would overload this method on your own custom implementation
  /// of `QueryEngine`, and would return your own custom `QueryEngine`
  /// class for use in the query engine.
  ///
  /// A "query scope" is the "root scope" of a `QueryEngine` that defines
  /// "root level operations". A root scope is returned by the query
  /// engine as soon as the query is created, i.e. `new QueryEngine({ connection })` would
  /// return a `QueryEngine`. Scopes are used in the Mythix ORM query engine so that
  /// you can't do anything funky... such as `User.where.EQ('value')`...
  /// how can a model be equal to a value? It can't... so scopes are used
  /// in the engine to ensure you can't do anything silly. This scope defines
  /// all operations that can be applied to the "root query", such as fetching
  /// the operation stack, the operation context, mapping or filtering the query, etc...
  ///
  /// Return: class inherits <see>QueryEngine</see>
  ///   The class to use for "root scopes". By default this method
  ///   will return the class used to initially create the query engine.
  ///   So for example if you create the query with your own custom class,
  ///   such as `new MyCustomQueryEngine({ connection })`, then this will
  ///   automatically return your class for you (`MyCustomQueryEngine` in this example).
  getQueryEngineScopeClass() {
    return this.getQueryEngineScope().constructor;
  }

  /// This "stacks" an incoming "operation context" on top
  /// the previous context in the "operation stack". It will
  /// also reset the `value` of the context to `undefined`, and
  /// will generate a new `contextID` for this new context.
  ///
  /// Note:
  ///   You probably should not use this method directly unless you know
  ///   exactly what you are doing. Instead you should use the <see>QueryEngineBase._pushOperationOntoStack</see>,
  ///   <see>QueryEngineBase._newQueryEngineScope</see>, <see>QueryEngineBase._newModelScope</see>, or
  ///   <see>QueryEngineBase._newFieldScope</see> to push operations onto the stack.
  ///
  /// Arguments:
  ///   context: object
  ///     The previous (top-most) "operation context" on top the "operation stack".
  ///   name?: string
  ///     The name for this operation context. This is only ever used when the "scope"
  ///     changes by using one of <see>QueryEngineBase._newQueryEngineScope</see>,
  ///     <see>QueryEngineBase._newModelScope</see>, or <see>QueryEngineBase._newFieldScope</see>.
  ///     These methods will set a "scope name" so that it can later be fetched using
  ///     <see>QueryEngineBase._fetchScope</see>. Scope names will only ever be one of
  ///     `'queryEngine'`, `'model'`, or `'field'`.
  ///   ...args?: Array<object>
  ///     Other objects to merge into the context.
  ///
  /// Return: object
  ///   Return the new "operation context", with the `args` objects merged in. This new
  ///   context will have a `prototype` that is set to the `context` provided.
  ///
  /// See: QueryEngineBase._fetchScope
  _inheritContext(context, name, ...args) {
    let newContext = Object.assign(
      Object.create(context),
      { value: undefined },
      ...args,
      {
        contextID: generateID(),
      },
    );

    if (name) {
      newContext[`${name}Context`] = newContext;
      newContext['currentScopeName'] = name;
    }

    return newContext;
  }

  /// Fetch a previous named scope.
  ///
  /// This is useful when you want to change scopes during query operations.
  /// For example, if you just did a field-level operation inside a <see>FieldScope</see>,
  /// such as `query.field.EQ('test')`, then you need to return a "model scope" so
  /// the user can continue chaining operations. Do to so, you would call: `return this._fetchScope('model');`
  /// which will fetch and return the *current* "model scope".
  ///
  /// If the specified scope is not found (as a previous scope in the "operation stack"),
  /// then this method will simply return `this`. Because this isn't often desired, and could
  /// be a problem, this method takes `scopeNames` as an array of scopes to "fall back" to
  /// if the specified scope can not be found. For example, one could do: `return this._fetchScope('model', 'queryEngine');`
  /// to request the `'model'` scope, but to fall-back to the `'queryEngine'` scope if there
  /// is no `'model'` scope to return.
  ///
  /// Arguments:
  ///   ...scopeNames: Array<string>
  ///     Specify the scopes you wish to fetch, in order, from left-to-right. The first
  ///     scope found will be returned.
  ///
  /// Return: <see>QueryEngine</see> | <see>ModelScope</see> | <see>FieldScope</see>
  ///   Return the specified scope, the first found in the list of provided scopes. If
  ///   no scope specified is found, then return `this` instead.
  ///
  /// See: QueryEngineBase._newQueryEngineScope
  ///
  /// See: QueryEngineBase._newModelScope
  ///
  /// See: QueryEngineBase._newFieldScope
  _fetchScope(...scopeNames) {
    let context = this.getOperationContext();

    for (let i = 0, il = scopeNames.length; i < il; i++) {
      let scopeName = scopeNames[i];
      if (scopeName === 'queryEngine') {
        return this._newQueryEngineScope();
      } else if (scopeName === 'model') {
        let Model = context.Model;
        if (!Model)
          continue;

        return this._newModelScope(Model);
      } else if (scopeName === 'field') {
        let Field = context.Field;
        if (!Field)
          continue;

        return this._newFieldScope(context.Field);
      }
    }

    return this;
  }

  /// Create a new "root scope" (`QueryEngine` scope), push it onto the
  /// "operation stack", and return the newly created scope.
  ///
  /// Return: <see>QueryEngine</see>
  ///   Return a new "root scope" (`QueryEngine` instance), after pushing
  ///   it onto the internal "operation stack". This also "names" the operation
  ///   on the stack, so a call to `this._fetchScope('queryEngine')` after this
  ///   will return this newly created scope.
  ///
  /// See: QueryEngineBase._fetchScope
  _newQueryEngineScope() {
    const QueryEngineClass  = this.getQueryEngineClass();
    let context             = this.currentContext;
    let newContext          = this._inheritContext(context, 'queryEngine');
    let newScope            = new QueryEngineClass(newContext);

    return newScope;
  }

  /// Create a new "model scope" (`ModelScope` scope), push it onto the
  /// "operation stack", and return the newly created scope.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class for this "model scope".
  ///
  /// Return: <see>ModelScope</see>
  ///   Return a new "model scope" (`ModelScope` instance), after pushing
  ///   it onto the internal "operation stack". This also "names" the operation
  ///   on the stack, so a call to `this._fetchScope('model')` after this
  ///   will return this newly created scope.
  ///
  /// See: QueryEngineBase._fetchScope
  _newModelScope(Model) {
    let ModelScopeClass = this.getModelScopeClass();
    let modelName       = Model.getModelName();
    let context         = this.currentContext;
    let extra           = {};
    let isFirst         = !context.rootModel;
    let connection      = this.getConnection();

    if (isFirst) {
      extra.rootModelName = modelName;
      extra.rootModel = Model;
    }

    // Ensure that the specified model
    // is the same dialect as the query
    let modelConnection = Model._getConnection();
    let dialect;

    if (modelConnection) {
      dialect = modelConnection.dialect;
      if (context.dialect && context.dialect !== dialect)
        throw new Error(`QueryEngine: Model "${Model.getModelName()}" is on a different connection dialect than the current query dialect of "${context.dialect}". You can not match different connection dialects in the same query.`);
    }

    if (!dialect && connection)
      dialect = connection.dialect;

    let newContext  = this._inheritContext(context, 'model', { operator: 'MODEL', queryProp: 'Model', Model, modelName, dialect, value: modelName }, extra);
    let newScope    = new ModelScopeClass(newContext);

    // We shouldn't add this scope if this is
    // already the current model of the scope
    if (context.Model !== Model)
      context.operationStack.push(newContext);

    if (isFirst)
      newScope = newScope.PROJECT(Model);

    // But we always need to return the scope
    // for the proxy to work properly
    return newScope;
  }

  /// Create a new "field scope" (`FieldScope` scope), push it onto the
  /// "operation stack", and return the newly created scope.
  ///
  /// Arguments:
  ///   Field: <see>Field</see>
  ///     The field for this "field scope".
  ///
  /// Return: <see>FieldScope</see>
  ///   Return a new "field scope" (`FieldScope` instance), after pushing
  ///   it onto the internal "operation stack". This also "names" the operation
  ///   on the stack, so a call to `this._fetchScope('field')` after this
  ///   will return this newly created scope.
  ///
  /// See: QueryEngineBase._fetchScope
  _newFieldScope(Field) {
    let fieldName       = Field.fieldName;
    let context         = this.currentContext;
    let FieldScopeClass = this.getFieldScopeClass();
    let extra           = {};

    if (!context.rootField) {
      extra.rootFieldName = fieldName;
      extra.rootField = Field;
    }

    let newContext  = this._inheritContext(context, 'field', { operator: 'FIELD', queryProp: 'Field', fieldName, Field, value: fieldName }, extra);
    let newScope    = new FieldScopeClass(newContext);

    // We shouldn't add this scope if this is
    // already the current field of the scope
    if (context.Field !== Field)
      context.operationStack.push(newContext);

    // But we always need to return the scope
    // for the proxy to work properly
    return newScope;
  }

  /// Construct a new QueryEngine instance.
  ///
  /// Arguments:
  ///   context: object
  ///     The "root" "operation context" for the query. This is required to contain a
  ///     `connection` property, with a valid <see>Connection</see> instance as a value. Besides that, you can apply
  ///     any properties you want here, and they will be available on all "operation contexts".
  constructor(context) {
    super();

    this._isMythixQueryEngine = true;

    if (!context)
      throw new TypeError('QueryEngineBase::constructor: "context" required.');

    if (!context.rootContext) {
      context._isMythixQueryEngine = true;
      context.rootContext = context;
    }

    if (!context.operationStack)
      context.operationStack = [];

    if (!('and' in context))
      context.and = true;

    if (!('or' in context))
      context.or = false;

    // console.log(`Creating new ${this.constructor.name} scope: `, context, Object.getPrototypeOf(context));

    let operationStack = context.operationStack;
    Object.defineProperties(this, {
      operationStack: {
        writable:     false,
        enumerable:   false,
        configurable: true,
        value:        operationStack,
      },
      currentContext: {
        enumerable:   false,
        configurable: true,
        get:          () => {
          let currentContext = operationStack[operationStack.length - 1];
          return currentContext || context;
        },
        set: () => {},
      },
    });
  }

  /// Fetch the current (top most) context id of the query.
  ///
  /// Each "operation context" gets its own unique ID. This
  /// is primarily for caching and comparison operations.
  /// Since a new context id is generated for each operation
  /// of the query, one can detect if two queries are identical
  /// simply by comparing their ids. This id can also be used for
  /// cache... since the same id always equates to the exact same
  /// query.
  ///
  /// Return: number
  ///   The unique "operation context" id for this query. This will always
  ///   be the id assigned to the top-most "operation context" of the "operation stack".
  getQueryID() {
    return this.currentContext.contextID;
  }

  /// Return the top-most "operation context" for the query.
  ///
  /// The internal operation stack looks like this `[ root context, <- context1, <- context2, <- context3, ... ]`
  ///
  /// Operation contexts are simple objects defining the query
  /// operations. Each new context added to the query is pushed
  /// on top the "operation stack" internal to the query. Each
  /// "operation context" also has its `prototype` set to the
  /// previous "frame" in the stack. This means from the top-most
  /// context, you can access attributes from the bottom-most
  /// context--assuming the property you are trying to access
  /// hasn't also be re-set in a higher-level "frame". For example,
  /// you could access a `distinct`, `order`, or `projections`
  /// attribute from the top-most operation context, which will
  /// always be the "most current" value for the operation you
  /// are requesting data for.
  ///
  /// Operation contexts always have at least the following properties:
  /// `operator`, `value`, and `queryProp` (used for replaying query operations).
  /// Each context might also have custom properties... for example, a `DISTINCT`
  /// operation will also have a custom `distinct` property it sets that is
  /// the distinct literal itself.
  ///
  /// Return: object
  ///   The top-most "operation context" on the "operation stack".
  getOperationContext() {
    return this.currentContext;
  }

  /// Return the entire "operation stack" for the query.
  ///
  /// The internal operation stack looks like this `[ root context, <- context1, <- context2, <- context3, ... ]`
  ///
  /// Each "frame" on the stack is itself an "operation context". Each
  /// "frame" defines an operation for the query, for example a `MODEL`,
  /// `EQ`, or `DISTINCT` operation. Essentially a Mythix ORM query is
  /// just an array of operations internally--in order. When the query
  /// is being used to interface with the underlying database, the "operation stack"
  /// is walked, and a part generated for each operation in the stack. For example,
  /// a query such as `User.where.id.EQ(1)` would contain the following operations:
  /// `[ { MODEL = User } <- { FIELD = id } <- { operator = EQ, value = 1 } ]`.
  ///
  /// Note:
  ///   You can dynamically alter the operation stack of a query by using one of
  ///   <see>QueryEngineBase.filter</see>, or <see>QueryEngineBase.map</see>. Since
  ///   queries are essentially just arrays of operations, they can be treated much
  ///   like arrays.
  ///
  /// Return: Array<context>
  ///   The entire "operation stack" for the query. This **is not** a copy of the stack,
  ///   but the entire stack directly... so **do not** mutate this value unless you know
  ///   exactly what you are doing.
  getOperationStack() {
    return this.currentContext.operationStack;
  }

  /// Check if the very last operation in the internal "operation stack"
  /// is a "control" operation. "control" operations are operations that
  /// change how the query behaves, and include `LIMIT`, `OFFSET`, `ORDER`,
  /// `GROUP_BY`, `HAVING`, and `PROJECT`.
  ///
  /// Return: boolean
  ///   Return `true` if the very last operation on the "operation stack"
  ///   is categorized as a `control` level operation.
  ///
  /// See: ModelScope.LIMIT
  ///
  /// See: ModelScope.OFFSET
  ///
  /// See: ModelScope.ORDER
  ///
  /// See: ModelScope.GROUP_BY
  ///
  /// See: ModelScope.HAVING
  ///
  /// See: ModelScope.PROJECT
  isLastOperationControl() {
    let queryParts  = this.getOperationStack();
    let lastPart    = queryParts[queryParts.length - 1];

    if (Object.prototype.hasOwnProperty.call(lastPart, 'control') && lastPart.control === true)
      return true;

    return false;
  }

  /// Check if the very last operation in the internal "operation stack"
  /// is a "condition" operation. "condition" operations are operations that
  /// are conditions for the query, for example `EQ`, `GT`, `LT`, etc...
  ///
  /// Return: boolean
  ///   Return `true` if the very last operation on the "operation stack"
  ///   is categorized as a `condition` level operation.
  isLastOperationCondition() {
    let queryParts  = this.getOperationStack();
    let lastPart    = queryParts[queryParts.length - 1];

    if (Object.prototype.hasOwnProperty.call(lastPart, 'condition') && lastPart.condition === true)
      return true;

    return false;
  }

  /// Check if the query has any conditions.
  ///
  /// A query might not have conditions... if for example
  /// it is defining a table-join.
  ///
  /// Return: boolean
  ///   Return `true` if the query has any conditions, i.e. `EQ`, or `GT`.
  queryHasConditions() {
    let context = this.getOperationContext();
    return context.hasCondition;
  }

  /// Check if the query has any table joins.
  ///
  /// Return: boolean
  ///   Return `true` if the query is joining on tables, or `false` otherwise.
  queryHasJoins() {
    let queryParts = this.getOperationStack();
    for (let i = 0, il = queryParts.length; i < il; i++) {
      let queryPart = queryParts[i];
      if (QueryEngineBase.isQuery(queryPart.value) && !queryPart.value.hasCondition)
        return true;
    }

    return false;
  }

  /// Debug a query.
  ///
  /// This will call `console.log` for every operation
  /// on the internal "operation stack", allowing you to
  /// debug each query part--in order.
  logQueryOperations() {
    let query = this.getOperationStack();
    for (let i = 0, il = query.length; i < il; i++) {
      let queryPart = query[i];
      let operator  = queryPart.operator;

      if (operator === 'MODEL')
        console.log(`MODEL -> ${queryPart.Model.getModelName()}`);
      else if (operator === 'FIELD')
        console.log(`FIELD -> ${queryPart.Field.fieldName}`);
      else
        console.log(`${operator} -> ${queryPart.value}`);
    }
  }

  /// Push a new operation onto the internal "operation stack"
  /// for the query.
  ///
  /// Arguments:
  ///   frame: object
  ///     The new "frame" to push onto the stack. This should just be a simple
  ///     object containing the correct properties for the operation being added.
  ///     This method will then ensure your new object is added as an "operation context",
  ///     setting the `prototype` to the previous "operation context" (frame) in
  ///     the stack.
  ///   context?: object
  ///     The context to set as the `prototype` for this new frame. If not provided,
  ///     then this will default to the top-most "operation context" already on
  ///     top of the internal "operation stack".
  _pushOperationOntoStack(queryPart, _context) {
    let context   = _context || this.getOperationContext();
    let operationStack = context.operationStack;

    operationStack.push(
      this._inheritContext(
        context,
        null,
        queryPart,
      ),
    );
  }

  /// Get the `connection` supplied to the query
  /// engine when it was first created.
  ///
  /// Return: <see>Connection</see>
  ///   The `connection` that was supplied to the query engine when it was created.
  getConnection() {
    return this.currentContext.connection;
  }

  /// Get a model class by name.
  ///
  /// This will fetch the `connection` using <see>QueryEngineBase.getConnection</see>,
  /// and then will request the model by name from the connection itself.
  ///
  /// Return: class <see>Model</see>
  ///   Return the named model class.
  getModel(modelName) {
    let connection  = this.getConnection();
    let Model       = (connection && connection.getModel(modelName));

    if (!Model) {
      let models = this.currentContext.models;
      if (models)
        Model = models[modelName];
    }

    return Model;
  }

  /// Fetch the top-most "root scope" or "queryEngine scope".
  ///
  /// Return: <see>QueryEngine</see>
  ///   The top-most "query engine" scope.
  getQueryEngineScope() {
    return this.currentContext.queryEngineScope;
  }

  /// Get the `QueryEngine` class that is being used for the query.
  ///
  /// This works by calling <see>QueryEngineBase.getQueryEngineScope</see>
  /// and returning the `constructor` property used by this scope. The
  /// `constructor` property will be the `QueryEngine` class itself.
  ///
  /// Return: class <see>QueryEngine</see>
  ///   The custom `QueryEngine` class being used for the query, or
  ///   the `QueryEngine` class Mythix ORM uses (the default).
  getQueryEngineClass() {
    return this.currentContext.queryEngineScope.constructor;
  }

  /// Clone this query.
  ///
  /// Return: <see>ModelScope</see> | <see>QueryEngine</see>
  ///   The query, cloned. By default this will return the
  ///   most recent "model scope" from the cloned query... if one is found.

  clone() {
    return this.map((part) => part)._fetchScope('model');
  }

  /// Clone this query, filtering the internal "operation stack"
  /// while doing so. This allows the user to entirely alter the
  /// nature of the query. You can filter out any operations you
  /// want to filter out. For example, you could choose to filter
  /// out all `ORDER` operations to ensure a query never has any
  /// order specified.
  ///
  /// The signature for the provided `callback` nearly matches the
  /// signature for Javascript's `Array.prototype.filter` method:
  /// `callback(operationContext: object, index: number, operationStack: Array<object>, query: QueryEngine)`
  ///
  /// Arguments:
  ///   callback: (operationContext: object, index: number, parts: Array<object>, query: QueryEngine) => boolean;
  ///     The callback to use for filtering. If this callback returns a "falsy" value, then the operation
  ///     will be filtered out.
  ///
  /// Return: <see>FieldScope</see> | <see>ModelScope</see> | <see>QueryEngine</see>
  ///   Return the last (top-most) scope of the original query... unless it was filtered out.
  ///   The returned query will be identical to the original query... minus
  ///   any operation that was filtered out.
  filter(callback) {
    const Klass           = this.getQueryEngineScopeClass();
    let context           = this.getOperationContext();
    let parts             = this.getOperationStack();
    let query             = new Klass({
      ...context.rootContext,
      connection:     this.getConnection(),
      operationStack: [],
    });

    for (let i = 0, il = parts.length; i < il; i++) {
      let part = parts[i];
      if (!part)
        continue;

      if (!callback(part, i, parts, query))
        continue;

      query._pushOperationOntoStack({ ...part });
    }

    return query;
  }

  /// Clone this query, mapping the internal "operation stack"
  /// while doing so. This allows the user to entirely alter the
  /// nature of the query. You can map any operations you
  /// want to alter. For example, you could choose to alter
  /// all `ORDER` operations, forcing a different order for the query.
  ///
  /// The signature for the provided `callback` nearly matches the
  /// signature for Javascript's `Array.prototype.map` method:
  /// `callback(operationContext: object, index: number, operationStack: Array<object>, query: QueryEngine)`
  /// The `operationContext` here is a copy of the original operation context, so it has
  /// its `prototype` disconnected from the context chain, and you can feel free to modify it
  /// (without effecting the original query).
  ///
  /// Arguments:
  ///   callback: (operationContext: object, index: number, parts: Array<object>, query: QueryEngine) => object;
  ///     The callback used to map each "operation context". If the return value is "falsy", or a non-object, then
  ///     it will be discarded (narrowing the "operation stack" of the final mapped query).
  ///
  /// Return: <see>FieldScope</see> | <see>ModelScope</see> | <see>QueryEngine</see>
  ///   Return the last (top-most) scope of the original query.
  ///   The returned query will be identical to the original query... excepting
  ///   any operation that was modified.
  map(callback) {
    const Klass           = this.getQueryEngineScopeClass();
    let context           = this.getOperationContext();
    let parts             = this.getOperationStack();
    let query             = new Klass({
      ...context.rootContext,
      connection:     this.getConnection(),
      operationStack: [],
    });

    for (let i = 0, il = parts.length; i < il; i++) {
      let part = parts[i];
      if (!part)
        continue;

      let newPart = callback({ ...part }, i, parts, query);
      if (!newPart || typeof newPart !== 'object')
        continue;

      query._pushOperationOntoStack(newPart);
    }

    return query;
  }

  /// This method recursively walks the query, calling the provided
  /// `callback` for every sub-query encountered. The provided `callback`
  /// will never be called with `this` query (the root query being walked).
  ///
  /// The callback signature is `callback(subQuery: QueryEngine, parentOperationContext: object, contextKey: string, depth: number): undefined`
  /// where the `subQuery` is the query found, the `parentOperationContext` is the parent "operation context" that the sub-query was found on,
  /// `contextKey` is the key the sub-query was found on (usually `'value'`) inside the `parentOperationContext`, and `depth` is the
  /// depth at which the sub-query was found, which will always be greater than or equal to `1`
  /// (`0` is reserved for the root query itself).
  ///
  /// Note:
  ///   Though you wouldn't generally modify the query while walking it
  ///   (for that you should instead use <see>Connection.finalizeQuery</see>)
  ///   it is possible by setting a new sub-query on the `contextKey` of
  ///   the `parentOperationContext`. i.e. `parentOperationContext[contextKey] = newSubQuery`.
  ///
  /// Arguments:
  ///   callback: (subQuery: QueryEngine, parentOperationContext: object, contextKey: string, depth: number) => void;
  ///     The callback that will be called for each sub-query encountered.
  ///   contextKeys?: Array<string> = [ 'value' ]
  ///     The "operation context" keys to check for sub-queries. This will almost always be "value"
  ///     for each operation... however, if you add custom operations that store sub-queries on
  ///     other operation context property names, then you would want to supply the names of those
  ///     properties here.
  ///
  /// Return: undefined
  ///   Nothing is returned from this method.
  walk(callback, _checkContextKeys) {
    const walkQueries = (query, parent, contextKey, depth) => {
      let parts = query.getOperationStack();
      for (let i = 0, il = parts.length; i < il; i++) {
        let part = parts[i];
        if (!part)
          continue;

        for (let j = 0, jl = checkContextKeys.length; j < jl; j++) {
          let contextKey = checkContextKeys[j];
          if (!Object.prototype.hasOwnProperty.call(part, contextKey))
            continue;

          let value = part[contextKey];
          if (value && this.constructor.isQuery(value))
            walkQueries(value, part, contextKey, depth + 1);
        }
      }

      if (parent !== null)
        callback(query, parent, contextKey, depth);
    };

    let checkContextKeys = _checkContextKeys || [ 'value' ];
    walkQueries(this, null, null, 0);
  }

  /// Fetch all models used in the query.
  ///
  /// By default, this will return all unique models
  /// used across the root query, including models used
  /// for table-joining.
  /// You can however pass the `options` `{ subQueries: true }`
  /// to return all models used in the query, including those
  /// used in sub-queries.
  ///
  /// Arguments:
  ///   options?: object
  ///     Options for the operation. The only option supported is `{ subQueries: true }`,
  ///     which if enabled, will request that this method also walk sub-queries.
  ///
  /// Return: Array<class <see>Model</see>>
  ///   An array of all model classes used in the query.
  getAllModelsUsedInQuery(_options) {
    let options = _options || {};
    let Models  = new Set();
    let query   = this.getOperationStack();

    for (let i = 0, il = query.length; i < il; i++) {
      let queryPart = query[i];

      if (Object.prototype.hasOwnProperty.call(queryPart, 'operator') && queryPart.operator === 'MODEL') {
        let Model = queryPart.Model;
        Models.add(Model);
      } else if (Object.prototype.hasOwnProperty.call(queryPart, 'condition') && queryPart.condition === true) {
        let operatorValue = queryPart.value;
        if (!QueryEngineBase.isQuery(operatorValue))
          continue;

        if (options.subQueries !== true && operatorValue.queryHasConditions())
          continue;

        let SubModels = operatorValue.getAllModelsUsedInQuery(options);
        for (let j = 0, jl = SubModels.length; j < jl; j++) {
          let Model = SubModels[j];
          Models.add(Model);
        }
      }
    }

    let allModels = Array.from(Models.values());
    return allModels;
  }

  /// Check if the model specified is used in the query.
  ///
  /// By default, this will check if the provided `Model`
  /// is used in the root query, or any table-joins in the
  /// root query. You can optionally pass the `options`
  /// `{ subQueries: true }` to also check if the provided
  /// `Model` is used in any sub-queries.
  ///
  /// Note:
  ///   This internally calls <see>QueryEngineBase.getAllModelsUsedInQuery</see>
  ///   an then checks for the existence of the provided `Model` in the result.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class to check for.
  ///   options?: object
  ///     Options for the operation. The only option supported is `{ subQueries: true }`,
  ///     which if enabled, will request that this method also walk sub-queries.
  ///
  /// Return: boolean
  ///   Return `true` if the specified `Model` is used in the query, or
  ///   any table-joins... `false` otherwise.
  isModelUsedInQuery(Model, options) {
    let allModels = this.getAllModelsUsedInQuery(options);
    return (allModels.indexOf(Model) >= 0);
  }
}

module.exports = QueryEngineBase;
