'use strict';

const { DateTime }          = require('luxon');
const EventEmitter          = require('events');
const Nife                  = require('nife');
const SqlString             = require('sqlstring');
const { QueryEngine }       = require('../query-engine');
const Utils                 = require('../utils');
const { Model: ModelBase }  = require('../model');
const Literals              = require('./literals');
const QueryGeneratorBase    = require('./query-generator-base');
const Types                 = require('../types');

const LiteralBase = Literals.LiteralBase;

/// ConnectionBase is the base class that
/// all connection classes should inherit from.
/// It provides common functionality to all
/// connections, and converts literals and
/// types into their native database representation.
///
/// A "connection" in Mythix ORM is essentially
/// the "application" for the ORM. It stores all
/// models used by the connection, stores and defines
/// the query generator (if any), converts literals
/// and types, and also provides the query engine.
///
/// Multiple connections can be used at the same time
/// in Mythix ORM. It is also planned that someday
/// Mythix ORM (or the community) will provide a
/// multiplex connection, to mix multiple connections
/// into a single connection for the entire application.
///
/// For now you can use multiple connections at once.
/// Just know that you should read and fully understand
/// [Connection Binding](./ConnectionBinding) if you plan
/// on using more than one connection for your application.
///
/// ConnectionBase is also an [Event Emitter](https://nodejs.org/docs/latest-v16.x/api/events.html),
/// so you can bind events to a connection instance with `.on`, and unbind
/// with `.off`. Events are connection specific, but common events that
/// can be bound to are:
///
/// 1. `connect` - Commonly fired when a connection is successfully established
/// 2. `acquire` - Commonly fired when a connection is acquired from a connection pool
/// 3. `error` - Commonly fired when a database level error occurs
/// 4. `disconnect` - Commonly fired when a connection is disconnected
///
/// *This is just an example of common connection driver events.
/// Please see documentation for your specific database connection for the proper events.*
///
/// Properties:
///   static Literals: Literals
///     All default Mythix ORM literal classes,
///     provided for convenient access.
///   static dialect: string
///     The dialect of the database, i.e. `'sqlite'`, or `'postgresql'`.
///   dialect: string
///     The dialect of the database, i.e. `'sqlite'`, or `'postgresql'`.
///   static _isMythixConnection: boolean
///     Helper for type checking methods. Should always be `true`.
///
/// Alias: Connection
class ConnectionBase extends EventEmitter {
  static Literals = Literals;

  static DefaultQueryGenerator = QueryGeneratorBase;

  static dialect = 'none';

  static _isMythixConnection = true;

  /// Use this method to check if a class
  /// is a Mythix ORM connection. It will return
  /// `true` if the provided value is a class
  /// that inherits from <see>ConnectionBase</see>, or
  /// if the provided value has an attribute
  /// named `_isMythixConnection` that is truthy.
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: Function
  ///     Value to check.
  static isConnectionClass(value) {
    if (!value)
      return false;

    if (value.prototype instanceof ConnectionBase)
      return true;

    if (value._isMythixConnection)
      return true;

    return false;
  }

  /// Check to see if the provided value is
  /// an *instance* of a Mythix ORM <see>ConnectionBase</see>.
  /// Unlike <see>ConnectionBase.static isConnectionClass</see>, which
  /// checks if a *class* is a <see>ConnectionBase</see>, this will check
  /// to see if an *instance* is an instance of a
  /// Mythix ORM <see>ConnectionBase</see>. It will return
  /// `true` if the provided value is an `instanceof`
  /// <see>ConnectionBase</see>, or if the value's `constructor`
  /// property has a truthy `_isMythixConnection` property
  /// (`value.constructor._isMythixConnection`)
  ///
  /// Return: boolean
  ///
  /// Arguments:
  ///   value: any
  ///     Value to check
  static isConnection(value) {
    if (!value)
      return false;

    if (value instanceof ConnectionBase)
      return true;

    if (value.constructor && value.constructor._isMythixConnection)
      return true;

    return false;
  }

  /// Fetch a literal class by its name.
  ///
  /// The following is a table that displays
  /// the `name` argument that can be provided,
  /// and the resulting Literal class that would
  /// be returned for that name.
  /// | `name`          | Resulting literal class     |
  /// | --------------- | --------------------------- |
  /// | `'Average'`     | <see>AverageLiteral</see>   |
  /// | `'Base'`        | <see>LiteralBase</see>      |
  /// | `'Count'`       | <see>CountLiteral</see>     |
  /// | `'Distinct'`    | <see>DistinctLiteral</see>  |
  /// | `'Field'`       | <see>FieldLiteral</see>     |
  /// | `'FieldBase'`   | <see>LiteralFieldBase</see> |
  /// | `'Literal'`     | <see>Literal</see>          |
  /// | `'Max'`         | <see>MaxLiteral</see>       |
  /// | `'Min'`         | <see>MinLiteral</see>       |
  /// | `'Sum'`         | <see>SumLiteral</see>       |
  ///
  /// Arguments:
  ///   name: string
  ///     The name of the literal class to return.
  ///
  /// Return: class <see>LiteralBase</see>
  ///   Return the literal class requested.
  static getLiteralClassByName(_name) {
    if (!_name)
      return;

    let Klass = Literals[_name];
    if (Klass)
      return Klass;

    let name = Nife.capitalize(_name.toLowerCase());
    if (name === 'Literal')
      return Literals.Literal;
    if (name === 'FieldBase')
      return Literals.LiteralFieldBase;
    else if (name === 'Base')
      return Literals.LiteralBase;

    return Literals[`${name}Literal`];
  }

  /// Create the literal specified by name.
  ///
  /// Whereas <see>ConnectionBase.static getLiteralClassByName</see> will
  /// simply return the literal class specified, this will create
  /// the specified literal, with the provided arguments.
  ///
  /// Please see <see>ConnectionBase.static getLiteralClassByName</see> for
  /// possible names that can be provided as the `name` argument.
  ///
  /// See: ConnectionBase.static getLiteralClassByName
  ///
  /// Arguments:
  ///   name: string
  ///     The name of the literal to create. See
  ///     <see>ConnectionBase.static getLiteralClassByName</see> for
  ///     possible values.
  ///   ...args: Array<any>
  ///     Arguments to provide to the literal constructor.
  ///
  /// Return: <see>LiteralBase</see>
  ///   Return an instance the literal requested,
  ///   using the arguments specified.
  static Literal(name, ...args) {
    const LiteralClass = this.getLiteralClassByName(name);
    if (!LiteralClass)
      throw new Error(`${this.constructor.name}::Literal: Unable to locate literal class for literal name "${name}".`);

    let literal = new LiteralClass(...args);
    return literal;
  }

  /// Create a connection.
  ///
  /// The `options` argument is generally connection
  /// specific. However, there are a few generic options,
  /// which you will find listed in the table below:
  /// | Option | Type | Default Value | Description |
  /// | ------------- | ---- | ------------- | ----------- |
  /// | `bindModels` | `boolean` | `true` | If `true`, then bind the provided `models` to this connection. Models are bound to a connection by setting a static `_mythixBoundConnection` property on each model class to this connection instance. |
  /// | `forceConnectionBinding` | `boolean` | `false` | Normally attempting to bind a connection to a model more than once will result in an exception being thrown. However, if you set this property to `true`, then Mythix ORM will rebind the connection without complaint, even if a connection is already bound to your models. Make sure you know what you are doing if you use this option. |
  /// | `models`      | `Array` or `Object` of <see>Models</see> | `undefined` | Models to provide to this connection. This can be either an array of <see>Models</see>, or an Object where each value is a <see>Model</see> |
  /// | `QueryEngine` | class <see>QueryEngine</see> | <see>QueryEngine</see> | The QueryEngine class to use for this connection and all this connection's models. You can provide your own class if you wish to add onto the query interface. |
  /// | `queryGenerator` | <see>QueryGenerator</see> | *Connection specific* | The query generator for this connection. If a <see>QueryGenerator</see> instance is provided (the correct one for the connection you are using), then this provided query generator will be used. If one isn't provided, then the connection will create its own query generator that is specific to the connection type. |
  ///
  /// Arguments:
  ///   options: object
  ///     Connection specific options to supply to your connection. These
  ///     will commonly include things like a hostname to connect to, a
  ///     user name and password, and any connection specific parameters.
  ///
  /// Return: <see>Connection</see>
  constructor(_options) {
    super();

    this.setMaxListeners(0);

    let options = Object.assign({
      QueryEngine,
    }, _options || {});

    if (!options.queryGenerator)
      options.queryGenerator = new QueryGeneratorBase(this);

    Object.defineProperties(this, {
      'dialect': {
        enumerable:   false,
        configurable: true,
        get:          () => {
          return this.constructor.dialect;
        },
        set:          () => {
        },
      },
      '_models': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        {},
      },
      '_options': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        options,
      },
      '_modelCache': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        new Map(),
      },
      'queryGenerator': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        options.queryGenerator,
      },
    });

    this.registerModels(options.models);
  }

  /// This method is an internal method that parses
  /// the "lock mode" options passed to a call to
  /// <see>Connection.transaction</see>.
  ///
  /// These might be different depending on the connection driver
  /// you are using. Please refer to the documentation
  /// for your connection driver for more details.
  ///
  /// Arguments:
  ///   options: any
  ///     Possibly connection specific lock options for a
  ///     connection's <see>Connection.transaction</see> method.
  ///     Generally this will either be a `true` value, a Model name
  ///     (for which table to lock), or a complete lock options object,
  ///     which generally will look something like:
  ///     `{ lock: boolean; modelName: string; read: boolean; write: boolean }`
  ///     <br>
  ///     <br>`lock` - If `true`, then lock the transaction
  ///     <br>`modelName` - The name of the table(s) to lock
  ///     <br>`read` - If `true`, then lock for reads
  ///     <br>`write` - If `true`, then lock for writes
  ///
  /// Return: object
  ///   Return the lock options for a transaction. These might change
  ///   based on the connection driver you are using, but will generally
  ///   look like `{ lock: boolean; modelName: string; read: boolean; write: boolean }`.
  ///   1. `lock` - If `true`, then lock the transaction
  ///   2. `modelName` - The name of the table(s) to lock
  ///   3. `read` - If `true`, then lock for reads
  ///   4. `write` - If `true`, then lock for writes
  getLockMode(options) {
    if (!options)
      return { lock: false, read: false, write: false };

    const throwError = () => {
      throw new Error(`${this.constructor.name}::getLockMode: "lock" must be the name of a model (lock: "ModelName"), or an object specifying the model and the lock mode (lock: { modelName: "ModelName", read: true, write: true, dependents: true, noWait: false }).`);
    };

    if (Nife.instanceOf(options, 'string')) {
      let Model = this.getModel(options);
      if (!Model)
        throwError();

      return { lock: true, modelName: options, read: true, write: true };
    } else if (Nife.instanceOf(options, 'object')) {
      let modelName = options.modelName;
      let Model = this.getModel(modelName);
      if (!Model)
        throwError();

      return Object.assign({ lock: true, modelName, read: true, write: true }, options);
    } else {
      throwError();
    }
  }

  /// Get the default order for selecting rows
  /// from the database. This will call the
  /// Model's <see name="Model.defaultOrder">Model.static defaultOrder</see> method
  /// first to see if the model specifies a default order
  /// for itself. If it doesn't, then the connection
  /// driver itself might specify a default order for
  /// each table.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model/table to fetch the default order for.
  ///   options: object
  ///     Operation specific options (i.e. options for a "select" call)
  ///
  /// Return: Array<string>
  ///   An array of fully qualified field names for this model should
  ///   be returned by this method. An empty array, `null`, or `undefined`
  ///   are also valid return values (in which case no order will be
  ///   applied to the given operation).
  getDefaultOrder(Model, options) {
    let order = Model.defaultOrder(options);
    if (!order)
      return;

    order = Nife.arrayFlatten(Nife.toArray(order)).filter((value) => {
      if (!value)
        return false;

      if (!Nife.instanceOf(value, 'string'))
        return false;

      return true;
    });

    if (Nife.isEmpty(order))
      return;

    let modelName = Model.getModelName();
    return order.map((value) => ((value.indexOf(':') < 0) ? `${modelName}:${value}` : value));
  }

  /// This method is called (and often provided)
  /// by the underlying database driver to see
  /// if a `LIMIT` clause is allowed to appear in
  /// a given context/operation.
  ///
  /// Arguments:
  ///   options: object
  ///     Driver specific options for the context.
  ///
  /// Return: boolean
  isLimitSupportedInContext(options) {
    return true;
  }

  /// This method is called (and often provided)
  /// by the underlying database driver to see
  /// if an `ORDER BY` clause is allowed to appear in
  /// a given context/operation.
  ///
  /// Arguments:
  ///   options: object
  ///     Driver specific options for the context.
  ///
  /// Return: boolean
  isOrderSupportedInContext(_options) {
    let options = _options || {};
    if (options.isSubQuery) {
      let subQueryOperator = options.subQueryOperator;
      if (subQueryOperator === 'EXISTS' || subQueryOperator === 'NOT EXISTS')
        return true;

      return 'PROJECTION_ONLY';
    }

    return true;
  }

  _getFromModelCache(Model, key, defaultValue) {
    let cache = this._modelCache.get(Model);
    if (!cache)
      return defaultValue;

    let value = cache.get(key);
    if (value == null)
      return defaultValue;

    return value;
  }

  _setToModelCache(Model, key, value) {
    let cache = this._modelCache.get(Model);
    if (!cache) {
      cache = new Map();
      this._modelCache.set(Model, cache);
    }

    cache.set(key, value);

    return value;
  }

  /// Get the options for this connection.
  ///
  /// These will be the options provided to
  /// the connection during creation, plus any
  /// other options the connection driver itself
  /// internally sets.
  ///
  /// Return: object
  ///   The options for this connection.
  getOptions() {
    return this._options;
  }

  /// Check to see if `start` has already been called
  /// on this connection. This is used to know if a
  /// connection is "active" or not.
  ///
  /// Return: boolean
  isStarted() {
    return true;
  }

  /// This will take something that can
  /// be turned into a query and turn it
  /// into a query.
  ///
  /// If a <see>QueryEngine</see> instance
  /// is provided, it will simply be returned.
  ///
  /// If a Model is provided, then `Model.where`
  /// will be returned, returning a <see>QueryEngine</see>
  /// for the model provided.
  ///
  /// In the future this may also accept other possible
  /// values that could be turned into a query.
  ///
  /// This is often internally called by methods of the connection
  /// on a given argument provided by the user, which could validly
  /// be either a model or a query. <see>Connection.destroyModels</see>
  /// is one example of this.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to attempt to turn into a <see>QueryEngine</see>.
  ///
  /// Return: <see>QueryEngine</see> | undefined
  toQueryEngine(_queryEngine) {
    let queryEngine = _queryEngine;
    if (!queryEngine)
      return;

    if (!QueryEngine.isQuery(queryEngine)) {
      if ('where' in queryEngine)
        queryEngine = queryEngine.where(this);
      else
        queryEngine = undefined;
    }

    return queryEngine;
  }

  async finalizeQuery(operation, query) {
    if (query && typeof query.finalizeQuery === 'function')
      return await query.finalizeQuery(operation);

    return query;
  }

  /// Register the provided model class.
  ///
  /// This will register the provided model class with this
  /// connection. The model must not already be bound to
  /// another connection, or you must specify the `option`
  /// `{ forceConnectionBinding: true }` if it is. You
  /// can specify the `option` of `{ bindModels: false }`
  /// if you don't wish to bind this model to this connection.
  ///
  /// Any `options` provided are optional, and will override
  /// the same options provided to the connection itself when
  /// it was created.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class to register with this connection. If not bound, then it
  ///     will simply exist in the model pool for this connection. If bound, then
  ///     this connection will bind itself to the model being registered.
  ///   options?: object
  ///     Options looks like `{ forceConnectionBinding: boolean; bindModels: boolean; }`.
  ///     This is an optional argument. Both options will default to the same options
  ///     provided to the connection when it was created. If you specify either of these
  ///     options they simply override the connection's default.
  ///
  /// Return: class <see>Model</see>
  ///   The registered model class, **which may have changed during registration**.
  ///   It is not uncommon for the connection driver itself to modify the model
  ///   class, or to return a new model class that inherits from your model class.
  ///   The class that is returned should be the class that you use for this connection,
  ///   and will be the same class returned by a call to <see>Connection.getModel</see>,
  ///   or <see>Connection.getModels</see>.
  registerModel(_Model, options) {
    let Model = _Model.bindConnection(this, options);

    this._models[Model.getModelName()] = Model;

    return Model;
  }

  /// Register multiple models at the same time.
  ///
  /// This will register the provided models with this
  /// connection. The models provided must not already be bound to
  /// another connection, or you must specify the `option`
  /// `{ forceConnectionBinding: true }` if any of them are. You
  /// can specify the `option` of `{ bindModels: false }`
  /// if you don't wish to bind these models to this connection.
  ///
  /// Any `options` provided are optional, and will override
  /// the same options provided to the connection itself when
  /// it was created.
  ///
  /// Arguments:
  ///   Model: Array<class <see>Model</see>> | { [key: string]: class <see>Model</see> }
  ///     The model classes to register with this connection. If no models are bound, then
  ///     they will simply exist in the model pool for this connection. If bound, then
  ///     this connection will bind itself to every model being registered.
  ///   options?: object
  ///     Options looks like `{ forceConnectionBinding: boolean; bindModels: boolean; }`.
  ///     This is an optional argument. Both options will default to the same options
  ///     provided to the connection when it was created. If you specify either of these
  ///     options they simply override the connection's default.
  ///
  /// Return: class <see>Model</see>
  ///   The registered model classes, **which may have changed during registration**.
  ///   It is not uncommon for the connection driver itself to modify the model
  ///   classes, or to return a new model classes that inherit from your model classes.
  ///   The classes that are returned should be the classes that you use for this connection,
  ///   and will be the same classes returned by a call to <see>Connection.getModel</see>,
  ///   or <see>Connection.getModels</see>.
  registerModels(models, options) {
    if (!models)
      return;

    let keys = Object.keys(models);

    for (let i = 0, il = keys.length; i < il; i++) {
      let key   = keys[i];
      let Model = models[key];

      this.registerModel(Model, options);
    }

    return this._models;
  }

  /// Get a value from the `AsyncLocalStorage` context.
  ///
  /// [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html)
  /// is used primarily for two purposes: 1) to provide a connection to models, and 2) to
  /// pass a transaction connection down through the call stack. However, it can
  /// also be used for any "context" level values the user wishes to store.
  ///
  /// Note:
  ///   An `AsyncLocalStorage` context might not exist when this call is made,
  ///   in which case this method will return `undefined`, or the `defaultValue`
  ///   if any was provided.
  ///
  /// Return: any
  ///
  /// Arguments:
  ///   key: any
  ///     The key for the value you wish to fetch.
  ///     The underlying "context" provided by `AsyncLocalStorage`
  ///     is a `Map`, so the "key" can be any value.
  ///   defaultValue?: any
  ///     The default value to return if there is no current `AsyncLocalStorage`
  ///     context, or if the requested key is not found.
  getContextValue(key, defaultValue) {
    return Utils.getContextValue(key, defaultValue);
  }

  /// Set a value onto the `AsyncLocalStorage` context.
  ///
  /// [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html)
  /// is used primarily for two purposes: 1) to provide a connection to models, and 2) to
  /// pass a transaction connection down through the call stack. However, it can
  /// also be used for any "context" level values the user wishes to store.
  ///
  /// Note:
  ///   An `AsyncLocalStorage` context might not exist when this call is made,
  ///   in which case this method will do nothing, and silently return.
  ///
  /// Return: any
  ///
  /// Arguments:
  ///   key: any
  ///     The key for the value you wish to set.
  ///     The underlying "context" provided by `AsyncLocalStorage`
  ///     is a `Map`, so the "key" can be any value.
  ///   value: any
  ///     The value to set to the specified key.
  setContextValue(key, value) {
    return Utils.setContextValue(key, value);
  }

  /// This builds a "connection context" to provide to
  /// the [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html) context
  /// of a <see>Connection.createContext</see> call.
  ///
  /// By default, this will set a `connection` property on the context,
  /// which will be the value of this connection instance. It also sets
  /// a `connection` property on the sub-contexts for each model. This
  /// is because models might have different connections, and so need
  /// a context per-model to work properly.
  ///
  /// "How could a model have different connections?" you ask? Well, it
  /// probably won't. But being as this is an [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html)
  /// context, it provides the connection to **every** model inside the context.
  /// Your application might be using other models that aren't part of this connection,
  /// and we wouldn't want those to get the wrong connection for those models.
  ///
  /// Arguments:
  ///   connection?: <see>Connection</see> = this
  ///     The connection to create the context for. This can be provided, and
  ///     is used in place of `this` instance, for example, in the case of transactions.
  ///     Transactions generally are the "same" *connection*, but not the same *instance*.
  ///
  /// Return: Map<any, any>
  ///   The new context that will be used for <see>Connection.createContext</see>.
  buildConnectionContext(_connection) {
    let connection  = _connection || this;
    let models      = connection._models;
    let modelNames  = Object.keys(models);
    let newContext  = new Map();

    newContext.set('connection', connection);

    for (let i = 0, il = modelNames.length; i < il; i++) {
      let modelName         = modelNames[i];
      let Model             = models[modelName];
      let currentModelScope = Model.getModelContext();

      newContext.set(modelName, { ...currentModelScope, connection });
    }

    return newContext;
  }

  /// Create a connection context to serve all code
  /// running inside it this connection.
  ///
  /// This uses [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html)
  /// to create a context that is passed through the entire call stack of the callback. In this
  /// way, a connection can be provided to every model and every operation within the call.
  ///
  /// Arguments:
  ///   callback: async Function
  ///     The method to provide the context to. Every call inside this call stack
  ///     will be provided the connection.
  ///   connection?: <see>Connection</see> = this
  ///     The connection to provide. Generally this will just be `this` connection instance,
  ///     however, it can be specified, and is for example inside transactions.
  ///   thisArg?: any = this
  ///     The `this` value to provide to the given `callback`.
  ///
  /// Return: any
  ///   The return value of the given `callback`.
  async createContext(callback, _connection, thisArg) {
    let connection  = _connection || this;
    let context     = this.buildConnectionContext(connection);

    return await Utils.runInContext(context, async () => {
      return await callback.call(thisArg || this, connection);
    });
  }

  /// Find a specific field across all registered models.
  ///
  /// This method is similar to [Array.find](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/find),
  /// except that the arguments to this `finder` method are those provided by
  /// <see name="Model.iterateFields">Model.static iterateFields</see>. If this `finder` method
  /// returns a truthy value for a given field, then that is the field that will be returned.
  ///
  /// Note:
  ///   This will search for the field across all models registered to the connection.
  ///   To find a specific field for a known model use <see>Connection.getField</see> instead.
  ///
  /// Arguments:
  ///   finder: Function
  ///     A function to assist in finding the specified field. The signature for this
  ///     function must match that used by <see name="Model.iterateFields">Model.static iterateFields</see>.
  ///
  /// Return: Field | undefined
  ///   The field found, if any.
  findModelField(finder) {
    let modelMap    = this.getModels();
    let modelNames  = Object.keys(modelMap);

    for (let i = 0, il = modelNames.length; i < il; i++) {
      let modelName = modelNames[i];
      let Model     = modelMap[modelName];
      let value     = Model.iterateFields(finder, null, false, true);

      if (value)
        return value;
    }
  }

  /// This is simply a convenience method that
  /// calls <see>ModelUtils.parseQualifiedName</see>.
  ///
  /// See: ModelUtils.parseQualifiedName
  ///
  /// Arguments:
  ///   fieldName: string
  ///     The field name, fully qualified field name, or model name to parse.
  ///     See <see>ModelUtils.parseQualifiedName</see> for more information.
  ///
  /// Return: object
  ///   The result. See <see>ModelUtils.parseQualifiedName</see> for more information.
  parseQualifiedName(str) {
    return Utils.parseQualifiedName(str);
  }

  /// Get all models known to this connection.
  /// Models are returned as a raw `Object`, and
  /// so can be destructured. The keys of the returned
  /// object are the model's names.
  ///
  /// Models can be registered to a connection with the
  /// <see>ConnectionBase.registerModel</see> or
  /// <see>ConnectionBase.registerModels</see> methods.
  ///
  /// Return: object
  ///   An object of all models known to the connection, where
  ///   each key is a model name, and each property value is
  ///   the model's class.
  getModels() {
    return this._models;
  }

  /// Retrieve a single model by its name.
  /// The model must be registered with this
  /// connection to be found.
  ///
  /// Models can be registered to a connection with the
  /// <see>ConnectionBase.registerModel</see> or
  /// <see>ConnectionBase.registerModels</see> methods.
  ///
  /// This method is "fully qualified aware", meaning you
  /// can pass a fully qualified name, such as "User:id"
  /// and it will fetch the "User" model.
  ///
  /// Return: class <see>Model</see>
  ///   The model class found by the model's name, or
  ///   `undefined` if no model is found.
  ///
  /// Arguments:
  ///   modelName: string
  ///     The model's name, or a fully qualified field name.
  getModel(modelName) {
    if (typeof modelName === 'symbol')
      return;

    let def = this.parseQualifiedName(modelName);
    return this._models[def.modelName];
  }

  /// Get a field by its fully qualified name,
  /// or by a fieldName + a modelName.
  ///
  /// Return: <see>Field</see>
  ///   The field found, or `undefined` if the specified field is
  ///   not found.
  ///
  /// Arguments:
  ///   fieldName: string
  ///     Fully qualified field name. If a fully qualified field name
  ///     isn't provided, then you *must* provide the `modelName` argument.
  ///   modelName?: string
  ///     The name of the model that the field exists on. This argument is
  ///     optional if the provided `fieldName` is a fully qualified name.
  getField(fieldName, modelName) {
    let def = this.parseQualifiedName(fieldName);
    if (def.modelName == null)
      def.modelName = modelName;

    let Model = this.getModel(def.modelName);
    if (!Model)
      return;

    return Model.getField(def.fieldNames[0]);
  }

  /// Return the <see>QueryEngine</see> class used
  /// for this connection, all its models, and all
  /// query operations. By default this is just the
  /// built-in <see>QueryEngine</see> that Mythix ORM
  /// provides. You can create your own, and provide
  /// it as the `QueryEngine` option to the connection
  /// when you create the connection.
  ///
  /// Return: class <see>QueryEngine</see>
  ///   The QueryEngine to use for this connection, all
  ///   its models, and all query operations for this
  ///   connection.
  getQueryEngineClass() {
    let options = this.getOptions();
    return options.QueryEngine;
  }

  /// Get the <see>QueryGenerator</see> instance
  /// for this connection, if it has one.
  ///
  /// All connection drivers may not have a query generator.
  /// The query generator is the class that takes care of
  /// generating queries for the underlying database (i.e. SQL).
  /// If a query generator exists on a connection, it is very
  /// likely to be unique to the connection (i.e. PostgreSQL and MySQL
  /// have different query generators).
  ///
  /// Return: <see>QueryGenerator</see> | null
  ///   Return the <see>QueryGenerator</see> for this connection,
  ///   or return `null` if none is defined for this connection.
  getQueryGenerator() {
    return this.queryGenerator;
  }

  /// Set the <see>QueryGenerator</see> instance for this
  /// connection. This is rarely used as the <see>QueryGenerator</see>
  /// is often supplied as connection options when creating
  /// the connection (as the `queryGenerator` option). However,
  /// it can be set to a new instance at any time with this
  /// method.
  ///
  /// Return: undefined
  ///
  /// Arguments:
  ///   queryGenerator: <see>QueryGenerator</see> | null
  ///     The new <see>QueryGenerator</see> instance to use
  ///     for generating underlying database queries. Set
  ///     to `null` to specify no query generator (note:
  ///     this might break whatever connection you are using
  ///     as most connections require their query generator).
  setQueryGenerator(queryGenerator) {
    this.queryGenerator = queryGenerator;
  }

  /// The low-level DB interface for escaping a
  /// value. By default this function uses the
  /// [sqlstring](https://www.npmjs.com/package/sqlstring)
  /// module to escape values. However, the `escape`
  /// method for whatever database the connection is
  /// using should be used instead of this. This is
  /// a "default implementation" that is meant as a
  /// fallback when a connection doesn't provide its
  /// own, but each connection should provide its own
  /// when it is able.
  ///
  /// Note:
  ///   This method escapes "values" that are given in
  ///   the underlying query language of the database.
  ///   To escape identifiers, use the <see>ConnectionBase._escapeID</see>
  ///   instead.
  ///
  /// Return: string
  ///   The value provided, escaped for the specific
  ///   underlying database driver.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to escape. This could be a number, a boolean,
  ///     a string, or anything else that can be provided to your
  ///     specific database.
  _escape(value) {
    if (Nife.instanceOf(value, 'string'))
      return `'${value.replace(/'/g, '\'\'')}'`;

    return SqlString.escape(value);
  }

  /// Unlike <see>ConnectionBase._escape</see> --which is
  /// a low-level interface for the database-- this method
  /// will escape specific values in specific ways needed
  /// by Mythix ORM. Said another way, whereas <see>ConnectionBase._escape</see>
  /// is a "low level database method", this is a "high level
  /// Mythix ORM" method.
  ///
  /// If this method is provided a literal, then it will convert
  /// the literal into a string, and return the resulting string.
  /// This is the purpose of the `options` argument. The `options`
  /// argument will be passed to the literal's `toString` method.
  ///
  /// For any non-literal value, it is first passed through the
  /// field's `serialize` method, for example `value = field.type.serialize(value, thisConnection);`.
  /// This generally won't modify the incoming value, but it might,
  /// for example, with DATE, or DATETIME types, that are modified
  /// to match an acceptable format for the underlying database.
  ///
  /// If this method is provided a boolean, then it will return
  /// an upper-cased version of the boolean as a string, i.e. `true` will be returned
  /// as `'TRUE'`, and `false` will be returned as `'FALSE'`.
  ///
  /// If this method is provided a BigInt, then it will convert the
  /// bigint into a string representation of the number, i.e. 42n will
  /// be returned as `'42'`.
  ///
  /// If this method is provided an `Array` value, then the array
  /// is processed by the connection-specific `prepareArrayValuesForSQL`
  /// method. By default, this method will complete the following operations
  /// on the provided array:
  ///
  ///   1. It will flatten the provided array into a 1D array
  ///   2. It will filter the array, such that it removes `undefined`, and any value that isn't a <see>LiteralBase</see>, a `null`, a `string`, a `boolean`, a `number`, or a `bigint`
  ///   3. It will further filter out duplicate values from the array, such that the processed array only contains unique values
  ///
  /// After the array is processed with the `prepareArrayValuesForSQL` method,
  /// it will then be mapped such that each value is passed through this
  /// `escape` method, and then all remaining values in the array will be
  /// joined with a `,` character between each element. The idea here is that
  /// if you are providing an array to the underlying database, it is usually
  /// for an `IN` or `NOT IN` operator, so the result will generally be used for
  /// one of these. Keep in mind however that each connection driver might escape
  /// values (including arrays) differently.
  ///
  /// All other provided values are simply handed off to <see>ConnectionBase._escape</see>.
  ///
  /// Return: string
  ///   The escaped value, as a string.
  ///
  /// Arguments:
  ///   field: <see>Field</see>
  ///     The field this value is coming from.
  ///   value: any
  ///     The value to escape.
  ///   options?: object
  ///     The options to provide to a Literal's `toString`
  ///     method. This `options` object is only ever used
  ///     if the provided `value` is a Literal.
  escape(field, _value, options) {
    var value = _value;
    if (LiteralBase.isLiteral(value))
      return value.toString(this, options);

    value = field.type.serialize(value, this);

    if (value === true) {
      return 'TRUE';
    } else if (value === false) {
      return 'FALSE';
    } else if (typeof value === 'bigint') {
      return value.toString();
    } else if (Array.isArray(value)) {
      let arrayValue = this.prepareArrayValuesForSQL(value);
      if (Nife.isEmpty(arrayValue))
        return '';

      return `(${arrayValue.map((item) => this.escape(field, item)).join(',')})`;
    }

    return this._escape(value);
  }

  /// Low-level database method for escaping an identifier.
  /// Each database driver should provide its own version of
  /// this method. This is the "default" method Mythix ORM
  /// provides as a "fallback" to database drivers that don't
  /// supply their own.
  ///
  /// It works by first stripping all quotes (single `'`, double `"`, and backtick `` ` ``)
  /// from the provided `value`. After this, it will split on the period (dot) character
  /// `.`, and then will map each resulting part through [sqlstring](https://www.npmjs.com/package/sqlstring)
  /// `escapeId` method, finally re-joining the parts with a period `.` character.
  ///
  /// The extra processing is to allow for already escaped identifiers to not be double-escaped.
  ///
  /// Return: string
  ///   The provided identifier, escaped for the underlying database.
  ///
  /// Arguments:
  ///  value: string
  ///    The identifier to escape.
  _escapeID(value) {
    let parts = value.replace(/['"`]/g, '').split(/\.+/g);
    return parts.map((part) => SqlString.escapeId(part).replace(/^`/, '"').replace(/`$/, '"')).join('.');
  }

  /// This method is very similar to <see>ConnectionBase._escapeID</see>,
  /// except that instead of being a "low level database method" that the
  /// database driver itself provides, this is the "Mythix ORM" implementation
  /// of escaping identifiers. The only difference from <see>ConnectionBase._escapeID</see>
  /// is that if the provided value is a Literal, it will be converted to a
  /// string and returned *without* being escaped. Literals are never modified,
  /// and are always provided to the underlying database exactly as they were defined.
  ///
  /// Return: string
  ///   The escaped identifier, or if `value` is a Literal, the Literal
  ///   converted to a string.
  ///
  /// Arguments:
  ///   value: string
  ///     The identifier to escape. If this is a Literal instead of a string,
  ///     then the Literal will be converted to a string and returned.
  ///   options?: object
  ///     The options to pass to `Literal.toString`. This options object is only
  ///     used if the provided `value` is a Literal instance.
  escapeID(value, options) {
    if (LiteralBase.isLiteral(value))
      return value.toString(this.connection, options);

    return this._escapeID(value);
  }

  /// Convert the provided <see>AverageLiteral</see> to a string
  /// for the underlying database driver. The conversion
  /// will be database specific. The database driver connection
  /// is free to override this method.
  ///
  /// Note:
  ///   This method is a proxy method for <see>QueryGenerator._averageLiteralToString</see>.
  ///   If the connection has no instance of a `queryGenerator` available to it,
  ///   then this method will simply return `undefined`.
  ///
  /// Note:
  ///   Generally speaking, Literal "options" are literal (and connection) specific. They often aren't
  ///   needed or used directly by the user, but instead are used by the underlying
  ///   connection itself, to define context specific options. For example, the underlying
  ///   connection might have an option to tell a literal that it is the "DEFAULT" value
  ///   of a field, instead of just a raw value to inject in the generated query stream,
  ///   and this might change the output of the literal when converted to a string.
  ///
  /// Return: string
  ///   The provided <see>AverageLiteral</see> converted to
  ///   a string for the underlying database driver.
  ///
  /// Arguments:
  ///   literal: <see>AverageLiteral</see>
  ///     The literal to convert to a string for the database.
  ///   options?: object
  ///     Optional options to pass to the <see>QueryGenerator._averageLiteralToString</see>
  ///     method.
  _averageLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._averageLiteralToString(literal, options);
  }

  /// Convert the provided <see>CountLiteral</see> to a string
  /// for the underlying database driver. The conversion
  /// will be database specific. The database driver connection
  /// is free to override this method.
  ///
  /// Note:
  ///   This method is a proxy method for <see>QueryGenerator._countLiteralToString</see>.
  ///   If the connection has no instance of a `queryGenerator` available to it,
  ///   then this method will simply return `undefined`.
  ///
  /// Note:
  ///   Generally speaking, Literal "options" are literal (and connection) specific. They often aren't
  ///   needed or used directly by the user, but instead are used by the underlying
  ///   connection itself, to define context specific options. For example, the underlying
  ///   connection might have an option to tell a literal that it is the "DEFAULT" value
  ///   of a field, instead of just a raw value to inject in the generated query stream,
  ///   and this might change the output of the literal when converted to a string.
  ///
  /// Return: string
  ///   The provided <see>CountLiteral</see> converted to
  ///   a string for the underlying database driver.
  ///
  /// Arguments:
  ///   literal: <see>CountLiteral</see>
  ///     The literal to convert to a string for the database.
  ///   options?: object
  ///     Optional options to pass to the <see>QueryGenerator._countLiteralToString</see>
  ///     method.
  _countLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._countLiteralToString(literal, options);
  }

  /// Convert the provided <see>DistinctLiteral</see> to a string
  /// for the underlying database driver. The conversion
  /// will be database specific. The database driver connection
  /// is free to override this method.
  ///
  /// Note:
  ///   This method is a proxy method for <see>QueryGenerator._distinctLiteralToString</see>.
  ///   If the connection has no instance of a `queryGenerator` available to it,
  ///   then this method will simply return `undefined`.
  ///
  /// Note:
  ///   Generally speaking, Literal "options" are literal (and connection) specific. They often aren't
  ///   needed or used directly by the user, but instead are used by the underlying
  ///   connection itself, to define context specific options. For example, the underlying
  ///   connection might have an option to tell a literal that it is the "DEFAULT" value
  ///   of a field, instead of just a raw value to inject in the generated query stream,
  ///   and this might change the output of the literal when converted to a string.
  ///
  /// Return: string
  ///   The provided <see>DistinctLiteral</see> converted to
  ///   a string for the underlying database driver.
  ///
  /// Arguments:
  ///   literal: <see>DistinctLiteral</see>
  ///     The literal to convert to a string for the database.
  ///   options?: object
  ///     Optional options to pass to the <see>QueryGenerator._distinctLiteralToString</see>
  ///     method.
  _distinctLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._distinctLiteralToString(literal, options);
  }

  /// Convert the provided <see>FieldLiteral</see> to a string
  /// for the underlying database driver. The conversion
  /// will be database specific. The database driver connection
  /// is free to override this method.
  ///
  /// Note:
  ///   This method is a proxy method for <see>QueryGenerator._fieldLiteralToString</see>.
  ///   If the connection has no instance of a `queryGenerator` available to it,
  ///   then this method will simply return `undefined`.
  ///
  /// Note:
  ///   Generally speaking, Literal "options" are literal (and connection) specific. They often aren't
  ///   needed or used directly by the user, but instead are used by the underlying
  ///   connection itself, to define context specific options. For example, the underlying
  ///   connection might have an option to tell a literal that it is the "DEFAULT" value
  ///   of a field, instead of just a raw value to inject in the generated query stream,
  ///   and this might change the output of the literal when converted to a string.
  ///
  /// Return: string
  ///   The provided <see>FieldLiteral</see> converted to
  ///   a string for the underlying database driver.
  ///
  /// Arguments:
  ///   literal: <see>FieldLiteral</see>
  ///     The literal to convert to a string for the database.
  ///   options?: object
  ///     Optional options to pass to the <see>QueryGenerator._fieldLiteralToString</see>
  ///     method.
  _fieldLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._fieldLiteralToString(literal, options);
  }

  /// Convert the provided <see>MaxLiteral</see> to a string
  /// for the underlying database driver. The conversion
  /// will be database specific. The database driver connection
  /// is free to override this method.
  ///
  /// Note:
  ///   This method is a proxy method for <see>QueryGenerator._maxLiteralToString</see>.
  ///   If the connection has no instance of a `queryGenerator` available to it,
  ///   then this method will simply return `undefined`.
  ///
  /// Note:
  ///   Generally speaking, Literal "options" are literal (and connection) specific. They often aren't
  ///   needed or used directly by the user, but instead are used by the underlying
  ///   connection itself, to define context specific options. For example, the underlying
  ///   connection might have an option to tell a literal that it is the "DEFAULT" value
  ///   of a field, instead of just a raw value to inject in the generated query stream,
  ///   and this might change the output of the literal when converted to a string.
  ///
  /// Return: string
  ///   The provided <see>MaxLiteral</see> converted to
  ///   a string for the underlying database driver.
  ///
  /// Arguments:
  ///   literal: <see>MaxLiteral</see>
  ///     The literal to convert to a string for the database.
  ///   options?: object
  ///     Optional options to pass to the <see>QueryGenerator._maxLiteralToString</see>
  ///     method.
  _maxLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._maxLiteralToString(literal, options);
  }

  /// Convert the provided <see>MinLiteral</see> to a string
  /// for the underlying database driver. The conversion
  /// will be database specific. The database driver connection
  /// is free to override this method.
  ///
  /// Note:
  ///   This method is a proxy method for <see>QueryGenerator._minLiteralToString</see>.
  ///   If the connection has no instance of a `queryGenerator` available to it,
  ///   then this method will simply return `undefined`.
  ///
  /// Note:
  ///   Generally speaking, Literal "options" are literal (and connection) specific. They often aren't
  ///   needed or used directly by the user, but instead are used by the underlying
  ///   connection itself, to define context specific options. For example, the underlying
  ///   connection might have an option to tell a literal that it is the "DEFAULT" value
  ///   of a field, instead of just a raw value to inject in the generated query stream,
  ///   and this might change the output of the literal when converted to a string.
  ///
  /// Return: string
  ///   The provided <see>MinLiteral</see> converted to
  ///   a string for the underlying database driver.
  ///
  /// Arguments:
  ///   literal: <see>MinLiteral</see>
  ///     The literal to convert to a string for the database.
  ///   options?: object
  ///     Optional options to pass to the <see>QueryGenerator._minLiteralToString</see>
  ///     method.
  _minLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._minLiteralToString(literal, options);
  }

  /// Convert the provided <see>SumLiteral</see> to a string
  /// for the underlying database driver. The conversion
  /// will be database specific. The database driver connection
  /// is free to override this method.
  ///
  /// Note:
  ///   This method is a proxy method for <see>QueryGenerator._sumLiteralToString</see>.
  ///   If the connection has no instance of a `queryGenerator` available to it,
  ///   then this method will simply return `undefined`.
  ///
  /// Note:
  ///   Generally speaking, Literal "options" are literal (and connection) specific. They often aren't
  ///   needed or used directly by the user, but instead are used by the underlying
  ///   connection itself, to define context specific options. For example, the underlying
  ///   connection might have an option to tell a literal that it is the "DEFAULT" value
  ///   of a field, instead of just a raw value to inject in the generated query stream,
  ///   and this might change the output of the literal when converted to a string.
  ///
  /// Return: string
  ///   The provided <see>SumLiteral</see> converted to
  ///   a string for the underlying database driver.
  ///
  /// Arguments:
  ///   literal: <see>SumLiteral</see>
  ///     The literal to convert to a string for the database.
  ///   options?: object
  ///     Optional options to pass to the <see>QueryGenerator._sumLiteralToString</see>
  ///     method.
  _sumLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._sumLiteralToString(literal, options);
  }

  /// Convert the provided Literal to a string
  /// for the underlying database driver. The conversion
  /// will be database specific. The database driver connection
  /// is free to override this method.
  ///
  /// This method will convert a <see>AverageLiteral</see>, a
  /// <see>CountLiteral</see>, a <see>DistinctLiteral</see>,
  /// a <see>FieldLiteral</see>, a <see>MaxLiteral</see>, a
  /// <see>MinLiteral</see>, a <see>SumLiteral</see>, or a
  /// <see>Literal</see> to a string. If the provided literal
  /// is not one of these types, than an exception will be thrown.
  ///
  /// If you want to add custom literals to your application, then
  /// you will need to overload this method, and handle those custom
  /// literals manually (or simply stick to using <see>Literal</see>).
  ///
  /// Return: string
  ///   The provided literal, converted to a string for the underlying
  ///   database driver.
  ///
  /// Arguments:
  ///   literal: <see>LiteralBase</see>
  ///     The literal to convert.
  ///   options?: object
  ///     Optional options that can be passed to the literal conversion
  ///     method. These are generally not provided by the user, but rather
  ///     are often provided by the connection itself, for context-specific
  ///     literal conversions. These options can and will changed based on
  ///     the literal being converted, and the underlying connection.
  literalToString(literal, options) {
    if (Literals.AverageLiteral.isLiteralType(literal))
      return this._averageLiteralToString(literal, options);
    else if (Literals.CountLiteral.isLiteralType(literal))
      return this._countLiteralToString(literal, options);
    else if (Literals.DistinctLiteral.isLiteralType(literal))
      return this._distinctLiteralToString(literal, options);
    else if (Literals.FieldLiteral.isLiteralType(literal))
      return this._fieldLiteralToString(literal, options);
    else if (Literals.MaxLiteral.isLiteralType(literal))
      return this._maxLiteralToString(literal, options);
    else if (Literals.MinLiteral.isLiteralType(literal))
      return this._minLiteralToString(literal, options);
    else if (Literals.SumLiteral.isLiteralType(literal))
      return this._sumLiteralToString(literal, options);
    else if (Literals.Literal.isLiteralType(literal))
      return literal.toString(this, options);

    throw new Error(`${this.constructor.name}::literalToString: Unsupported literal ${literal}.`);
  }

  /// Convert a "BIGINT" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `BIGINT`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _bigintTypeToString(type) {
    return 'BIGINT';
  }

  /// Convert a "BLOB" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `BLOB`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _blobTypeToString(type) {
    return 'BLOB';
  }

  /// Convert a "BOOLEAN" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `BOOLEAN`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _booleanTypeToString(type) {
    return 'BOOLEAN';
  }

  /// Convert a "CHAR" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `CHAR`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _charTypeToString(type) {
    return 'CHAR';
  }

  /// Convert a "DATE" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `BIGINT`,
  /// however, it may be different based on what database you are using.
  ///
  /// Note:
  ///   Mythix ORM always stores DATE and DATETIME types as a timestamp
  ///   (in milliseconds from the UNIX Epoch) whenever it is able to.
  ///   This is why the default type for most connection drivers is
  ///   `BIGINT`. Another common type for this conversion is `TIMESTAMP`.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _dateTypeToString(type) {
    return 'TIMESTAMP';
  }

  /// Convert a "DATETIME" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `BIGINT`,
  /// however, it may be different based on what database you are using.
  ///
  /// Note:
  ///   Mythix ORM always stores DATE and DATETIME types as a timestamp
  ///   (in milliseconds from the UNIX Epoch) whenever it is able to.
  ///   This is why the default type for most connection drivers is
  ///   `BIGINT`. Another common type for this conversion is `TIMESTAMP`.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _datetimeTypeToString(type) {
    return 'TIMESTAMP';
  }

  /// Convert a "NUMERIC" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `NUMERIC`, `DECIMAL`,
  /// or `NUMBER`, however, it may be different based on what database
  /// you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _numericTypeToString(type) {
    return `NUMERIC(${type.precision}, ${type.scale})`;
  }

  /// Convert a "REAL" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `FLOAT`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _realTypeToString(type) {
    return 'FLOAT';
  }

  /// Convert a "INTEGER" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `INTEGER`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _integerTypeToString(type) {
    return 'INTEGER';
  }

  /// Convert a "STRING" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `VARCHAR`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _stringTypeToString(type) {
    return `VARCHAR(${type.length})`;
  }

  /// Convert a "TEXT" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `TEXT`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _textTypeToString(type) {
    return 'TEXT';
  }

  /// Convert a "UUIDV1" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `VARCHAR`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _uuidV1TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  /// Convert a "UUIDV3" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `VARCHAR`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _uuidV3TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  /// Convert a "UUIDV4" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `VARCHAR`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _uuidV4TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  /// Convert a "UUIDV5" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `VARCHAR`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _uuidV5TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  /// Convert a "XID" field type to a type
  /// acceptable by the underlying database driver.
  ///
  /// For most SQL connections this would be `VARCHAR`,
  /// however, it may be different based on what database you are using.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  // eslint-disable-next-line no-unused-vars
  _xidTypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  /// Convert any field type to the type needed for the underlying
  /// database driver. Only built-in Mythix ORM fields are supported.
  /// If a custom field type that is not supported is provided then
  /// an exception will be thrown.
  ///
  /// If you need to support a custom field type, simply subclass the
  /// connection you are using, and overload this method to handle
  /// your custom field types.
  ///
  /// Return: string
  ///   The field type needed by the underlying database driver.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field type to convert to use in the underlying database.
  ///   options?: object
  ///     Optional options to pass to the underlying conversion method.
  typeToString(type, options) {
    if (Types.BigIntType.isSameType(type))
      return this._bigintTypeToString(type, options);
    else if (Types.BlobType.isSameType(type))
      return this._blobTypeToString(type, options);
    else if (Types.BooleanType.isSameType(type))
      return this._booleanTypeToString(type, options);
    else if (Types.CharType.isSameType(type))
      return this._charTypeToString(type, options);
    else if (Types.DateType.isSameType(type))
      return this._dateTypeToString(type, options);
    else if (Types.DateTimeType.isSameType(type))
      return this._datetimeTypeToString(type, options);
    else if (Types.NumericType.isSameType(type))
      return this._numericTypeToString(type, options);
    else if (Types.RealType.isSameType(type))
      return this._realTypeToString(type, options);
    else if (Types.IntegerType.isSameType(type))
      return this._integerTypeToString(type, options);
    else if (Types.StringType.isSameType(type))
      return this._stringTypeToString(type, options);
    else if (Types.TextType.isSameType(type))
      return this._textTypeToString(type, options);
    else if (Types.UUIDV1Type.isSameType(type))
      return this._uuidV1TypeToString(type, options);
    else if (Types.UUIDV3Type.isSameType(type))
      return this._uuidV3TypeToString(type, options);
    else if (Types.UUIDV4Type.isSameType(type))
      return this._uuidV4TypeToString(type, options);
    else if (Types.UUIDV5Type.isSameType(type))
      return this._uuidV5TypeToString(type, options);
    else if (Types.XIDType.isSameType(type))
      return this._xidTypeToString(type, options);

    throw new Error(`${this.constructor.name}::typeToString: Unsupported type ${type}.`);
  }

  /// Convert a given "time" type object to
  /// the value needed by the underlying database.
  ///
  /// By default this method will take a
  /// Luxon `DateTime` instance, or a `Date` instance,
  /// and convert it to the value needed by the
  /// underlying field in the database. This is
  /// generally a `BIGINT`, or `TIMESTAMP` value,
  /// as Mythix ORM will always try to store dates
  /// and times as millisecond timestamps
  /// (number of milliseconds since the UNIX Epoch).
  ///
  /// If a `number`, `bigint`, or a `string` type is
  /// provided, then Mythix ORM will use Luxon to try
  /// and parse the provided value. If the value is parsed
  /// correctly, it will then be converted to the proper
  /// value as needed by the underlying field in the database.
  ///
  /// Return: any
  ///   Return the value needed by the underlying database field.
  ///   Generally this will be a `bigint` type that is returned,
  ///   but may be something different depending on the field and
  ///   the database itself.
  ///
  /// Arguments:
  ///   value: any
  ///     The incoming date/time type to convert to the proper
  ///     database value for the underlying field.
  ///   type: <see>Type</see>
  ///     The field type that this conversion is for. This will
  ///     generally be a `DATE` or `DATETIME` type.
  convertDateToDBTime(value, type) {
    if (Nife.instanceOf(value, 'number'))
      return value;
    else if (Nife.instanceOf(value, 'bigint'))
      return Number(value).valueOf();
    else if (DateTime.isDateTime(value))
      return value.toMillis();
    else if (value instanceof Date || (value && value.constructor && value.constructor.name === 'Date'))
      return value.valueOf();
    else if (Nife.instanceOf(value, 'string'))
      return DateTime.fromISO(value).toMillis();

    return value;
  }

  /// This method will ensure all provided "models"
  /// are instances of the provided model class.
  ///
  /// This method is used by the `insert`, `upsert`, and `update`
  /// methods of the connection to ensure every model
  /// provided by the user is an actual model instance.
  ///
  /// For example, it is perfectly valid to create a model
  /// like `await Model.create({ ...attributes })`. As you
  /// can see, the provided "model" is actually just a raw
  /// object. Nearly any call in Mythix ORM will accept a
  /// raw object in-place of a model instance, and this
  /// method is used to ensure all provided "models" are
  /// actually model instances.
  ///
  /// Return: Array<Model>
  ///   Return all provided models, converted to model instances.
  ///   Any provided model that is already a model instance will
  ///   not be modified, and instead will simply be returned.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class to use for instantiating models.
  ///   models: object | <see>Model</see> | Array<Model | object>
  ///     The models to convert to model instances (if needed).
  ///   options: object
  ///     Options for the operation. This options object is used
  ///     to pass the `startIndex` and `endIndex` (or `batchSize`)
  ///     as provided by the user when calling `insert`, `upsert`, or `update`.
  ensureAllModelsAreInstances(Model, _models, options) {
    if (!_models)
      return [];

    if (_models._mythixPreparedModels)
      return _models._mythixPreparedModels.models;

    let instantiatedModels  = [];
    let startIndex          = options.startIndex || 0;
    let models              = Nife.toArray(_models);
    let endIndex            = models.length;

    if (options.endIndex)
      endIndex = Math.min(options.endIndex, endIndex);
    else if (options.batchSize)
      endIndex = Math.min(startIndex + options.batchSize, endIndex);

    for (let i = startIndex; i < endIndex; i++) {
      let model = models[i];

      if (!ModelBase.isModel(model))
        model = new Model(model, { connection: this });

      instantiatedModels.push(model);
    }

    return instantiatedModels;
  }

  /// This method will prepare all provided models for
  /// an `insert`, `upsert`, or `update` operation.
  ///
  /// What it does is ensure every provided model is a model
  /// instance, checks if the model is persisted, and if each
  /// model has dirty fields.
  ///
  /// Models that are persisted and not dirty will be filtered out,
  /// so that proceeding operations will "ignore" them entirely.
  ///
  /// All dirty fields across all models are combined into a unified
  /// list of dirty fields. This unified list is then used for the list
  /// of columns in an `insert`, `upsert`, or `updateAll` operation.
  ///
  /// Return: PreparedModels
  ///   Return an object with the shape `{ models: Array<Model>, dirtyFields: Array<Field>, dirtyModels: Array<Model> }`
  ///   which is known to Mythix ORM as "prepared models". Mythix ORM can understand
  ///   "prepared models" verses simple "models" in most contexts. PreparedModels are often
  ///   passed around after first receiving models from the user, so as not to "prepare" them
  ///   more than once.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class to use for preparing the provided models. All models
  ///     provided must be of this model type.
  ///   models: object | <see>Model</see> | Array<Model | object>
  ///     The models to prepare. Any model that is a "raw object" will
  ///     be instantiated into the Model class provided.
  ///   options: object
  ///     Options for the operation. These include `startIndex`, `endIndex`
  ///     (or `batchSize`) for batch operations, and `skipPersisted`, `isUpdateOperation`,
  ///     or `isInsertOperation` for other context-specific operations.
  prepareAllModelsForOperation(Model, _models, _options) {
    if (!_models)
      return {};

    if (_models._mythixPreparedModels)
      return _models._mythixPreparedModels;

    if (Array.isArray(_models) && !_models.length)
      return {};

    let options           = _options || {};
    let finalizedModels   = [];
    let dirtyModels       = [];
    let dirtyFieldNames   = {};
    let dirtyFields       = [];
    let hasAllFieldNames  = false;
    let totalFieldCount   = Model.getConcreteFieldCount();
    let startIndex        = options.startIndex || 0;
    let models            = Nife.toArray(_models);
    let endIndex          = models.length;

    if (options.endIndex)
      endIndex = Math.min(options.endIndex, endIndex);
    else if (options.batchSize)
      endIndex = Math.min(startIndex + options.batchSize, endIndex);

    // Make sure all items are models,
    // and find all the dirty fields
    for (let i = startIndex; i < endIndex; i++) {
      let model = models[i];

      if (!ModelBase.isModel(model)) {
        model = new Model(model, { connection: this });
      } else if (model.isPersisted() && options.skipPersisted) {
        if (model.isDirty())
          dirtyModels.push(model);

        continue;
      }

      if (!hasAllFieldNames) {
        Object.assign(
          dirtyFieldNames,
          model._getDirtyFields({
            update: options.isUpdateOperation,
            insert: options.isInsertOperation,
          }),
        );

        if (Object.keys(dirtyFieldNames).length >= totalFieldCount)
          hasAllFieldNames = true;
      }

      finalizedModels.push(model);
    }

    let fieldNames = Object.keys(dirtyFieldNames);
    for (let i = 0, il = fieldNames.length; i < il; i++) {
      let fieldName = fieldNames[i];
      let field     = Model.getField(fieldName);
      if (!field)
        continue;

      dirtyFields.push(field);
    }

    let finalResult = { models: finalizedModels, dirtyFields, dirtyModels };
    Object.defineProperties(finalResult, {
      '_mythixPreparedModels': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        finalResult,
      },
    });

    return finalResult;
  }

  /// Recursively walk all provided models and split models into a map
  /// of model name and model instances.
  ///
  /// This method is used to split apart provided models by the user.
  /// For example, it is fully valid to provide "sub models" during most persisting
  /// operations, such as `new User({ primaryRole: new Role({ name: 'admin' }) })`.
  ///
  /// This method would find the sub model "Role" in the above example, and split
  /// it out to be processed separately in the persisting operation.
  ///
  /// Return: Map<string, Set<Model>>
  ///   Return a map of all models found, where the model name is the key for the
  ///   `Map`, and each model instance is added to the `Set` for that key.
  ///   Using the above example, the return value would be: `new Map({ User: new Set([ user ]), Role: new Set([ role ]) })`.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class of the primary model being scanned. The primary model could
  ///     also be called the "root model". In our example above the primary model is `User`.
  ///   primaryModel: <see>Model</see>
  ///     The model instance to scan for "sub models". In the example above this would be
  ///     `User` (though `Role` itself will also be passed through this method to check if
  ///     it also has any sub models).
  ///   _relationMap?: Map<string, Set<Model>>
  ///     This argument is provided internally while recursing, and should **not** be provided
  ///     unless you know exactly what you are doing.
  splitModelAndSubModels(Model, primaryModel, _relationMap) {
    const addModelInstance = (modelName, self) => {
      let relatedModels = relationMap.get(modelName);
      if (!relatedModels) {
        relatedModels = new Set();
        relationMap.set(modelName, relatedModels);
      }

      relatedModels.add(self);
    };

    const splitSubModels = (Model, model) => {
      if (alreadyVisitedMap.get(model))
        return;

      alreadyVisitedMap.set(model, true);

      let modelName = Model.getModelName();

      Model.iterateFields(({ field, fieldName }) => {
        let fieldType = field.type;
        if (!fieldType.isRelational())
          return;

        // We don't deal with multi-relational fields
        if (fieldType.isManyRelation())
          return;

        let TargetModel     = fieldType.getTargetModel(this);
        let targetModelName = TargetModel.getModelName();
        if (targetModelName === modelName || targetModelName === primaryModelName)
          return;

        let modelInstance = model[fieldName];
        if (modelInstance == null)
          return;

        if (!(modelInstance instanceof TargetModel))
          modelInstance = new TargetModel(modelInstance, { connection: this });

        addModelInstance(targetModelName, modelInstance);

        // Sub model data may have another
        // model to create
        splitSubModels(TargetModel, modelInstance, relationMap);
      });
    };

    let alreadyVisitedMap = new Map();
    let relationMap       = _relationMap || new Map();
    let primaryModelName  = Model.getModelName();

    splitSubModels(Model, primaryModel);

    return relationMap;
  }

  /// Recursively prepare all models for a persisting operation
  /// while also splitting each model out into its own separate
  /// list, and finally sorting the models based on insertion order.
  ///
  /// This method will use <see>ConnectionBase.splitModelAndSubModels</see>
  /// to split provided models out into their own separate space. See this
  /// method for a better explanation of what this means.
  ///
  /// It will also assign any foreign key values across models that it is able to.
  /// For example, in the example provided in the documentation for <see>ConnectionBase.splitModelAndSubModels</see>,
  /// we had `new User({ primaryRole: new Role({ name: 'admin' }) })`, where the `Role`
  /// model is being persisted (as a child of `User`) at the same time the `User` is being
  /// persisted. If `Role` has a generated id, such as one of the `UUID` or `XID` types,
  /// then in our example here the `primaryRoleID` of the `User` could be set before `User`
  /// is persisted, saving an extra query to the database to update this foreign key after
  /// `Role` is persisted. In short, this method will also assign any foreign key values
  /// that it already has before persisting any models.
  ///
  /// Finally, after it has "prepared" the models, and split all models into their own
  /// space, it will sort the resulting `Map` such that the models are in the correct
  /// order for insertion. In the example we have been using, if `User` has a `primaryRoleID`
  /// that is a foreign key, then it might require a non-null value before the `User`
  /// model can be persisted. Because of this, it is important that the `Role` model
  /// is stored first, so that we have the value we need for this `primaryRoleID` foreign key.
  /// This is why the models are sorted in what is known as "creation order", or "insertion order".
  /// This order is defined by the foreign keys themselves. Mythix ORM will walk all foreign keys
  /// involved in the operation, and decide based on these what the "creation order" should be.
  /// If you have two models that both have foreign keys pointing to each other, then the sort
  /// order is undefined. If this is the case, the work-around is to simply manually persist
  /// your models in the correct order (i.e. save `Role` first, and then supply the foreign key
  /// to `User` yourself from the result).
  ///
  /// Return: Map<string, Set<Model>>
  ///   The models processed, and put into their own named `Set`. The keys for
  ///   the `Map` are the names of the models themselves.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class of the primary model being persisted. The primary model could
  ///     also be called the "root model". In our example above the primary model is `User`.
  ///   models: object | <see>Model</see> | Array<Model | object>
  ///     The models to prepare. Any model that is a "raw object" will
  ///     be instantiated into the Model class provided. Any "sub models" will be
  ///     split into their own space in the resulting `Map`.
  ///   options?: object
  ///     Optional options to supply to this operation. This options object
  ///     isn't used by Mythix ORM, but is provided in case any driver specific
  ///     connection needs them when overloading this model. These options come
  ///     from the options provided to a database operation, such as the options
  ///     provided to an `insert`, `upsert`, or `update` call.
  // eslint-disable-next-line no-unused-vars
  prepareAllModelsAndSubModelsForOperation(Model, models, _options) {
    let primaryModelRelationMap = new Map();
    let groupedModelMap         = new Map();
    let primaryModelName        = Model.getModelName();

    const addModelToGroup = (modelName, self) => {
      let group = groupedModelMap.get(modelName);
      if (!group) {
        group = new Set();
        groupedModelMap.set(modelName, group);
      }

      group.add(self);
    };

    // Collect all primary models and sub-models
    for (let i = 0, il = models.length; i < il; i++) {
      let model = models[i];
      if (!ModelBase.isModel(model))
        model = new Model(model, { connection: this });

      let subModels = this.splitModelAndSubModels(Model, model);

      primaryModelRelationMap.set(model, subModels);
      addModelToGroup(primaryModelName, model);

      for (let [ modelName, models ] of subModels.entries()) {
        for (let subModel of models.values())
          addModelToGroup(modelName, subModel);
      }
    }

    // Sort group map by model creation order
    let sortedGroupedModelMap = new Map();
    let sortedModelNames      = Utils.sortModelNamesByCreationOrder(this, Array.from(groupedModelMap.keys()));
    for (let i = 0, il = sortedModelNames.length; i < il; i++) {
      let groupModelName  = sortedModelNames[i];
      let subModels       = Array.from(groupedModelMap.get(groupModelName).values());

      sortedGroupedModelMap.set(groupModelName, subModels);
    }

    // Now copy related field values between all instantiated models
    for (let [ primaryModel, subModelMap ] of primaryModelRelationMap.entries()) {
      for (let [ targetModelName, targetModels ] of subModelMap.entries()) {
        let TargetModel = this.getModel(targetModelName);

        for (let targetModel of targetModels.values()) {
          if (targetModel !== primaryModel) {
            Utils.setRelationalValues(this, TargetModel, targetModel, Model, primaryModel);
            Utils.setRelationalValues(this, Model, primaryModel, TargetModel, targetModel);
          }

          for (let [ sourceModelName, sourceModels ] of subModelMap.entries()) {
            if (sourceModelName === targetModelName)
              continue;

            if (!TargetModel.isForeignKeyTargetModel(this, sourceModelName))
              continue;

            let SourceModel = this.getModel(sourceModelName);
            for (let sourceModel of sourceModels.values()) {
              if (sourceModel === primaryModel)
                continue;

              Utils.setRelationalValues(this, TargetModel, targetModel, SourceModel, sourceModel);
            }
          }
        }
      }
    }

    return sortedGroupedModelMap;
  }

  /// This is a low-level "helper" method that is used by all
  /// "bulk operations", such as `insert`, `upsert`, `update`,
  /// or `destroyModels`. It prepares all provided models for operation,
  /// splits out sub-models provided, skips already persisted models,
  /// and also calls the model hooks `onBeforeSave`, `onAfterSave`, etc...
  /// This method will also always operate in batches, as defined by the
  /// `batchSize` option (default is `500` if not provided).
  ///
  /// Lastly, when it is all done running the "batch operation" on all models,
  /// it will update any foreign keys on each model, and if any model has again
  /// been marked dirty from this update it will finally persist those updated
  /// models.
  ///
  /// When it is fully complete, this method will return all "primary" models supplied
  /// to the method (i.e. if saving `User` models, then all supplied users will be returned
  /// and if any sub models were involved those won't be returned). The "primary model" is
  /// the model whose model class was specified as the `Model` argument on invocation of this
  /// method, and should match the type of the input `models`.
  ///
  /// Return: Array<Model>
  ///   Return all input models, converted to model instances, and updated. How they
  ///   are updated depends on the operation being performed.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class for the input models being provided.
  ///   models: Array<Model>
  ///     The models to bulk operate on. All of them must be instances of the class
  ///     provided as the `Model` argument.
  ///   options?: object
  ///     Options to supply to the method. Refer to the following table for a list of
  ///     possible options.
  ///     | Option | Type | Default Value | Description |
  ///     | ------ | ---- | ------------- | ----------- |
  ///     | `batchSize` | `number` | `500` | The number of models for each batch |
  ///     | `isInsertOperation` | `boolean` | `false` | `true` if this is an insert operation. If this is an insert operation, then dirty models will be collected post-insert, and re-saved if any are re-marked dirty after foreign key updates. |
  ///     | `isDeleteOperation` | `boolean` | `false` | `true` if this is a delete operation. If this is a delete operation, then model preparation and splitting is skipped, and the models are processed directly. |
  ///   beforeCallback?: (Model: typeof <see>Model</see>, batchModelInstances: Array<Model>, options: object, queryGenerator: <see>QueryGenerator</see>) => Promise<void>
  ///     Callback that is called for each batch of models, before the current operation (i.e. insert) operates on them.
  ///     It is generally in this callback that `onBefore*` model hooks are called.
  ///   callback: (Model: typeof <see>Model</see>, preparedModels: <see name="PreparedModels">ConnectionBase.prepareAllModelsForOperation</see>, options: object, queryGenerator: <see>QueryGenerator</see>) => Promise<void>
  ///     Callback to call to process each batch of models. The models are in "prepared model" format, meaning they
  ///     are supplied as an object with `models`, `dirtyFields`, and `dirtyModels` properties.
  ///   afterCallback?: (Model: typeof <see>Model</see>, models: Array<Model>, options: object, queryGenerator: <see>QueryGenerator</see>) => Promise<void>
  ///     Callback that is called for each entire model set (not in batches). For example, if you bulk insert
  ///     1000 users, and your `batchSize` is `100`, then this will be called once, with all
  ///     1000 processed users, not 10 times with 100 users each. It is generally in this callback
  ///     that `onAfter*` model hooks are called.
  ///   afterOperationCallback?: (Model: typeof <see>Model</see>, dirtyModels: Set<Model>, options: object, queryGenerator: <see>QueryGenerator</see>) => Promise<void>
  ///     Callback that is called after the entire operation completes successfully. This will only be called
  ///     if `dirtyModels` is not empty. This method generally will re-save any models that got dirty during
  ///     the operation... for example, when foreign keys have been updated on the models being processed.
  ///     Models won't be added to the `dirtyModels` set if they are marked dirty in `onAfter*` model hooks,
  ///     but only if the foreign key update process marked the model as dirty.
  async bulkModelOperation(Model, _models, _options, beforeCallback, callback, afterCallback, afterOperationCallback) {
    let models = _models;
    if (!models)
      return;

    let inputIsArray = false;
    if (!Array.isArray(models)) {
      if (!models._mythixPreparedModels) {
        if (Nife.instanceOf(models, 'map', 'set'))
          inputIsArray = true;

        models = Nife.toArray(models).filter(Boolean);
      } else {
        inputIsArray = true;
      }
    } else {
      inputIsArray = true;
    }

    if (Nife.isEmpty(models))
      return (inputIsArray) ? [] : undefined;

    let queryGenerator  = this.getQueryGenerator();
    let options         = Object.assign({}, _options || {});
    let batchSize       = options.batchSize || 500;
    if (batchSize < 1)
      throw new Error(`${this.constructor.name}::bulkModelOperation: "batchSize" can not be less than 1.`);

    const computeBulkModels = async (Model, _models, options) => {
      let models      = Nife.toArray(_models);
      let totalModels = models.length;
      if (totalModels === 0)
        return { results: finalResults, dirtyModels };

      let offset        = 0;
      let finalResults  = [];
      let dirtyModels   = [];

      options.endIndex = 0;

      while (options.endIndex < totalModels) {
        options.startIndex = offset;
        options.endIndex = offset + batchSize;

        if (options.endIndex >= totalModels)
          options.endIndex = totalModels;

        // We need to run before callbacks first
        // because this might change the fields
        // that are dirty
        let batchModelInstances = this.ensureAllModelsAreInstances(Model, models, options);
        if (typeof beforeCallback === 'function')
          await beforeCallback.call(this, Model, batchModelInstances, options, queryGenerator);

        let preparedModels = this.prepareAllModelsForOperation(Model, batchModelInstances, Object.assign({}, options, { startIndex: 0, endIndex: batchModelInstances.length }));
        if (preparedModels.models.length === 0) {
          // no dirty models in this batch
          offset += batchSize;
          continue;
        }

        await callback.call(this, Model, preparedModels, options, queryGenerator);

        if (options.isInsertOperation)
          dirtyModels = dirtyModels.concat(preparedModels.dirtyModels);

        let batchModels = preparedModels.models;
        if (batchModels) {
          for (let i = 0, il = batchModels.length; i < il; i++) {
            let batchModel = batchModels[i];
            // batchModel.clearDirty();
            finalResults.push(batchModel);
          }
        }

        offset += batchSize;
      }

      return { results: finalResults, dirtyModels };
    };

    if (options.isDeleteOperation) {
      // For delete we DO NOT want to collect
      // related models... just delete what we have
      let { results } = await computeBulkModels(Model, models, options);
      return (inputIsArray || !results) ? results : results[0];
    }

    let primaryModelName  = Model.getModelName();
    let groupedModelMap   = this.prepareAllModelsAndSubModelsForOperation(Model, models, options);
    let alreadyStored     = {};
    let allDirtyModels    = new Set();
    let allStoredModels   = new Set();
    let primaryResult;

    for (let [ modelName, models ] of groupedModelMap) {
      let GroupModel                = this.getModel(modelName);
      let { results, dirtyModels }  = await computeBulkModels(GroupModel, models, options);

      // If prepareAllModelsForOperation found persisted
      // models that were dirty, then add them here
      if (dirtyModels && dirtyModels.length > 0) {
        for (let i = 0, il = dirtyModels.length; i < il; i++)
          allDirtyModels.add(dirtyModels[i]);
      }

      alreadyStored[modelName] = true;

      if (modelName === primaryModelName)
        primaryResult = results;

      for (let storedModel of results) {
        allStoredModels.add(storedModel);

        for (let [ groupModelName, groupModels ] of groupedModelMap) {
          if (groupModelName === modelName)
            continue;

          if (!alreadyStored[groupModelName])
            continue;

          let TargetModel = this.getModel(groupModelName);
          if (!TargetModel.isForeignKeyTargetModel(this, modelName))
            continue;

          for (let targetModel of groupModels) {
            let hasChanges = Utils.setRelationalValues(this, TargetModel, targetModel, GroupModel, storedModel);
            if (hasChanges)
              allDirtyModels.add(targetModel);
          }
        }
      }

      if (typeof afterCallback === 'function')
        await afterCallback.call(this, GroupModel, results, options, queryGenerator);
    }

    if (allDirtyModels.size > 0 && typeof afterOperationCallback === 'function')
      await afterOperationCallback.call(this, Model, allDirtyModels, options, queryGenerator);

    // Now mark all models as clean and persisted
    for (let storedModel of allStoredModels) {
      storedModel.clearDirty();
      storedModel._persisted = true;
    }

    return (inputIsArray || !primaryResult) ? primaryResult : primaryResult[0];
  }

  /// Mark all models provided as "persisted".
  ///
  /// Every Mythix ORM model instance has a non-enumerable property `_persisted`.
  /// If this is `true`, then Mythix ORM will treat the model as persisted.
  ///
  /// This method iterates all provided models, and marks each as persisted
  /// by setting this `_persisted` property to `true`.
  /// For each model iterated, this method checks if the instance property
  /// `_mythixModelInstance` is `true`. If this is not the case, then that
  /// instance (whatever it is) will be silently skipped.
  ///
  /// Return: undefined
  ///   This method modifies `models` directly, and returns nothing.
  ///
  /// Arguments:
  ///   models: Array<Model> | <see name="PreparedModels">ConnectionBase.prepareAllModelsForOperation</see>
  ///     Models to mark as persisted.
  ///   value:
  ///     If `true`, models will be marked as persisted. If `false`, models will be marked
  ///     as not-persisted.
  setPersisted(_models, value) {
    let models = _models;
    if (models._mythixPreparedModels)
      models = models.models;

    if (Nife.isEmpty(models))
      return;

    for (let i = 0, il = models.length; i < il; i++) {
      let model = models[i];
      if (!model || !model._mythixModelInstance)
        continue;

      model._persisted = value;
    }
  }

  /// Start this connection.
  ///
  /// The default implementation will throw an exception.
  ///
  /// Every connection is expected to overload this
  /// and provide connection specific startup code
  /// (such as connecting to the database).
  ///
  /// Return: Promise<void>
  async start() {
    throw new Error(`${this.constructor.name}::start: Child class is required to implement "start".`);
  }

  /// Stop (shutdown) this connection.
  ///
  /// The default implementation will throw an exception.
  ///
  /// Every connection is expected to overload this
  /// and provide connection specific shutdown code
  /// (such as disconnecting from the database).
  ///
  /// Return: Promise<void>
  async stop() {
    this.removeAllListeners();
  }

  /// Run model hooks by name.
  ///
  /// This method will run the hooks for a model instance.
  /// `operationHookName` will be one of `onBeforeCreate`, `onBeforeUpdate`,
  /// `onAfterCreate`, or `onAfterUpdate`.
  ///
  /// `operationHookName` will be one of `onBeforeSave`, or `onAfterSave`.
  /// The idea is that any of the `onBefore*` operations will always be
  /// followed by an `onBeforeSave`, whereas the `onAfter*` operations
  /// are all followed by an `onAfterSave`.
  ///
  /// Note:
  ///   Hooks for models are run serially for a given model, but all model
  ///   hooks are run in parallel across all models.
  ///
  /// Note:
  ///   The `onValidate` hook is called from the `onBeforeSave` hook itself.
  ///   This is so that the user can decide if model validations should be
  ///   ran or not (simply by calling `this.onValidate` themselves... or not).
  ///   **If you overload the `onBeforeSave` method, make certain you call
  ///   `super.onBeforeSave.apply(this, arguments)` (or `this.onValidate.apply(this, arguments)`
  ///   directly), because if you don't
  ///   your model validations will be skipped.**
  ///
  /// Return: Promise<Array<any>>
  ///   The result of all model hooks. Mythix ORM ignores the return value
  ///   from model hooks, but it collects them and returns them for the user.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class we are running hooks for. All `models` provided should
  ///     be instances of this class.
  ///   models: Array<<see>Model</see>>
  ///     All model instances to run hooks on. These should all be instances of the
  ///     provided `Model` class.
  ///   operationHookName: `'onBeforeCreate'` | `'onBeforeUpdate'` | `'onAfterCreate'` | `'onAfterUpdate'`
  ///     The hook to call (if not skipped by the `skipHooks` option).
  ///   saveHookName: `'onBeforeSave'` | `'onAfterSave'`
  ///     The name of the save hook to call after the operation hook
  ///     has completed (if not skipped by the `skipHooks` option).
  ///   options: object
  ///     Options for the operation being completed. For example, if this is an
  ///     `insert` operation, then these "options" will be the options for the
  ///     `insert` operation. One other useful option that can be supplied here
  ///     is the `skipHooks: boolean | { [key: 'onBeforeCreate' | 'onBeforeUpdate' | 'onAfterCreate' | 'onAfterUpdate' | 'onBeforeSave' | 'onAfterSave' ]: boolean; }` option.
  ///     If this is `true`, then all hooks will be bypassed (not called).
  ///     If this is an object, then each key should be a hook name, and if it has
  ///     a `true` value, then that specific hook will be bypassed (not called).
  ///
  ///     This allows the caller of an operation such as `insert`, or `update` to
  ///     request that specific hooks not be called.
  async runSaveHooks(Model, models, operationHookName, saveHookName, _options) {
    const throwError = (error) => {
      throw error;
    };

    let options   = _options || {};
    let promises  = [];
    let context   = { connection: this, Model, options, self: null };
    let skipHooks = options.skipHooks;

    const shouldSkipHook = (hookName) => {
      if (!skipHooks)
        return false;

      if (skipHooks === true)
        return true;

      if (skipHooks[hookName] === true)
        return true;

      return false;
    };

    for (let i = 0, il = models.length; i < il; i++) {
      let model         = models[i];
      let modelContext  = { ...context, self: model };

      let promise = (shouldSkipHook(operationHookName)) ? undefined : model[operationHookName](modelContext);
      if (!Nife.instanceOf(promise, 'promise'))
        promise = Promise.resolve(promise);

      if (!shouldSkipHook(saveHookName)) {
        promise = promise.then(async () => await model[saveHookName](modelContext), throwError);
        if (!Nife.instanceOf(promise, 'promise'))
          promise = Promise.resolve(promise);
      }

      promises.push(promise);
    }

    await Promise.all(promises);
  }

  /// Drop a table/bucket from the database.
  ///
  /// This uses the provided `Model` class to
  /// find the table/bucket name to drop, and then
  /// will drop it from the underlying database.
  ///
  /// The `options` argument is database specific,
  /// but might contain options such as `ifExists`,
  /// or `cascade`, for example.
  ///
  /// Return: any
  ///   A database specific return value for the drop table
  ///   operation.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model to drop from the database. The method <see name="Model.getTableName">Model.static getTableName</see>
  ///     is called on the model class to figure out what table/bucket to
  ///     drop from the database.
  ///   options?: object
  ///     Database specific operations for dropping the table/bucket. Please
  ///     refer to the documentation of the driver you are using for further
  ///     information.
  // eslint-disable-next-line no-unused-vars
  async dropTable(Model, options) {
    throw new Error(`${this.constructor.name}::dropTable: This operation is not supported for this connection type.`);
  }

  /// Drop all specified tables/buckets from the database.
  ///
  /// This uses the provided `Models` classes to
  /// find the table/bucket names to drop, and then
  /// will drop all of them from the underlying database.
  ///
  /// The `options` argument is database specific,
  /// but might contain options such as `ifExists`,
  /// or `cascade`, for example.
  ///
  /// The model classes provided are first sorted in
  /// "creation order" using the <see>Utils.sortModelNamesByCreationOrder</see>
  /// method, and then the tables/buckets are dropped in the
  /// reverse order. This is to ensure that any foreign key
  /// constraints in play will play nicely with the operation
  /// and not throw errors.
  ///
  /// This method simply calls <see>ConnectionBase.dropTable</see> for every
  /// model provided--after sorting the models based on their
  /// foreign keys.
  ///
  /// Return: any
  ///   A database specific return value for the drop tables
  ///   operation.
  ///
  /// Arguments:
  ///   Models: Array<class <see>Model</see>>
  ///     All the models to drop from the database. The method <see name="Model.getTableName">Model.static getTableName</see>
  ///     is called on each model class to figure out what table/bucket to
  ///     drop from the database.
  ///   options?: object
  ///     Database specific operations for dropping the table/bucket. Please
  ///     refer to the documentation of the driver you are using for further
  ///     information.
  async dropTables(_Models, options) {
    if (!_Models)
      return;

    // First we collect all models and put them into a map
    let modelMap = _Models;

    if (Nife.instanceOf(_Models, 'array', 'function')) {
      modelMap = {};

      let Models = Nife.toArray(_Models).filter(Boolean);
      for (let i = 0, il = Models.length; i < il; i++) {
        let Model     = Models[i];
        let modelName = Model.getModelName();

        modelMap[modelName] = Model;
      }
    }

    // Second we sort the model names in creation order,
    // and going in reverse of that order we destroy
    // each table.
    let modelNames        = Object.keys(modelMap);
    let sortedModelNames  = Utils.sortModelNamesByCreationOrder(this, modelNames);
    let results           = [];

    for (let i = sortedModelNames.length - 1; i >= 0; i--) {
      let modelName = sortedModelNames[i];
      let Model     = modelMap[modelName];

      results.push(await this.dropTable(Model, options));
    }

    return results;
  }

  /// Create a table/bucket using the provided model class.
  ///
  /// The provided `options` are database specific,
  /// but might contain things like `ifExists`, for
  /// example.
  ///
  /// Return: any
  ///   A connection specific return value for the operation.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class used to create the table/bucket. The <see name="Model.getTableName">Model.static getTableName</see>
  ///     method will be called to get the table/bucket name to create. Then
  ///     the `static fields` property on the model class is used to create the
  ///     columns/fields for the table/bucket. Only "concrete" fields are created
  ///     in the underlying database. Any "virtual" or "relational" fields will
  ///     be skipped.
  ///   options?: object
  ///     Database specific operations for creating the table/bucket. Please
  ///     refer to the documentation of the driver you are using for further
  ///     information.
  async createTable(Model, options) {
    throw new Error(`${this.constructor.name}::createTable: This operation is not supported for this connection type.`);
  }

  /// Create all specified tables/buckets in the database.
  ///
  /// This uses the provided `Models` classes to
  /// create the tables/buckets specified.
  ///
  /// The `options` argument is database specific,
  /// but might contain options such as `ifNotExists`,
  /// for example.
  ///
  /// The model classes provided are first sorted in
  /// "creation order" using the <see>Utils.sortModelNamesByCreationOrder</see>
  /// method, and then the tables/buckets are created in the
  /// that order. This is to ensure that any foreign key
  /// constraints in play will play nicely with the operation
  /// and not throw errors.
  ///
  /// This method simply calls <see>ConnectionBase.createTable</see> for every
  /// model provided--after sorting the models based on their
  /// foreign keys.
  ///
  /// Return: any
  ///   A database specific return value for the create tables
  ///   operation.
  ///
  /// Arguments:
  ///   Models: Array<class <see>Model</see>>
  ///     The model classes used to create the tables/buckets. The <see name="Model.getTableName">Model.static getTableName</see>
  ///     method will be called for each model to get the table/bucket name to create. Then
  ///     the `static fields` property on each model class is used to create the
  ///     columns/fields for the table/bucket. Only "concrete" fields are created
  ///     in the underlying database. Any "virtual" or "relational" fields will
  ///     be skipped.
  ///   options?: object
  ///     Database specific operations for dropping the table/bucket. Please
  ///     refer to the documentation of the driver you are using for further
  ///     information.
  async createTables(_Models, options) {
    if (!_Models)
      return;

    // First we collect all models and put them into a map
    let modelMap = _Models;

    if (Nife.instanceOf(_Models, 'array', 'function')) {
      modelMap = {};

      let Models = Nife.toArray(_Models).filter(Boolean);
      for (let i = 0, il = Models.length; i < il; i++) {
        let Model     = Models[i];
        let modelName = Model.getModelName();

        modelMap[modelName] = Model;
      }
    }

    // Second we sort the model names in creation order,
    // and then create the tables in that order
    let modelNames        = Object.keys(modelMap);
    let sortedModelNames  = Utils.sortModelNamesByCreationOrder(this, modelNames);
    let results           = [];

    for (let i = 0, il = sortedModelNames.length; i < il; i++) {
      let modelName = sortedModelNames[i];
      let Model     = modelMap[modelName];

      results.push(await this.createTable(Model, options));
    }

    return results;
  }

  // Define operations

  async defineTable() {
    throw new Error(`${this.constructor.name}::defineTable: This operation is not supported for this connection type.`);
  }

  async defineConstraints() {
    throw new Error(`${this.constructor.name}::defineConstraints: This operation is not supported for this connection type.`);
  }

  async defineIndexes() {
    throw new Error(`${this.constructor.name}::defineIndexes: This operation is not supported for this connection type.`);
  }

  // Alter operations

  /// Alter a table/bucket based on the provided attributes for
  /// the model.
  ///
  /// For SQL based drivers this might run a statement like the following
  /// `ALTER TABLE "users" RENAME TO "old_users";`
  ///
  /// Please refer to the documentation of the database driver
  /// you are using for more information.
  ///
  /// Return: Promise<void>
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model to alter. This is used to alter the underlying
  ///     table/bucket in the database.
  ///   newAttributes: object
  ///     The attributes to alter. Please refer to the documentation
  ///     of the database driver you are using for more information.
  async alterTable(Model, newModelAttributes, options) {
    throw new Error(`${this.constructor.name}::renameTable: This operation is not supported for this connection type.`);
  }

  /// Drop the specified column/field from the database.
  ///
  /// The table/bucket to drop the field from is known
  /// by the `Model` property (model class) on the field
  /// itself.
  ///
  /// The provided `options` are specific to the database
  /// you are using.
  /// Please refer to the documentation of the database driver
  /// you are using for more information.
  ///
  /// Return: any
  ///   A database specific return value for the operation completed.
  ///
  /// Arguments:
  ///   Field: <see>Field</see>
  ///     The column/field to drop from the database.
  /// options?: object
  ///   Database specific option for the operation. Please refer to the
  ///   documentation of the database driver you are using for more information.
  async dropColumn(Field, options) {
    throw new Error(`${this.constructor.name}::dropColumn: This operation is not supported for this connection type.`);
  }

  /// Alter the specified column/field in the database.
  ///
  /// This will take the two fields, `Field` and `NewField`,
  /// and will compare them. It will generate multiple alter
  /// table statements internally, and will alter the column/field
  /// based on the differences it detects between the two fields.
  ///
  /// If `NewField` is provided as a raw object, then it will be
  /// converted into a <see>Field</see>.
  ///
  /// This method will check for the following differences between
  /// the two fields, in this order:
  /// 1. `allowNull`
  /// 2. `type`
  /// 3. `defaultValue`
  /// 4. `primaryKey`
  /// 5. `unique`
  /// 6. `index` (will calculate index differences, and do the minimum work required)
  /// 7. `columnName`
  ///
  /// Return: Promise<void>
  ///
  /// Arguments:
  ///   Field: <see>Field</see>
  ///     The current column/field (as it is in the database) that we are changing.
  ///   NewField: <see>Field</see> | object
  ///     The new field properties to compare. Only the provided properties will be compared.
  ///     For example, if you only supply a `defaultValue` property, then only that will be
  ///     altered (if it differs from `Field`).
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  async alterColumn(Field, newFieldAttributes, options) {
    throw new Error(`${this.constructor.name}::alterColumn: This operation is not supported for this connection type.`);
  }

  /// Add the column/field specified to the database.
  ///
  /// The table/bucket to add the field to is fetched from the
  /// `Model` property on the supplied field.
  ///
  /// Return: Promise<void>
  ///
  /// Arguments:
  ///   Field: <see>Field</see>
  ///     The new field to add to the underlying database.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  async addColumn(Field, options) {
    throw new Error(`${this.constructor.name}::addColumn: This operation is not supported for this connection type.`);
  }

  /// Create an index (or combo index) in the database.
  ///
  /// This will create a new index for the field(s) specified.
  /// The `indexFields` argument must be an array of field names
  /// as strings. It can contain more than one field. If it does
  /// contain more than one field, then a combo index will be created
  /// for all specified fields (if the database you are using supports
  /// combined indexes).
  ///
  /// All the provided field names must exist on the provided `Model`.
  /// If they don't, then an exception will be thrown. The field names
  /// can be fully qualified, but they don't need to be. If they are
  /// fully qualified, then they must all still be owned by the provided
  /// `Model`. You can not for example use a fully qualified field name
  /// from another model.
  ///
  /// Combo indexes are created by combining two or more fields to create
  /// the index. For example, you could create a combo index for Users
  /// like `[ 'firstName', 'lastName', 'email' ]` if it is common for your
  /// application to query on all three of these fields at once.
  ///
  /// If you just want to index a single column/field, simply provide only
  /// one field name, i.e. `[ 'firstName' ]`.
  ///
  /// Return: Promise<void>
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model that owns the specified fields.
  ///   indexFields: Array<string>
  ///     The field names to use to create the index. One field name
  ///     is valid if you only wish to index a single field. These are
  ///     used to generate the index name, along with which fields to
  ///     index.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  async addIndex(Model, indexFields, options) {
    throw new Error(`${this.constructor.name}::addIndex: This operation is not supported for this connection type.`);
  }

  /// Drop the index from the database based on the specified fields.
  ///
  /// This is the exact inverse of <see>ConnectionBase.addIndex</see>,
  /// and it functions nearly identically, except that it will drop
  /// the specified index instead of creating it.
  ///
  /// Return: Promise<void>
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model that owns the specified fields.
  ///   indexFields: Array<string>
  ///     The field names to used to drop the index. These are used
  ///     to generate the index name, which will then be dropped.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  async dropIndex(Model, indexFields, options) {
    throw new Error(`${this.constructor.name}::addIndex: This operation is not supported for this connection type.`);
  }

  /// Insert the specified models into the specified
  /// table/bucket (based on the provided `Model`).
  ///
  /// This will insert one or more models into the database.
  /// Like nearly all such Mythix ORM methods, bulk operations are supported
  /// out-of-the-box. You can provide a single model to `models`,
  /// or you can provide an array of models.
  ///
  /// The provided "models" can be either an object, an array of
  /// objects, a model instance, or an array of model instances,
  /// or a mix of both. Mythix ORM will ensure any provided raw
  /// objects are first converted into model instances using the
  /// provided `Model` before it inserts anything.
  ///
  /// You can also supply "sub models", and those will also be
  /// inserted in the correct order (and any foreign keys will
  /// also be updated for you). For example, if you have a `User`
  /// model that has a virtual `Type.Model` `primaryRole` field,
  /// then you can supply a new `Role` model upon insertion and
  /// Mythix ORM will handle this properly for you. For example:
  /// `await connection.insert(User, new User({ primaryRole: new Role({ ... }) }))`.
  /// This type of sub model save also works on through-tables.
  /// If `primaryRole` was targeting a `Role` model, but through
  /// another table(s), then Mythix ORM will also create the through-table
  /// relationships (if it is able to).
  ///
  /// This **will not work** for `Types.Models` (multi-relations).
  /// Mythix ORM doesn't know what you intend for multi-relations
  /// (overwrite the set? add to the set? what?) so it will deliberately
  /// skip multi-relational fields. "sub models on insert" only work for
  /// single-relation fields defined with `Types.Model`. For multi-relation
  /// fields you must manually work through the relation yourself. For example,
  /// if our user instead had a `Types.Models` `roles` field (plural), then you
  /// would instead need to:
  ///
  /// Example:
  ///   let user = await connection.insert(User, new User({ ... }));
  ///   let role = await connection.insert(Role, { ... }); // model instance is not required
  ///   await user.addToRoles(role);
  ///
  ///
  /// Return: Promise<Array<Model> | Model>
  ///   If you provide an array of models, then an array of models will be
  ///   returned. If you provide only a single model, then a single
  ///   model will be returned. If you provided "sub models" then those
  ///   will be returned as "related models" on the primary model. For example,
  ///   using the above `User` example, the newly created `Role` model that was
  ///   stored for the `primaryRole` would be available on the returned `User`
  ///   model as `user.Roles[0]`, or you could also access it via the field you
  ///   set it on `user.primaryRole`.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class used for the operation. This defines what
  ///     table/bucket to insert the specified models into.
  ///   models: Array<Model | object> | object | <see>Model</see>
  ///     The model(s) to insert into the database. If raw objects are provided,
  ///     then the properties of each object must match the required attributes
  ///     for the model class.
  ///   options?: object
  ///     Most of these options are database/driver specific. However, the following
  ///     options are common across all database drivers:
  ///     | Option | Type | Default Value | Description |
  ///     | ------------- | ---- | ------------- | ----------- |
  ///     | `skipHooks` | `boolean` &#124; `object` | `undefined` | Skip specific hooks. See <see>ConnectionBase.runSaveHooks</see> for more information. |
  ///     | `batchSize` | `number` | `500` | The size of each batch during a multi-model insert operation. |
  // eslint-disable-next-line no-unused-vars
  async insert(Model, models, _options) {
    throw new Error(`${this.constructor.name}::insert: This operation is not supported for this connection type.`);
  }

  /// Insert or update (upsert) models into the database.
  ///
  /// This method is only supported by some databases. Database
  /// drivers that don't support `upsert` natively may attempt to emulate
  /// the operation (at the cost of speed).
  ///
  /// This method should function identically to <see>ConnectionBase.insert</see>,
  /// with the exception that it should update rows that already exist in the database
  /// instead of inserting new rows.
  ///
  /// See: ConnectionBase.insert
  ///
  /// Return: Promise<Array<Model> | Model>
  ///   If you provide an array of models, then an array of models will be
  ///   returned. If you provide only a single model, then a single
  ///   model will be returned. If you provided "sub models" then those
  ///   will be returned as "related models" on the primary model. For example,
  ///   using the above `User` example, the newly created `Role` model that was
  ///   stored for the `primaryRole` would be available on the returned `User`
  ///   model as `user.Roles[0]`, or you could also access it via the field you
  ///   set it on `user.primaryRole`.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class used for the operation. This defines what
  ///     table/bucket to insert the specified models into.
  ///   models: Array<Model | object> | object | <see>Model</see>
  ///     The model(s) to insert into the database. If raw objects are provided,
  ///     then the properties of each object must match the required attributes
  ///     for the model class.
  ///   options?: object
  ///     Most of these options are database/driver specific. However, the following
  ///     options are common across all database drivers:
  ///     | Option | Type | Default Value | Description |
  ///     | ------------- | ---- | ------------- | ----------- |
  ///     | `skipHooks` | `boolean` &#124; `object` | `undefined` | Skip specific hooks. See <see>ConnectionBase.runSaveHooks</see> for more information. |
  ///     | `batchSize` | `number` | `500` | The size of each batch during a multi-model upsert operation. |
  // eslint-disable-next-line no-unused-vars
  async upsert(Model, models, _options) {
    throw new Error(`${this.constructor.name}::upsert: This operation is not supported for this connection type.`);
  }

  /// Update the specified models in the database.
  ///
  /// Many databases don't have good (or even decent) support
  /// for bulk updates, so unfortunately this method is fairly
  /// slow, and will usually make a query to the database for each
  /// model updated.
  ///
  /// If you want to update many models at the same time (using the same
  /// attributes across all models), then consider using the <see>ConnectionBase.updateAll</see>
  /// method instead.
  ///
  /// Note:
  ///   Models will only be updated if they are dirty. Also, only the
  ///   dirty attributes for each model will be updated (some attributes
  ///   are always dirty, for example `updatedAt` fields are forced to
  ///   always be dirty based on the configuration of their `defaultValue`).
  ///
  /// Return: Promise<Array<Model> | Model>
  ///   If you provide an array of models, then an array of models will be
  ///   returned. If you provide only a single model, then a single
  ///   model will be returned.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class used for the operation. This defines what
  ///     table/bucket to update.
  ///   models: Array<Model | object> | object | <see>Model</see>
  ///     The model(s) to update in the database. If raw objects are provided,
  ///     then the properties of each object must match the required attributes
  ///     for the model class.
  ///   options?: object
  ///     Most of these options are database/driver specific. However, the following
  ///     options are common across all database drivers:
  ///     | Option | Type | Default Value | Description |
  ///     | ------------- | ---- | ------------- | ----------- |
  ///     | `skipHooks` | `boolean` &#124; `object` | `undefined` | Skip specific hooks. See <see>ConnectionBase.runSaveHooks</see> for more information. |
  ///     | `batchSize` | `number` | `500` | The size of each batch during a multi-model update operation. |
  // eslint-disable-next-line no-unused-vars
  async update(Model, models, _options) {
    throw new Error(`${this.constructor.name}::update: This operation is not supported for this connection type.`);
  }

  /// Update multiple models at the same time (bulk update).
  ///
  /// This will update multiple models at the same time
  /// using the provided `query` to select which models to update.
  /// All matching rows will set the provided `attributes` upon them.
  ///
  /// The provided `attributes` can be a model instance, or
  /// a raw object. If a raw object is provided, then they
  /// will be converted into a model instance using the provided
  /// `Model` class. This also means that you can *only* bulk update
  /// columns/fields that exist on the model itself (i.e. you might have
  /// other columns in your table not related to this model, and
  /// those can **not** be updated using this method).
  ///
  /// Note:
  ///   As always with Mythix ORM, you will **never** supply
  ///   raw column names as the `attributes`. You must always
  ///   provide model field names in Mythix ORM.
  ///
  /// Note:
  ///   This will be an update operation across all matching rows,
  ///   using the data provided. This method is really only useful
  ///   when you want to update multiple rows to the **same values**.
  ///   If you need to update each row to different values per-row,
  ///   then use the <see>ConnectionBase.update</see> method instead.
  ///
  /// Return: Promise<any>
  ///   A database specific result from the `UPDATE` statement.
  ///   In the future all "database specific" results will be
  ///   abstracted away. So in the future, this will likely return
  ///   the number of rows updated as a `number` (**HELP WANTED**).
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query used to select which models/rows to update.
  ///     The "root model" of the query is the table/bucket that
  ///     will be updated.
  ///   attributes: object | Model
  ///     The attributes to set across all updated rows.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  // eslint-disable-next-line no-unused-vars
  async updateAll(_queryEngine, model, _options) {
    throw new Error(`${this.constructor.name}::updateAll: This operation is not supported for this connection type.`);
  }

  /// Destroy the provided models.
  ///
  /// This method will bulk-iterate the specified
  /// models and will destroy each one. A primary key field
  /// is required for every model, or this method will throw
  /// an exception.
  ///
  /// The `skipHooks` option (see <see>ConnectionBase.runSaveHooks</see>) won't
  /// do anything here, because Mythix ORM doesn't have any `on*Destroy` hooks.
  /// Mythix ORM doesn't have any destroy hooks for performance reasons.
  /// However, the `batchSize` option is still useful for this method.
  ///
  /// Note:
  ///   **WARNING!!!**: If you supply `null` or `undefined` as the `models`
  ///   argument then this method will silently return. If you also supply
  ///   the option `{ truncate: true }` then the entire table will be truncated.
  ///   This option is an "opt-in", to make sure you don't truncate an entire
  ///   table on accident.
  ///
  /// Note:
  ///   Some databases (i.e. SQLite) don't support a `TRUNCATE` statement,
  ///   and so will deliberately call this method with `null` for the `models`
  ///   argument for `truncate` operations. When doing so they will also
  ///   deliberately supply the `{ truncate: true }` option.
  ///
  /// Return: Promise<Array<Model> | Model>
  ///   If you provide an array of models, then an array of models will be
  ///   returned. If you provide only a single model, then a single
  ///   model will be returned.
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class used for the operation. This defines what
  ///     table/bucket to delete from.
  ///   models: Array<Model | object> | object | <see>Model</see>
  ///     The model(s) to delete from the database. If raw objects are provided,
  ///     then they must contain a `primaryKey` for each model. If any of the
  ///     models provided don't contain a `primaryKey` field, then an exception
  ///     will be thrown. If you have a table/bucket that has no primary key column,
  ///     then you will need to destroy rows manually yourself using a manual query
  ///     with <see>ConnectionBase.query</see>, or by using the <see>ConnectionBase.destroy</see> method.
  ///   options?: object
  ///     Most of these options are database/driver specific. However, the following
  ///     options are common across all database drivers (`skipHooks` is not supported in this context):
  ///     | Option | Type | Default Value | Description |
  ///     | ------------- | ---- | ------------- | ----------- |
  ///     | `batchSize` | `number` | `500` | The size of each batch during a multi-model destroy operation. |
  // eslint-disable-next-line no-unused-vars
  async destroyModels(Model, _models, _options) {
    throw new Error(`${this.constructor.name}::destroyModels: This operation is not supported for this connection type.`);
  }

  /// Destroy multiple models by query, or by the
  /// provided models themselves. If models are provided,
  /// then each model must have a valid primary key field,
  /// or an exception will be thrown.
  ///
  /// If models are provided as the second argument, then
  /// it is required that the first argument be a <see>Model</see>
  /// class. In this case, this method simply calls
  /// <see>ConnectionBase.destroyModels</see> to complete the operation.
  ///
  /// If the first argument is a <see>QueryEngine</see> instance,
  /// then rows will be deleted from the database using the provided query.
  /// In this case, it is expected that the second argument
  /// will be the "options" for this operation instead of an array of models.
  ///
  /// Return: Promise<Array<Models> | Model | number>
  ///   Return the models deleted (if models were provided),
  ///   or the number of rows deleted (if a query was provided).
  ///
  /// Arguments:
  ///   queryOrModel: class <see>Model</see> | <see>QueryEngine</see>
  ///     A <see>QueryEngine</see> instance to specify which models to
  ///     delete via a query, or a <see>Model</see> class if models are
  ///     being provided to be deleted.
  ///   modelsOrOptions: Array<Model> | Model | object
  ///     If a <see>QueryEngine</see> instance is provided as the first argument
  ///     then this is expected to be the "options" for the operation. If however
  ///     a <see>Model</see> class is provided as the first argument, then this
  ///     should be a <see>Model</see> instance, or an array of <see>Model</see> instances.
  ///   options?: object | undefined
  ///     Most of these options are database/driver specific.
  ///     The `batchSize` option is ignored if a <see>QueryEngine</see> instance is provided as the
  ///     first argument to the call. If a <see>Model</see> class is provided as the first argument to
  ///     the call, then this `options` object will be at argument index `1` instead (the second argument, `modelsOrOptions`).
  ///     The following options are common across all database drivers
  ///     (`skipHooks` is not supported in this context):
  ///     | Option | Type | Default Value | Description |
  ///     | ------------- | ---- | ------------- | ----------- |
  ///     | `batchSize` | `number` | `500` | The size of each batch during a multi-model destroy operation. |
  // eslint-disable-next-line no-unused-vars
  async destroy(_queryEngineOrModel, modelsOrOptions, _options) {
    throw new Error(`${this.constructor.name}::destroy: This operation is not supported for this connection type.`);
  }

  /// Select data from the underlying database.
  ///
  /// To select data from the database you will use this method,
  /// providing a `query` as a <see>QueryEngine</see> instance.
  /// The provided query will be used to generate the underlying
  /// request to the database, will collect the response, and return
  /// the results to the caller.
  ///
  /// This method is an async generator method, and is designed for
  /// "streaming" results from the database, one "row" at a time. Many
  /// methods that call this method (such as <see>QueryEngine.all</see>)
  /// collect all the results from the async generator to return an array
  /// of results to the caller. This is because it is often tedious to
  /// collect the results yourself from the async generator, and often the
  /// caller simply wants a small amount of data from the database.
  /// However, if you intend to fetch large amounts of data from your database,
  /// it is a good idea to call this method directly, and iterate the results
  /// of the async generator manually. All methods that in-turn call this
  /// method (such as <see>QueryEngine.all</see>) will generally have a
  /// `{ stream: true }` option that can be provided, causing the method to
  /// return the async generator, instead of the collected results.
  ///
  ///
  /// Return: AsyncGenerator<Model>
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query to run against the underlying database. This is generated
  ///     from a <see>QueryEngine</see> interface. The result of this <see>QueryEngine</see>
  ///     instance will then be converted to a query, or generated code to
  ///     interact with the underlying database.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  // eslint-disable-next-line no-unused-vars, require-yield
  async *select(_queryEngine, _options) {
    throw new Error(`${this.constructor.name}::select: This operation is not supported for this connection type.`);
  }

  /// Aggregate data across rows.
  ///
  /// Though this method can be called directly, it is generally called from
  /// one of <see>ConnectionBase.average</see>, <see>ConnectionBase.sum</see>,
  /// <see>ConnectionBase.count</see>, <see>ConnectionBase.min</see>,
  /// or <see>ConnectionBase.max</see>, or one of the other aggregate methods
  /// provided by the connection.
  ///
  /// It takes a `query` which is used to generate a query for the underlying
  /// database, and sets the query `PROJECTION` to the aggregate `literal` provided.
  /// For example, a `sum` method call would call this method with a `SumLiteral`,
  /// which would change the `query` projection to the expanded result of the
  /// `SumLiteral`, returning the "sum" of all column values targeted by the literal field.
  /// The results will then be collected, and always returned as a `number` primitive
  /// to the caller.
  ///
  /// Return: Promise<number>
  ///   The result of the aggregate operation, as a number.
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query used to select which rows to aggregate across.
  ///   literal: <see>LiteralBase</see>
  ///     A literal, used as the aggregate function. For example, if a
  ///     <see>CountLiteral</see> is provided, then the count of all rows
  ///     matching the provided query will be the result.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  // eslint-disable-next-line no-unused-vars
  async aggregate(_queryEngine, _literal, options) {
    throw new Error(`${this.constructor.name}::aggregate: This operation is not supported for this connection type.`);
  }

  /// Get the average for a single column, spanning all matching rows.
  ///
  /// This will return the average of all values in a column,
  /// across all matching rows, as a `number` primitive.
  ///
  /// Return: Promise<number>
  ///   The average of all matching columns.
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query used to select which rows are used to calculate the average.
  ///   field: <see>Field</see> | string
  ///     A field instance, or a fully qualified field name, used as the target
  ///     column in the underlying database to calculate an average across all matching values.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  // eslint-disable-next-line no-unused-vars
  async average(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::average: This operation is not supported for this connection type.`);
  }

  /// Get the number of rows matching the query.
  ///
  /// This will return the number of rows matching
  /// the provided query, as a `number` primitive.
  ///
  /// Note:
  ///   In most databases, if the `field` argument is not
  ///   specified, then the count operation will be across
  ///   all table columns (`COUNT(*)`).
  ///
  /// Return: Promise<number>
  ///   The count (number) of all rows matching the provided query.
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query used to select which rows to count.
  ///   field: <see>Field</see> | string
  ///     A field instance, or a fully qualified field name, used as the target
  ///     column in the underlying database to count the rows. If not specified,
  ///     then most database drivers will count across all columns (i.e. `COUNT(*)`).
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  // eslint-disable-next-line no-unused-vars
  async count(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::count: This operation is not supported for this connection type.`);
  }

  /// Get the minimum value for a column, spanning all matching rows.
  ///
  /// This will return the minimum of all values in a column,
  /// across all matching rows, as a `number` primitive.
  ///
  /// Return: Promise<number>
  ///   The minimum value found across all matching rows.
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query used to select which rows are used to calculate the minimum.
  ///   field: <see>Field</see> | string
  ///     A field instance, or a fully qualified field name, used as the target
  ///     column in the underlying database to find the minimum across all matching values.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  // eslint-disable-next-line no-unused-vars
  async min(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::min: This operation is not supported for this connection type.`);
  }

  /// Get the maximum value for a column, spanning all matching rows.
  ///
  /// This will return the maximum of all values in a column,
  /// across all matching rows, as a `number` primitive.
  ///
  /// Return: Promise<number>
  ///   The maximum value found across all matching rows.
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query used to select which rows are used to calculate the maximum.
  ///   field: <see>Field</see> | string
  ///     A field instance, or a fully qualified field name, used as the target
  ///     column in the underlying database to find the maximum across all matching values.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  // eslint-disable-next-line no-unused-vars
  async max(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::max: This operation is not supported for this connection type.`);
  }

  /// Get the sum of all values for a column, spanning all matching rows.
  ///
  /// This will return the sum of all values in a column,
  /// across all matching rows, as a `number` primitive.
  ///
  /// Return: Promise<number>
  ///   The sum of all values found across all matching rows.
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query used to select which rows are used to calculate the sum.
  ///   field: <see>Field</see> | string
  ///     A field instance, or a fully qualified field name, used as the target
  ///     column in the underlying database to find the sum of all matching values.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  // eslint-disable-next-line no-unused-vars
  async sum(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::sum: This operation is not supported for this connection type.`);
  }

  /// Pluck only specific columns/fields from the
  /// underlying database, using the provided `query`.
  ///
  /// This method will return only the specified `fields`
  /// from the database--as raw values--from rows matched
  /// on by the provided query.
  ///
  /// This method is often much faster than a normal `select` operation,
  /// in that only the data requested will be transmitted from the database,
  /// and models won't be constructed and stitched together on load. Use this
  /// when you just need an "array of values" across certain columns from a table.
  ///
  /// Return: Array<any> | Array<Array<any>> | Array<object>
  ///   If the provided `fields` argument is an array of fields
  ///   (fully qualified field names, or <see>Field</see> instances),
  ///   then the result will be an array of arrays, where each item
  ///   in the top-level array is a "row", and each sub-array is the
  ///   values for the columns (fields) specified. If a single field
  ///   is specified, then an one dimensional array is returned, where
  ///   each item is the column value for each row fetched.
  ///   For example, if we call `pluck` like: `let result = await connection.pluck(User.where.firstName.EQ('Bob'), 'User:id')`
  ///   then the `result` would look like `[ 'USER_id_1`, `USER_id_2`, ... ]`
  ///   where each value in the array is a user id. If however we call pluck like:
  ///   `let result = await connection.pluck(User.where.firstName.EQ('Bob'), [ 'User:id', 'User:firstName' ])`
  ///   then the `result` would look like `[ [ 'USER_id_1', 'Bob' ], [ 'USER_id_2', 'Bob' ], ... ]`, because
  ///   the provided `fields` is an array, an array of field values will be returned from each row.
  ///   If the `option` `mapToObjects` is `true`, then an array of objects will be returned,
  ///   where each object is a "row", and each property one of the specified fields. Note that
  ///   the properties of each returned object will be the fully qualified field name of each field
  ///   specified. So, for example, specifying a pluck field of `User:id` means that each returned
  ///   object in the array will look like `[ { 'User:id': 'USER_id_1' }, { 'User:id': 'USER_id_2' }, ... ]`.
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query used to select rows from which columns will be plucked.
  ///   fields: <see>Field</see> | string | Array<Field> | Array<string>
  ///     Which fields to "pluck" from the matching rows. If an array is provided (even if
  ///     it only contains a single field), then an array of arrays will be returned
  ///     as the result. If a single field is provided (not an array), then an array
  ///     of raw plucked column values will be returned instead.
  ///   options?: object
  ///     Operation specified options. These might change based on the database driver you
  ///     are using, so please refer to your specific database driver documentation. One
  ///     option that is common across all drivers is the `mapToObjects` boolean option.
  ///     If `true`, then each row in the returned array will be an object instead of
  ///     raw column values, where the property of each "row object" will be the fully
  ///     qualified names of each field provided as the `fields` argument.
  // eslint-disable-next-line no-unused-vars
  async pluck(_queryEngine, _fields, _options) {
    throw new Error(`${this.constructor.name}::pluck: This operation is not supported for this connection type.`);
  }

  /// Check if any rows match the provided `query`.
  ///
  /// Return: boolean
  ///   `true` if one or more rows match the provided query, or `false` otherwise.
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query used to select rows, to check if said rows exist in the database.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  // eslint-disable-next-line no-unused-vars
  async exists(queryEngine, options) {
    throw new Error(`${this.constructor.name}::exists: This operation is not supported for this connection type.`);
  }

  /// Truncate (erase/clear) the entire table/bucket defined
  /// by the provided `Model`. All rows/objects from the underlying
  /// table/bucket will be destroyed, leaving you with an empty
  /// table/bucket.
  ///
  /// Return: Promise<void>
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The model class that defines the underlying table/bucket to wipe clean/erase.
  ///   options?: object
  ///     Operation specific options. These will change depending on the database
  ///     driver you are using. Please refer to the documentation for your specific
  ///     driver for more information.
  // eslint-disable-next-line no-unused-vars
  async truncate(Model, options) {
    throw new Error(`${this.constructor.name}::truncate: This operation is not supported for this connection type.`);
  }

  /// "raw" database/driver specific query interface.
  ///
  /// For SQL based databases, this would send a direct
  /// query string to the database. For other drivers, the
  /// arguments and operations that are executed might change.
  ///
  /// Use this method to directly interact with the underlying
  /// database, in its own native query language.
  ///
  /// The arguments and return value from this method is database/driver
  /// specific. Any `options` argument the database provides are also
  /// specific to the underlying database driver. However, one option
  /// is common across all drivers, and this is the `logger` option. If
  /// set, it is expected to have a `log` method in the provided `logger`
  /// object. Oftentimes, this will simply be `{ logger: console }`, but
  /// you can provided any custom `logger` instance you want, as long as it
  /// has a `log` method that can be called to log the results of the query.
  /// Most drivers also support a `{ logger }` option as a connection option
  /// when the connection is first instantiated, which will provided logging
  /// to every query that goes through the connection.
  ///
  /// Return: database specific
  ///   A database/driver specific return value, based on the query provided.
  // eslint-disable-next-line no-unused-vars
  async query(sql, options) {
    throw new Error(`${this.constructor.name}::query: This operation is not supported for this connection type.`);
  }

  /// Initiate a transaction (or snapshot/sub-transaction)
  /// if the database driver supports transactions.
  ///
  /// This will initiate a transaction in the underlying database,
  /// if the database supports it. For SQL type databases this would
  /// be a `BEGIN/COMMIT/ROLLBACK` block. If this method is called
  /// when a transaction is already in-progress, then a snapshot/sub-transaction
  /// will be started instead.
  ///
  /// This method will start a transaction in the underlying database,
  /// and call the provided asynchronous callback. If the callback throws
  /// an error, then the transaction (or snapshot) will be automatically
  /// rolled-back. There is a single `connection` argument that will be
  /// provided to the callback function when called. This `connection` argument
  /// will be the transaction connection itself, which for many database drivers is simply
  /// the same connection the transaction was started from. At a lower-level,
  /// Mythix ORM  will use an [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html)
  /// context to provide the transaction connection to all code executed inside
  /// the callback, so the provided `connection` argument can generally be ignored.
  /// However, if [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html) isn't
  /// supported in your environment, or the specific driver you are using requires that you use
  /// the supplied `connection` argument, then you must use the supplied `connection`
  /// for all your operations, and provide it as the `{ connection }` option to all Mythix ORM
  /// calls made inside the callback.
  ///
  /// Return: Promise<any>
  ///   Return whatever return value is returned from the provided callback.
  ///
  /// Arguments:
  ///   callback: (connection: <see>Connection</see>) => any
  ///     The async callback to call for the transaction operation. This should return as quickly as
  ///     possible to avoid deadlocks in the underlying database. Whatever value this method
  ///     returns will be the return value from the `transaction` call itself. If an exception is
  ///     thrown in this method, then the transaction will be automatically rolled-back. If no
  ///     exception is thrown from this method, then when done executing, a `COMMIT` will be sent
  ///     to the underlying database automatically for you.
  ///   options?: object
  ///     Optional database specific options to supply for the transaction operation. There are two
  ///     options that are supported across most database drivers, and those are `connection` and `lock`.
  ///     The `connection` option supplies the the connection to initiate the transaction from, which for
  ///     example might be another transaction connection (initiating a sub-transaction). If no `connection`
  ///     option is supplied, then the `connection` this method was called from is used instead. The second
  ///     common option is `lock`, which specifies how/if to lock the table for the transaction.
  ///     See <see>ConnectionBase.getLockMode</see> for your specific driver for more information on this `lock` option.
  // eslint-disable-next-line no-unused-vars
  async transaction(callback, options) {
    throw new Error(`${this.constructor.name}::transaction: This operation is not supported for this connection type.`);
  }

  /// Translate the `defaultValue` as provided by each models field's
  /// into a `DEFAULT` value for the underlying database.
  ///
  /// For example, if you were to specify a `defaultValue` for one of your
  /// models field's, such as `defaultValue: Types.INTEGER.Defaults.AUTOINCREMENT`,
  /// then this method would return a database specific `autoincrement` default for
  /// the column defined by your field. For date/time fields, this might be a database
  /// specific `NOW()` default. For non-database specific "defaults", the default value
  /// as defined by your `defaultValue` on your fields is simply returned.
  ///
  /// Return: any
  ///   Your defined field's `defaultValue`, intercepted, and possibly translated to
  ///   a proper value for the underlying database. It is common for many database
  ///   drivers that only date/time and auto-increment default values are intercepted
  ///   and translated. Most other "default values" are simply returned.
  ///
  /// Arguments:
  ///   type: <see>Type</see>
  ///     The field's `type` instance, as defined by the field itself.
  ///   context: DefaultValueContext
  ///     A `DefaultValueContext` as is normally provided to all `defaultValue` methods
  ///     defined on <see name="fields">Field</see>. See the documentation for the `defaultValue`
  ///     property of <see>Field</see> for more information.
  // eslint-disable-next-line no-unused-vars
  async getDefaultFieldValue(type, context) {
    throw new Error(`${this.constructor.name}::getDefaultFieldValue: This operation is not supported for this connection type.`);
  }

  /// A connection specific "helper" to force "dirty" fields
  /// for all models.
  ///
  /// This method, when present on a connection, will assist in
  /// defining which fields are "dirty" for a model. It is used,
  /// for example, by `Types.DATETIME.Defaults.NOW.UPDATED` default values.
  /// In the case presented, DATETIME fields that should always be updated
  /// are always marked dirty by this method, even though they might
  /// not actually be "dirty" client-side.
  ///
  /// If this method returns _any_ value other than `undefined`, then
  /// the field being operated upon will be marked "dirty" with the value
  /// returned, even if the field would otherwise not be marked "dirty".
  ///
  /// Return: any
  ///   If _any_ value is returned that is not `undefined`, then the field
  ///   will be marked "dirty" with the value returned. Return `undefined`
  ///   if you do not wish to mark a specific field as "dirty". Note however
  ///   that the field _might_ actually still be dirty because the user modified
  ///   the field, in which case this method will not even be called for that field.
  ///   In short, this method is only called for fields that are not already marked
  ///   as dirty, and will force a field to be marked dirty if any value other than
  ///   `undefined` is returned.
  ///
  /// Arguments:
  ///   context: DefaultValueContext
  ///     See <see>Field</see> for documentation on the `DefaultValueContext` interface.
  // eslint-disable-next-line no-unused-vars
  dirtyFieldHelper(context) {

  }
}

module.exports = ConnectionBase;
