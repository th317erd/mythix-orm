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
  /// be either a model or a query. <see>Connection.destroyAll</see>
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

  getModels() {
    return this._models;
  }

  getModel(modelName) {
    if (typeof modelName === 'symbol')
      return;

    let def = this.parseQualifiedName(modelName);
    return this._models[def.modelName];
  }

  getField(fieldName, modelName) {
    let def = this.parseQualifiedName(fieldName);
    if (def.modelName == null)
      def.modelName = modelName;

    let Model = this.getModel(def.modelName);
    if (!Model)
      return;

    return Model.getField(def.fieldNames[0]);
  }

  getQueryEngineClass() {
    let options = this.getOptions();
    return options.QueryEngine;
  }

  getQueryGenerator() {
    return this.queryGenerator;
  }

  setQueryGenerator(queryGenerator) {
    this.queryGenerator = queryGenerator;
  }

  _escape(value) {
    if (Nife.instanceOf(value, 'string'))
      return `'${value.replace(/'/g, '\'\'')}'`;

    return SqlString.escape(value);
  }

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

  _escapeID(value) {
    let parts = value.replace(/['"`]/g, '').split(/\.+/g);
    return parts.map((part) => SqlString.escapeId(part).replace(/^`/, '"').replace(/`$/, '"')).join('.');
  }

  escapeID(value, options) {
    if (LiteralBase.isLiteral(value))
      return value.toString(this.connection, options);

    return this._escapeID(value);
  }

  _averageLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._averageLiteralToString(literal, options);
  }

  _countLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._countLiteralToString(literal, options);
  }

  _distinctLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._distinctLiteralToString(literal, options);
  }

  _fieldLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._fieldLiteralToString(literal, options);
  }

  _maxLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._maxLiteralToString(literal, options);
  }

  _minLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._minLiteralToString(literal, options);
  }

  _sumLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let queryGenerator = this.getQueryGenerator();
    if (!queryGenerator)
      return;

    return queryGenerator._sumLiteralToString(literal, options);
  }

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

  // eslint-disable-next-line no-unused-vars
  _bigintTypeToString(type) {
    return 'BIGINT';
  }

  // eslint-disable-next-line no-unused-vars
  _blobTypeToString(type) {
    return 'BLOB';
  }

  // eslint-disable-next-line no-unused-vars
  _booleanTypeToString(type) {
    return 'BOOLEAN';
  }

  // eslint-disable-next-line no-unused-vars
  _charTypeToString(type) {
    return 'CHAR';
  }

  // eslint-disable-next-line no-unused-vars
  _dateTypeToString(type) {
    return 'TIMESTAMP';
  }

  // eslint-disable-next-line no-unused-vars
  _datetimeTypeToString(type) {
    return 'TIMESTAMP';
  }

  // eslint-disable-next-line no-unused-vars
  _numericTypeToString(type) {
    return `NUMERIC(${type.precision}, ${type.scale})`;
  }

  // eslint-disable-next-line no-unused-vars
  _realTypeToString(type) {
    return 'FLOAT';
  }

  // eslint-disable-next-line no-unused-vars
  _integerTypeToString(type) {
    return 'INTEGER';
  }

  // eslint-disable-next-line no-unused-vars
  _stringTypeToString(type) {
    return `VARCHAR(${type.length})`;
  }

  // eslint-disable-next-line no-unused-vars
  _textTypeToString(type) {
    return 'TEXT';
  }

  // eslint-disable-next-line no-unused-vars
  _uuidV1TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  // eslint-disable-next-line no-unused-vars
  _uuidV3TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  // eslint-disable-next-line no-unused-vars
  _uuidV4TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  // eslint-disable-next-line no-unused-vars
  _uuidV5TypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

  // eslint-disable-next-line no-unused-vars
  _xidTypeToString(type) {
    return `VARCHAR(${type.getTotalLength()})`;
  }

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

  async start() {
    throw new Error(`${this.constructor.name}::start: Child class is required to implement "start".`);
  }

  async stop() {
    this.removeAllListeners();
  }

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

  async dropTable(Model, options) {
    throw new Error(`${this.constructor.name}::dropTable: This operation is not supported for this connection type.`);
  }

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

  async createTable(Model, options) {
    throw new Error(`${this.constructor.name}::createTable: This operation is not supported for this connection type.`);
  }

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

  async alterTable(Model, newModelAttributes, options) {
    throw new Error(`${this.constructor.name}::renameTable: This operation is not supported for this connection type.`);
  }

  async dropColumn(Field, options) {
    throw new Error(`${this.constructor.name}::dropColumn: This operation is not supported for this connection type.`);
  }

  async alterColumn(Field, newFieldAttributes, options) {
    throw new Error(`${this.constructor.name}::alterColumn: This operation is not supported for this connection type.`);
  }

  async addColumn(Field, options) {
    throw new Error(`${this.constructor.name}::addColumn: This operation is not supported for this connection type.`);
  }

  async addIndex(Model, indexFields, options) {
    throw new Error(`${this.constructor.name}::addIndex: This operation is not supported for this connection type.`);
  }

  async dropIndex(Model, indexFields, options) {
    throw new Error(`${this.constructor.name}::addIndex: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async insert(Model, models, _options) {
    throw new Error(`${this.constructor.name}::insert: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async upsert(Model, models, _options) {
    throw new Error(`${this.constructor.name}::upsert: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async update(Model, models, _options) {
    throw new Error(`${this.constructor.name}::update: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async updateAll(_queryEngine, model, _options) {
    throw new Error(`${this.constructor.name}::updateAll: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async destroyModels(Model, _models, _options) {
    throw new Error(`${this.constructor.name}::destroyModels: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async destroy(_queryEngineOrModel, modelsOrOptions, _options) {
    throw new Error(`${this.constructor.name}::destroy: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars, require-yield
  async *select(_queryEngine, _options) {
    throw new Error(`${this.constructor.name}::select: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async aggregate(_queryEngine, _literal, options) {
    throw new Error(`${this.constructor.name}::aggregate: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async average(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::average: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async count(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::count: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async min(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::min: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async max(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::max: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async sum(_queryEngine, _field, options) {
    throw new Error(`${this.constructor.name}::sum: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async pluck(_queryEngine, _fields, _options) {
    throw new Error(`${this.constructor.name}::pluck: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async exists(queryEngine, options) {
    throw new Error(`${this.constructor.name}::exists: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async truncate(Model, options) {
    throw new Error(`${this.constructor.name}::truncate: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async query(sql, options) {
    throw new Error(`${this.constructor.name}::query: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async transaction(callback, options) {
    throw new Error(`${this.constructor.name}::transaction: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  async getDefaultFieldValue(type, context) {
    throw new Error(`${this.constructor.name}::getDefaultFieldValue: This operation is not supported for this connection type.`);
  }

  // eslint-disable-next-line no-unused-vars
  dirtyFieldHelper(context) {

  }
}

module.exports = ConnectionBase;
