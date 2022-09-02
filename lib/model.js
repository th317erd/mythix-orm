'use strict';

const Nife            = require('nife');
const Inflection      = require('inflection');
const Type            = require('./types/type');
const DefaultHelpers  = require('./types/helpers/default-helpers');
const Field           = require('./field');

/// Used as a unique key for cache.
/// Caches will generally be stored in
/// a Map, and since a Map's keys can
/// be any instance, we use a "CacheKey"
/// object on Models that will change
/// when they are dirty. This will allow
/// downstream libraries to use this "dirty"
/// key as a cache key.
class CacheKey {
  /// A "number" argument is provided here
  /// as a way to get an "id" from a CacheKey.
  /// Another way to view this "number" is
  /// a cache version. This number is incremented
  /// every time a model's fields get dirty.
  ///
  /// Arguments:
  ///   number: number
  ///     An incrementing number, or "version" for the CacheKey
  constructor(number) {
    this.number = (number == null) ? 0 : (number.valueOf() + 1);
  }

  /// Return the "number" or "version"
  /// of this CacheKey.
  ///
  /// Return: number
  valueOf() {
    return this.number;
  }
}

function bindStaticWhereToModelClass(ModelClass) {
  const whereProp = {
    enumberable:  true,
    configurable: true,
    set:          function() {},
    get:          function() {
      const self = this;

      function unboundWhere(_connection, _propName) {
        let connection = _connection;

        if (arguments.length === 0) {
          connection = self._getConnection();
          if (!connection)
            throw new Error(`${self.getModelName()}::where: Model has no bound connection. You need to provide a connection by calling "${self.getModelName()}.where(connection)" instead.`);

          return self.getQueryEngine(connection);
        }

        if (!connection) {
          connection = self._getConnection();
          if (!connection)
            throw new Error(`${self.getModelName()}::where: Model has no bound connection. You need to provide a connection by calling "${self.getModelName()}.where(connection)" instead.`);
        }

        let propName = _propName;

        if (connection.constructor && !connection.constructor._isMythixConnection) {
          connection = self._getConnection();
          if (!connection)
            throw new Error(`${self.getModelName()}::where: Model has no bound connection. You need to provide a connection by calling "${self.getModelName()}.where(connection)" instead.`);

          propName = _connection;
        }

        let queryEngine = self.getQueryEngine(connection, {
          models: {
            [self.getModelName()]: self,
          },
        });

        if (propName)
          return queryEngine[propName];
        else
          return queryEngine;
      }

      return new Proxy(unboundWhere, {
        get: (_, propName) => {
          let connection = self._getConnection();
          if (!connection)
            throw new Error(`${self.getModelName()}::where: Model has no bound connection. You need to provide a connection by calling "${self.getModelName()}.where(connection)" instead.`);

          // This will throw an exception
          // if no connection is available
          let queryEngine = self.getQueryEngine(connection);
          return queryEngine[propName];
        },
      });
    },
  };

  Object.defineProperties(ModelClass, {
    'where':  whereProp,
    '$':      whereProp,
  });
}

/// The base Model class for Mythix ORM.
/// Every Mythix ORM model should inherit
/// from this class. This class provides
/// support for all model operations in
/// Mythix ORM.
///
/// Note:
///   Many model methods have both static and instance
///   methods of the same name that do the same thing.
///   This is because it is common to access Model
///   methods directly on the model class as well as an
///   instance. For example, <see>Model.static getModelName</see> is both
///   a static method, and an instance method. Nearly all
///   the methods listed here have both a `static` and
///   and instance version. We are only listing the `static`
///   versions because the instance versions generally will
///   just be proxies to these `static` methods.
/// Note:
///   An underscore prefix on a method in Mythix ORM
///   implies that this method should not be overloaded
///   unless you know exactly what you are doing.
///   It also implies that there is another method
///   that can and should be overloaded instead.
class Model {
  /// This property assists with type checking
  ///
  /// Type: boolean
  static _isMythixModel = true;

  /// Use this method to check if a class
  /// is a Mythix ORM model. It will return
  /// `true` if the provided value is a class
  /// that inherits from <see>Model</see>, or
  /// if the provided value has an attribute
  /// named `_isMythixModel` that is truthy.
  ///
  /// Return: boolean
  /// Arguments:
  ///   value: Function
  ///     Value to check.
  static isModelClass(value) {
    if (!value)
      return false;

    if (value.prototype instanceof Model)
      return true;

    if (value._isMythixModel)
      return true;

    return false;
  }

  /// Check to see if the provided value is
  /// an *instance* of a Mythix ORM <see>Model</see>.
  /// Unlike <see>Model.static isModelClass</see>, which
  /// checks if a *class* is a <see>Model</see>, this will check
  /// to see if an *instance* is an instance of a
  /// Mythix ORM <see>Model</see>. It will return
  /// `true` if the provided value is an `instanceof`
  /// <see>Model</see>, or if the value's `constructor`
  /// property has a truthy `_isMythixModel` property
  /// (`value.constructor._isMythixModel`)
  ///
  /// Return: boolean
  /// Arguments:
  ///   value: any
  ///     Value to check
  static isModel(value) {
    if (!value)
      return false;

    if (value instanceof Model)
      return true;

    if (value.constructor && value.constructor._isMythixModel)
      return true;

    return false;
  }

  /// Like everywhere in Javascript, we can
  /// call `.toString()` to a get a string
  /// representation of our <see>Model</see>
  /// *class*. The optional `showFields` argument,
  /// if true, will list the models fields as well.
  /// Without the `showFields` argument, the model
  /// name alone will be returned as a string.
  ///
  /// Return: string
  /// Arguments:
  ///   showFields?: boolean
  ///     If `true`, then list the models fields.
  static toString(showFields) {
    let fieldNames = [];

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      fieldNames.push(`  ${fieldName}: ${field.type.getDisplayName()}`);
    });

    let fieldsStr = (showFields && fieldNames.length) ? `\n${fieldNames.join(',\n')}\n` : '';

    return `[model ${this.getModelName()}] {${fieldsStr}}`;
  }

  // eslint-disable-next-line no-unused-vars
  static [Symbol.for('nodejs.util.inspect.custom')](depth, inspectOptions, inspect) {
    return this.toString((depth !== 0));
  }

  /// Get the underlying connection bound to
  /// the model's class. Connection binding is
  /// optional, so this method can also be provided
  /// a connection. If a connection is provided, then
  /// it will simply be returned. Because Mythix ORM
  /// has no globals, this is a design pattern to
  /// supply a connection if you have one available,
  /// or fallback to a "bound" connection (if one can
  /// be found).
  ///
  /// Return: <see>Connection</see>
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection that can be provided.
  ///     If provided, it will simply be returned.
  ///     Otherwise, if not provided, or a falsy value,
  ///     then attempt to fallback to the bound connection,
  ///     if any is available.
  static _getConnection(_connection) {
    return _connection || this._mythixBoundConnection;
  }

  /// See <see>Model.static _getConnection</see>
  ///
  /// Return: <see>Connection</see>
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection that can be provided.
  ///     If provided, it will simply be returned.
  ///     Otherwise, if not provided, or a falsy value,
  ///     then attempt to fallback to `modelInstance._connection`,
  ///     if that also fails to find a connection, the finally
  ///     the method will call <see>Model.static _getConnection</see>
  ///     to get the bound connection, if any is available.
  /// Note:
  ///   Pay attention that unlike <see>Model.static _getConnection</see>
  ///   this checks the model's instance for `._connection` property. If that is
  ///   a valid connection, then it will be returned before the bound
  ///   connection. This is helpful when you have chosen not to bind
  ///   a connection to your models. This will allow you to provide a
  ///   connection directly when you create the model, which can make
  ///   interacting with the model less tiresome. i.e. `new Model(null, { connection })`.
  _getConnection(connection) {
    if (connection)
      return connection;

    if (this._connection)
      return this._connection;

    return this.constructor._getConnection();
  }

  /// Get the underlying connection bound to
  /// the model's class. Connection binding is
  /// optional, so this method can also be provided
  /// a connection. If a connection is provided, then
  /// it will simply be returned. Because Mythix ORM
  /// has no globals, this is a design pattern to
  /// supply a connection if you have one available,
  /// or fallback to a "bound" connection (if one can
  /// be found). This method should be overloaded if
  /// you wish to provide your own model specific
  /// connection.
  ///
  /// Return: <see>Connection</see>
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection that can be provided.
  ///     If provided, it will simply be returned.
  ///     Otherwise, if not provided, or a falsy value,
  ///     then attempt to fallback to the bound connection,
  ///     if any is available.
  static getConnection(_connection) {
    let connection = this._getConnection(_connection);
    if (!connection)
      throw new Error(`${this.getModelName()}::getConnection: No connection bound to model. You need to provide a connection for this operation.`);

    return connection;
  }

  getConnection(_connection) {
    if (_connection)
      return _connection;

    let connection = this._getConnection();
    if (!connection)
      return this.constructor.getConnection();

    return connection;
  }

  /// A <see>Connection</see> instance will call
  /// this method on a <see>Model</see> class to
  /// bind the model to the connection. Overload
  /// this to change the behavior of binding connections
  /// to models. This method should return a model
  /// class. The default behavior is to return
  /// `this` class. However, you might return a
  ///  different class for example if you generated
  ///  a new class that inherited from `this` class.
  ///
  /// Return: class extends <see>Model</see>
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     The connection instance to bind to
  ///     this model class.
  static bindConnection(connection) {
    let ModelClass = this;

    // Ensure that the model fields
    // are constructed properly
    ModelClass.getFields();

    let connectionOptions = connection.getOptions();
    if (connectionOptions && connectionOptions.bindModels === false) {
      let modelName = ModelClass.getModelName();

      ModelClass = class BoundModel extends ModelClass {
        static getModelName() {
          return modelName;
        }
      };

      ModelClass.fields = ModelClass.mergeFields();
      ModelClass._sortedFields = null; // Clear cache
    } else {
      if (Object.prototype.hasOwnProperty.call(ModelClass, '_mythixBoundConnection') && ModelClass._mythixBoundConnection)
        throw new Error(`${this.getModelName()}:bindConnection: Model is already bound to a connection. You can not bind a model to a connection more than once.`);
    }

    const whereProp = {
      enumberable:  false,
      configurable: true,
      get:          () => {
        return ModelClass.getQueryEngine(connection);
      },
      set:          () => {},
    };

    Object.defineProperties(ModelClass, {
      '_mythixBoundConnection': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        connection,
      },
      'where':  whereProp,
      '$':      whereProp,
    });

    return ModelClass;
  }

  /// This method is called every time a `Model.where`
  /// or `Model.$` property is accessed. It should return
  /// a class that inherits from (or is) `QueryEngine`. By default,
  /// it will call `this.getConnection().getQueryEngineClass()`
  /// to get the query class defined in the connection
  /// options. Though this can be overloaded per-model
  /// (creating a different type of `QueryEngine` per-model),
  /// the `QueryEngine` class to instantiate to use for queries will
  /// generally be supplied by the connection.
  ///
  /// Return: class <see>QueryEngine</see>
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection to pass through.
  ///     This is needed if your models are not
  ///     bound to a connection.
  static getQueryEngineClass(connection) {
    return this.getConnection(connection).getQueryEngineClass();
  }

  getQueryEngineClass(connection) {
    return this.constructor.getQueryEngineClass(connection);
  }

  /// This method is called any time a `query.unscoped()`
  /// call is made. It will return the query's "root class"
  /// `where` with no <see>Model.static defaultScope</see>
  /// scope applied. Calling "unscoped()" will reset the query,
  /// so make sure you always call it first: `Model.where.unscoped()...`
  ///
  /// Return: <see>QueryEngine</see>
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection to pass through.
  ///     This is needed if your models are not
  ///     bound to a connection.
  ///   options?: object
  ///     Any extra options to pass to the <see>QueryEngine</see>
  ///     constructor.
  static getUnscopedQueryEngine(connection, options) {
    let QueryEngineClass  = this.getQueryEngineClass(connection);
    let queryEngine       = new QueryEngineClass(
      Object.assign(
        {},
        (options || {}),
        {
          connection: this.getConnection(connection),
        },
      ),
    );

    let modelName = this.getModelName();
    return queryEngine[modelName];
  }

  getUnscopedQueryEngine(connection, options) {
    return this.constructor.getUnscopedQueryEngine(connection, options);
  }

  /// One can apply a "default scope" to any model simply
  /// by providing this static method on the model. By
  /// default it will simply return the <see name="derp">QueryEngine</see>
  /// instance (a query) that it was provided.
  ///
  /// The way this works is simple. The caller provides the
  /// query as the first and only argument. The user, by
  /// overloading this method can then easily modify this
  /// provided query, changing the "default scope" whenever
  /// the model is used for queries. For example, let's say
  /// you have a User model, and by default, you only want
  /// to query against Users that have their `active` column
  /// set to `true`. In order to do this, you could easily
  /// provide a `static defaultScope` method on your model
  /// class that would modify the base query. See the following example:
  ///
  /// Example:
  ///   class User extends Model {
  ///     static defaultScope(baseQuery) {
  ///       // `baseQuery` is equal to `User.where`
  ///       // without a default scope.
  ///       return baseQuery.active.EQ(true);
  ///     }
  ///   }
  /// Return: <see>QueryEngine</see>
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query that you should add onto.
  static defaultScope(query) {
    return query;
  }

  defaultScope(queryEngine) {
    return this.constructor.defaultScope(queryEngine);
  }

  /// This method is called any time a `Model.where` or
  /// `Model.$` property is accessed. It returns a query,
  /// based off this model class (as the root model).
  /// It will first call <see>Model.static getUnscopedQueryEngine</see>
  /// to get the root query for the model, and then it will
  /// call <see>Model.static defaultScope</see> to apply any
  /// default scope to the root query. Finally, it will return
  /// the query to the user to start interacting with.
  ///
  /// Return: <see>QueryEngine</see>
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection to pass through.
  ///     This is needed if your models are not
  ///     bound to a connection.
  ///   options?: object
  ///     Any extra options to pass to the <see>QueryEngine</see>
  ///     constructor.
  static getQueryEngine(connection, options) {
    let queryEngine   = this.getUnscopedQueryEngine(connection, options);
    let defaultScope  = this.defaultScope(queryEngine, options);

    return defaultScope;
  }

  getQueryEngine(connection, options) {
    return this.constructor.getQueryEngine(connection, options);
  }

  /// Return the foreign key relationships for the model.
  ///
  /// Return: Map<string, array<object>>
  ///   The format is `Map[modelName] = [ { targetFieldName, sourceFieldName }, ... ]`
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection to pass through.
  ///     This is needed if your models are not
  ///     bound to a connection.
  static getForeignKeyFieldsMap(_connection) {
    let connection          = this.getConnection(_connection);
    let foreignKeyFieldsMap = connection._getFromModelCache(this, 'foreignKeyFieldsMap');
    if (foreignKeyFieldsMap)
      return foreignKeyFieldsMap;

    let foreignKeyFields = new Map();

    Nife.iterate(this.getFields(), ({ value: field }) => {
      let fieldName = field.fieldName;

      if (field.type.isForeignKey()) {
        let targetModelName = field.type.getTargetModelName(connection);
        let targetFieldName = field.type.getTargetFieldName(connection);
        let relationSet     = foreignKeyFields.get(targetModelName);

        if (!relationSet) {
          relationSet = [];
          foreignKeyFields.set(targetModelName, relationSet);
        }

        relationSet.push({ targetFieldName: targetFieldName, sourceFieldName: fieldName });
      }
    });

    connection._setToModelCache(this, 'foreignKeyFieldsMap', foreignKeyFields);

    return foreignKeyFields;
  }

  getForeignKeyFieldsMap(connection) {
    return this.constructor.getForeignKeyFieldsMap(connection);
  }

  /// Return all the foreign key target models. This will be
  /// all models targeted by all foreign keys defined by
  /// foreign key fields on this model.
  ///
  /// Return: Map<string, Model>
  ///   The format is `Map[modelName] = Model`
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection to pass through.
  ///     This is needed if your models are not
  ///     bound to a connection.
  static getForeignKeysTargetModels(_connection) {
    let connection              = this.getConnection(_connection);
    let foreignKeyTargetModels  = connection._getFromModelCache(this, 'foreignKeyTargetModels');
    if (foreignKeyTargetModels)
      return foreignKeyTargetModels;

    let foreignKeyFieldsMap         = this.getForeignKeyFieldsMap(connection);
    let foreignKeyTargetModelNames  = Array.from(foreignKeyFieldsMap.keys());

    foreignKeyTargetModels = new Map();

    for (let i = 0, il = foreignKeyTargetModelNames.length; i < il; i++) {
      let modelName = foreignKeyTargetModelNames[i];
      let Model     = connection.getModel(modelName);

      foreignKeyTargetModels.set(modelName, Model);
    }

    connection._setToModelCache(this, 'foreignKeyTargetModels', foreignKeyTargetModels);

    return foreignKeyTargetModels;
  }

  getForeignKeysTargetModels(connection) {
    return this.constructor.getForeignKeysTargetModels(connection);
  }

  /// Return all the foreign key target model names. This will be
  /// all models targeted by all foreign keys defined by
  /// foreign key fields on this model. This will only
  /// return the models name's.
  ///
  /// Return: Array<string>
  ///   The format is `Array[] = modelName`
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection to pass through.
  ///     This is needed if your models are not
  ///     bound to a connection.
  static getForeignKeysTargetModelNames(_connection) {
    let connection                  = this.getConnection(_connection);
    let foreignKeyTargetModelNames  = connection._getFromModelCache(this, 'foreignKeyTargetModelNames');
    if (foreignKeyTargetModelNames)
      return foreignKeyTargetModelNames;

    let foreignKeyFieldsMap = this.getForeignKeyFieldsMap(connection);
    foreignKeyTargetModelNames = Array.from(foreignKeyFieldsMap.keys());

    connection._setToModelCache(this, 'foreignKeyTargetModelNames', foreignKeyTargetModelNames);

    return foreignKeyTargetModelNames;
  }

  getForeignKeysTargetModelNames(connection) {
    return this.constructor.getForeignKeysTargetModelNames(connection);
  }

  /// Return all the foreign key target field names.
  /// This will return the target field names of all
  /// foreign key fields specified on the model.
  ///
  /// Return: Array<string>
  ///   The format is `Array[] = modelName`
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection to pass through.
  ///     This is needed if your models are not
  ///     bound to a connection.
  ///   modelName: string
  ///     The model name for which you wish to fetch
  ///     foreign key fields from.
  static getForeignKeysTargetFieldNames(_connection, modelName) {
    let connection              = this.getConnection(_connection);
    let cacheKey                = `${modelName}:foreignKeyFieldNames`;
    let foreignKeyFieldNames    = connection._getFromModelCache(this, cacheKey);
    if (foreignKeyFieldNames)
      return foreignKeyFieldNames;

    let foreignKeyFieldsMap = this.getForeignKeyFieldsMap(connection);
    let fieldNames = (foreignKeyFieldsMap.get(modelName) || []);

    connection._setToModelCache(this, cacheKey, fieldNames);

    return fieldNames;
  }

  getForeignKeysTargetFieldNames(connection, modelName) {
    return this.constructor.getForeignKeysTargetFieldNames(connection, modelName);
  }

  /// Get a specific foreign key field's relationship
  /// information. This will be what is stored in the
  /// relationship field map for the specified target
  /// field.
  /// See <see>Model.static getForeignKeyFieldsMap</see> for more information.
  ///
  /// Return: object<{ targetFieldName, sourceFieldName }>
  ///   This will be the relationship info for this foreign key field
  ///   which will be an object of the shape `{ targetFieldName, sourceFieldName }`.
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection to pass through.
  ///     This is needed if your models are not
  ///     bound to a connection.
  ///   modelName: string
  ///     The model name for which you wish to fetch
  ///     foreign key fields from.
  ///   fieldName: string
  ///     The field name for which you wish to fetch
  ///     foreign key relational information about.
  static getForeignKeysTargetField(_connection, modelName, fieldName) {
    let connection            = this.getConnection(_connection);
    let cacheKey              = `${modelName}:${fieldName}:foreignKeyTargetField`;
    let foreignKeyFieldNames  = connection._getFromModelCache(this, cacheKey);
    if (foreignKeyFieldNames)
      return foreignKeyFieldNames;

    let foreignKeyFieldsMap = this.getForeignKeyFieldsMap(connection);
    let fields              = foreignKeyFieldsMap.get(modelName);
    if (!fields)
      return;

    let fieldInfo = fields.find((fieldInfo) => (fieldInfo.targetFieldName === fieldName));

    connection._setToModelCache(this, cacheKey, fieldInfo);

    return fieldInfo;
  }

  getForeignKeysTargetField(connection, modelName, fieldName) {
    return this.constructor.getForeignKeysTargetField(connection, modelName, fieldName);
  }

  /// Check if the specified model is a model
  /// pointed to by a foreign key field.
  ///
  /// Return: boolean
  ///   `true` if the specified `modelName` model is pointed
  ///   to by one of the foreign key fields, `false` otherwise.
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection to pass through.
  ///     This is needed if your models are not
  ///     bound to a connection.
  ///   modelName: string
  ///     The model name for which you wish to fetch
  ///     foreign key fields from.
  static isForeignKeyTargetModel(_connection, modelName) {
    let connection          = this.getConnection(_connection);
    let foreignKeyFieldsMap = this.getForeignKeyFieldsMap(connection);
    return foreignKeyFieldsMap.has(modelName);
  }

  isForeignKeyTargetModel(connection, modelName) {
    return this.constructor.isForeignKeyTargetModel(connection, modelName);
  }

  /// Get the table name for this model. By default
  /// Mythix ORM will take the model's name, and convert
  /// it to snake_case.
  ///
  /// Return: string
  ///   The name of the table for this model.
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection to pass through.
  ///     This is needed if your models are not
  ///     bound to a connection.
  static getTableName(connection) {
    if (!connection) {
      let tableName = Nife.camelCaseToSnakeCase(this.getPluralModelName());
      return tableName;
    }

    let cacheKey        = `${this.getModelName()}:tableName`;
    let modelTableName  = connection._getFromModelCache(this, cacheKey);
    if (modelTableName)
      return modelTableName;

    let tableName = Nife.camelCaseToSnakeCase(this.getPluralModelName());

    connection._setToModelCache(this, cacheKey, tableName);

    return tableName;
  }

  getTableName(connection) {
    return this.constructor.getTableName(connection);
  }

  /// Get the model name for this model. By default
  /// this will be the name of the class itself, i.e.
  /// `this.name`. Overload this method to provide your
  /// own model name (singular name).
  ///
  /// Return: string
  ///   The name the model.
  static getModelName() {
    return this.name;
  }

  getModelName() {
    return this.constructor.getModelName();
  }

  /// Get the singular model name for this model. By default
  /// this will be the name of the class itself, i.e.
  /// `this.name`. You should probably overload <see>Model.static getModelName</see>
  /// to provide your own model name, instead of overloading this method.
  ///
  /// Note:
  ///   This will return the "singular" name of the model
  ///   which is the same as <see>Model.static getModelName</see>.
  /// Note:
  ///   This method simply calls <see>Model.static getModelName</see>
  ///   to get the singular model name.
  /// Return: string
  ///   The name the model in singular form.
  /// See: Model.static getPluralModelName
  /// See: Model.static getModelName
  static getSingularName() {
    return this.getModelName();
  }

  getSingularName() {
    return this.constructor.getSingularName();
  }

  /// Get the plural model name for this model. By default
  /// this will be the singular name of the model, converted
  /// to plural using the [Inflection](https://github.com/dreamerslab/node.inflection)
  /// library. Overload this method to provide your own plural
  /// name for the model.
  ///
  /// Note:
  ///   This will return the "plural" name of the model.
  /// Return: string
  ///   The name the model in plural form.
  /// See: Model.static getSingularName
  static getPluralModelName() {
    return Inflection.pluralize(this.getSingularName());
  }

  getPluralModelName() {
    return this.constructor.getPluralModelName();
  }

  /// Get the Model class for this model.
  ///
  /// Return: class <see>Model</see>
  static getModel() {
    return this;
  }

  getModel() {
    return this.constructor.getModel();
  }

  /// Get the fields for this model. You can optionally
  /// specify the `fieldNames` argument, which will cause
  /// the method to return only the fields specified by name.
  /// If the `fieldNames` argument is provided, then the
  /// return value will always be an array of fields. If
  /// the `fieldNames` argument is not provided, then
  /// the return value could be either an Array of fields,
  /// or an object of fields, keyed by field name (depending
  /// on how you defined your model fields).
  ///
  /// Note:
  ///   The first time this method is called the model's fields
  ///   will be built, and possibly modified. All fields will be
  ///   turned into <see>Field</see> instances, and each field
  ///   will be properly constructed to include required attributes,
  ///   such as their parent `Model`, their `fieldName`, and their
  ///   `columnName`. A model's fields are always initialized when
  ///   the model is first bound to a connection, or when this method
  ///   is first called.
  /// Note:
  ///   In Mythix ORM you **never** specify a field via its `columnName`.
  ///   You always use a field's defined `fieldName` to match against
  ///   a field. Using a `columnName` simply won't work, unless `columnName`
  ///   and `fieldName` just happen to have the same value.
  /// Return: Array<<see>Field</see>> | Object<string, <see>Field</see>>
  /// Arguments:
  ///   fieldNames?: Array<string>
  ///     An array of field names to fetch. If not
  ///     provided then all model fields will be returned,
  ///     including virtual fields.
  static getFields(fieldNames) {
    let fields = this.initializeFields(this.fields);
    if (fields !== this.fields) {
      Object.defineProperties(this, {
        'fields': {
          writable:     true,
          enumberable:  true,
          configurable: true,
          value:        fields,
        },
        '_sortedFields': {
          writable:     true,
          enumberable:  true,
          configurable: true,
          value:        null,
        },
      });
    }

    if (Nife.isNotEmpty(fieldNames)) {
      let filteredFields = [];

      this.iterateFields(({ field, fieldName }) => {
        if (fieldNames.indexOf(fieldName) < 0)
          return;

        filteredFields.push(field);
      }, fields);

      return filteredFields;
    } else {
      return fields;
    }
  }

  getFields(fieldNames) {
    return this.constructor.getFields(fieldNames);
  }

  /// This method is identical to <see>Model.static getFields</see>
  /// except that it will always return an Array of fields, and the
  /// fields will always be sorted by their `fieldName`.
  ///
  /// Note:
  ///   In Mythix ORM you **never** specify a field via its `columnName`.
  ///   You always use a field's defined `fieldName` to match against
  ///   a field. Using a `columnName` simply won't work, unless `columnName`
  ///   and `fieldName` just happen to have the same value.
  /// Return: Array<<see>Field</see>>
  /// Arguments:
  ///   fieldNames?: Array<string>
  ///     An array of field names to fetch. If not
  ///     provided then all model fields will be returned,
  ///     including virtual fields.
  static getSortedFields(fieldNames) {
    if (Object.prototype.hasOwnProperty.call(this, '_sortedFields') && this._sortedFields)
      return this._sortedFields;

    let fields              = this.getFields(fieldNames);
    let sortedFields        = [];
    let primaryKeyFieldName = this.getPrimaryKeyFieldName();

    Nife.iterate(fields, ({ value, context}) => context.push(value), sortedFields);

    sortedFields = sortedFields.sort((a, b) => {
      let x = a.fieldName;
      let y = b.fieldName;

      if (primaryKeyFieldName) {
        if (x === primaryKeyFieldName)
          return -1;
        else if (y === primaryKeyFieldName)
          return 1;
      }

      if (x === y)
        return 0;

      return (x < y) ? -1 : 1;
    });

    this._sortedFields = sortedFields;

    return sortedFields;
  }

  getSortedFields(fieldNames) {
    return this.constructor.getSortedFields(fieldNames);
  }

  /// Merge specified fields into this model's fields.
  /// The `mergeFields` argument is optional. If not
  /// provided, then this method will simply clone
  /// the model's fields. If specified, then the fields
  /// provided will be merged into the existing fields.
  /// Merging takes place based on each field's `fieldName`
  /// attribute. The `mergeFields` argument can be an
  /// `Array`, an `object`, a `Map`, or a `Set`. Fields
  /// are matched based on `fieldName`, but when a match
  /// is found, it is completely overridden by what is
  /// found in the `mergeFields` argument. In short, the
  /// list of fields itself is merged, but the fields themselves
  /// are not merged (a shallow merge). All input objects that
  /// have keys (Object<string, <see>Field</see>> | Map<string, <see>Field</see>>)
  /// must specify the `fieldName` of the field as the key.
  /// If the input is an Array type (Array<<see>Field</see>> | Set<<see>Field</see>>)
  /// then each field **must** contain a `fieldName` attribute, or
  /// an exception will be thrown.
  ///
  /// Note:
  ///   Anywhere you see <see>Field</see> as an input in this documentation,
  ///   you can also simply provide a raw object with the same shape, and
  ///   it will be automatically converted to a <see>Field</see> instance
  ///   for you.
  /// Return: Array<<see>Field</see>> | object<string, <see>Field</see>>
  /// Arguments:
  ///   mergeFields?: Array<<see>Field</see>> | Set<<see>Field</see>> | Object<string, <see>Field</see>> | Map<string, <see>Field</see>>
  ///     A list of fields to merge into the current
  ///     model fields. The field list,
  ///     as well as all fields will be cloned.
  static mergeFields(mergeFields) {
    const cloneField = ({ value: field, key: _fieldName, index, type }) => {
      if (!field)
        return;

      if (Field.isField(field)) {
        let clonedField = field.clone();

        if (Type.isType(clonedField.type)) {
          clonedField.type.setField(clonedField);
          clonedField.type.setModel(ModelClass);
        }

        clonedField.setModel(ModelClass);

        clonedFields.set(field.fieldName, clonedField);

        return;
      }

      let fieldName = (type === 'Set' || type === 'Array') ? field.fieldName : (field.fieldName || _fieldName);
      if (Nife.isEmpty(fieldName))
        throw new Error(`${this.name}::mergeFields: "fieldName" is missing on field index ${index}.`);

      let fieldCopy = Object.assign({}, field, { Model: ModelClass });
      if (Type.isType(fieldCopy.type)) {
        fieldCopy.type.setField(fieldCopy);
        fieldCopy.type.setModel(ModelClass);
      }

      clonedFields.set(fieldName, fieldCopy);
    };

    const mapToObject = (map) => {
      let obj = {};

      for (let [ key, value ] of map.entries())
        obj[key] = value;

      return obj;
    };

    let ModelClass    = this;
    let fields        = this.fields;
    let clonedFields  = new Map();

    Nife.iterate(fields, cloneField);
    Nife.iterate(mergeFields, cloneField);

    return (Nife.instanceOf(fields, 'array', 'set')) ? Array.from(clonedFields.values()) : mapToObject(clonedFields);
  }

  /// Initialize all fields for the model. This will be called anytime
  /// a <see>Model.static getFields</see> call is made. However, it
  /// caches its results, so it will immediately return if the model's
  /// fields have already been initialized.
  ///
  /// This method does a number of things in the process of "initializing"
  /// model fields. It first ensures that every field is an instance of
  /// <see>Field</see>. Second, it ensures that the <see>Type</see> of the
  /// field is initialized (properly instantiated). Third, it ensures that
  /// all required attributes of each field is present. Required attributes
  /// are `Model` (the parent model), `fieldName` (the name of this field),
  /// and `columnName` (the column name for the DB). `columnName`, if not
  /// provided, will simply become `fieldName`.
  ///
  /// Note:
  ///   The cache for built fields is stored directly on the input `fields`
  ///   argument, under a non-enumerable `_mythixFieldsInitialized` cache key.
  /// Return: Array<<see>Field</see>> | object<string, <see>Field</see>>
  /// Arguments:
  ///   fields: Array<<see>Field</see>> | Set<<see>Field</see>> | Object<string, <see>Field</see>> | Map<string, <see>Field</see>>
  ///     A list of fields to work off of. When this method is
  ///     called by <see>Model.static getFields</see>, then
  ///     this argument will be `static Model.fields`.
  static initializeFields(fields) {
    if (!fields)
      return fields;

    if (fields._mythixFieldsInitialized)
      return fields;

    let ModelClass      = this;
    let isArray         = Array.isArray(fields);
    let finalizedFields = (isArray) ? [] : {};
    let primaryKeyField;

    Nife.iterate(fields, ({ value: _field, key: _fieldName, index, type: collectionType }) => {
      let field = _field;
      if (!field)
        return;

      // If we have a type instead of an object,
      // then mutate it into the proper structure
      if (Type.isTypeClass(field) || Type.isType(field)) {
        field = {
          type: field,
        };
      }

      let fieldName = (collectionType === 'Set' || collectionType === 'Array') ? field.fieldName : (field.fieldName || _fieldName);
      if (Nife.isEmpty(fieldName))
        throw new Error(`${ModelClass.getModelName()}::initializeFields: "fieldName" is missing on field index ${index}.`);

      field.fieldName = fieldName;

      if (!field.type)
        throw new Error(`${ModelClass.getModelName()}::initializeFields: "type" not found on "${this.getModelName()}.${fieldName}". "type" is required for all fields.`);

      if (!field.columnName)
        field.columnName = fieldName;

      field.Model = ModelClass;

      if (!Field.isField(field))
        field = new Field(field);

      if (field.primaryKey)
        primaryKeyField = field;

      let type = field.type = Type.instantiateType(field.type);
      type.setField(field);
      type.setModel(ModelClass);

      if (isArray)
        finalizedFields.push(field);
      else
        finalizedFields[fieldName] = field;
    });

    Object.defineProperties(finalizedFields, {
      '_mythixFieldsInitialized': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        true,
      },
      '_primaryKeyField': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        primaryKeyField,
      },
    });

    return finalizedFields;
  }

  /// Iterate all model fields. This is a convenience method to
  /// iterate `static Model.fields`, and is needed as the fields
  /// of a model can be an `Array`, an `object`, a `Map`, or a `Set`.
  /// This method works a lot like `Array.prototype.map`. Any return
  /// value from the provided `callback` will be pushed into an array
  /// just like `Array.prototype.map`. The `callback` provided, when called,
  /// will be provided a `context` object, as the single argument to
  /// the callback.
  ///
  /// At any time you can call the `stop` method provided via the context.
  /// When called, `iterateFields` will stop iterating, and immediately
  /// return an array of results returned by all calls to `callback`.
  ///
  /// The provided `context` object has the following shape:
  ///
  /// Interface:
  ///   interface IterationContext {
  ///     field: Field;           // The Field instance itself.
  ///     fieldName: string;      // The name of the field.
  ///     fields:                 // All the model's fields.
  ///       Array<Field> |
  ///       Set<Field> |
  ///       Object<string, Field> |
  //        Map<string, Field>;
  ///     index: string | number; // The current index into the list of fields.
  ///     stop: Function;         // A method that when called will halt iteration.
  ///     isStopped: Function;    // Call this method to see if iteration is about to be halted.
  ///   }
  ///
  /// Return: Array<any>
  /// Arguments:
  ///   callback: Function(context: IterationContext)
  ///     A callback method that will be called for
  ///     every field on the model.
  ///   fields?: Array<<see>Field</see>> | Set<<see>Field</see>> | Object<string, <see>Field</see>> | Map<string, <see>Field</see>> = null
  ///     An optional list of fields to use *instead* of `static Model.fields`. This is
  ///     handy if, for example, you want to iterate a sub-set of the model's fields,
  ///     such as the "dirty" fields reported by the model.
  ///   sorted?: boolean = false
  ///     If `true`, then sort the model fields before iterating. If `false`,
  ///     simply iterate the model's fields in their defined order.
  static iterateFields(callback, _fields, sorted) {
    if (typeof callback !== 'function')
      return [];

    let fields = _fields;
    if (!fields)
      fields = (sorted !== false) ? this.getSortedFields() : this.getFields();

    return Nife.iterate(fields, ({ value: field, index, context, stop, isStopped }) => {
      let result = callback({ field, fieldName: field.fieldName, fields, index, stop, isStopped });

      if (!isStopped())
        context.push(result);
    }, []);
  }

  iterateFields(callback, _fields, sorted) {
    return this.constructor.iterateFields(callback, _fields, sorted);
  }

  /// Check if any of the model's fields have a "remote value"
  /// as a `defaultValue`. A `defaultValue` method can itself
  /// report that it is "remote", meaning it is a value provided
  /// by the database itself. This method simply iterates all the
  /// model's fields, and calls `field.type.isRemote()` on each field.
  /// If `field.type.isRemote()` returns `true` for any field, then
  /// this method will return `true`, otherwise it will return `false`.
  /// Use this method to know if the model contains any fields whose
  /// value is obtained directly from the database itself (i.e. primary key
  /// date fields, etc...).
  ///
  /// Return: boolean
  static hasRemoteFieldValues() {
    let hasRemote = false;

    this.iterateFields(({ field, stop }) => {
      if (field.type.isRemote()) {
        hasRemote = true;
        stop();
      }
    });

    return hasRemote;
  }

  hasRemoteFieldValues() {
    return this.constructor.hasRemoteFieldValues();
  }

  /// Get the primary key field of the model, if any is defined.
  /// If no primary key field is defined, then this will return
  /// `undefined`. A primary key is specified per-model by using
  /// `primaryKey: true` on one (and only one) of the model's fields.
  ///
  /// Return: <see>Field</see>
  static getPrimaryKeyField() {
    let fields = this.getFields();
    if (!fields)
      return;

    return fields._primaryKeyField;
  }

  getPrimaryKeyField() {
    return this.constructor.getPrimaryKeyField();
  }

  /// Get the *name* of the primary key field of the model,
  /// if any is defined.
  /// If no primary key field is defined, then this will return
  /// `undefined`. A primary key is specified per-model by using
  /// `primaryKey: true` on one (and only one) of the model's fields.
  ///
  /// Return: string
  static getPrimaryKeyFieldName() {
    let primaryKeyField = this.getPrimaryKeyField();
    if (!primaryKeyField)
      return;

    return primaryKeyField.fieldName;
  }

  getPrimaryKeyFieldName() {
    return this.constructor.getPrimaryKeyFieldName();
  }

  /// Check to see if the primary key field (if any is defined)
  /// has a "remote" default value. For example, an autoincrementing
  /// primary key field would return `true` from this method, because
  /// an autoincrementing field value is retrieved directly from the
  /// database. This could return `false`, if for example, you were
  /// using UUIDs, or XIDs for your primary key.
  ///
  /// Return: boolean
  ///   `true` if the primary key field of the model has a "remote"
  ///   `defaultValue`, `false` otherwise. Remoteness is checked
  ///   via `this.getPrimaryKeyField().type.isRemote()`.
  static primaryKeyHasRemoteValue() {
    let primaryKeyField = this.getPrimaryKeyField();
    if (!primaryKeyField)
      return false;

    return primaryKeyField.type.isRemote();
  }

  primaryKeyHasRemoteValue() {
    return this.constructor.primaryKeyHasRemoteValue();
  }

  /// Get a specific model field by field name. This method
  /// will return `undefined` if the specified field can not
  /// be found on the model. This will find both concrete and
  /// virtual fields that are defined on the model.
  ///
  /// Note:
  ///   In Mythix ORM you **never** specify a field via its `columnName`.
  ///   You always use a field's defined `fieldName` to match against
  ///   a field. Using a `columnName` simply won't work, unless `columnName`
  ///   and `fieldName` just happen to have the same value.
  /// Return: <see>Field</see>
  /// Arguments:
  ///   fieldName: string
  ///     The specified field name to find. This **is not** the `columnName` of the field.
  static getField(findFieldName) {
    let fields = this.getFields();
    if (!fields || !findFieldName)
      return;

    let foundField;

    this.iterateFields(({ field, fieldName, stop }) => {
      if (fieldName === findFieldName) {
        foundField = field;
        stop();
      }
    });

    return foundField;
  }

  getField(findFieldName) {
    return this.constructor.getField(findFieldName);
  }

  /// Check if the model has the specified field by name.
  /// If the specified field is found on the model, then
  /// return `true`, otherwise `false` will be returned.
  ///
  /// Note:
  ///   In Mythix ORM you **never** specify a field via its `columnName`.
  ///   You always use a field's defined `fieldName` to match against
  ///   a field. Using a `columnName` simply won't work, unless `columnName`
  ///   and `fieldName` just happen to have the same value.
  /// Return: boolean
  /// Arguments:
  ///   fieldName: string
  ///     The specified field name to find. This **is not** the `columnName` of the field.
  static hasField(fieldName) {
    return !!this.getField(fieldName);
  }

  hasField(fieldName) {
    return this.constructor.hasField(fieldName);
  }

  /// Count the number of concrete fields on the model.
  /// "concrete" fields are non-virtual fields that are
  /// backed by the database. A `FOREIGN_KEY` field **is**
  /// a concrete field, so `FOREIGN_KEY` fields will be counted.
  ///
  /// Return: number
  ///   The number of concrete fields the model has
  static getConcreteFieldCount() {
    let count = 0;

    this.iterateFields(({ field }) => {
      if (field.type.isVirtual())
        return;

      count++;
    });

    return count;
  }

  getConcreteFieldCount() {
    return this.constructor.getConcreteFieldCount();
  }

  /// Specify the default SELECT "ORDER" for the model.
  /// This method will be called if no "ORDER" was specified
  /// for any given query. This method should return an
  /// Array of fully qualified field names. The `options`
  /// argument is the current options for the specific
  /// query being operated on.
  ///
  /// Note:
  ///   Internally, Mythix ORM will call `this.connection.defaultOrder(options)`,
  ///   which--if not overloaded--will simply call this method from the
  ///   model itself.
  /// Note:
  ///   A "fully qualified field name" in Mythix ORM means
  ///   a full field definition, including the model name.
  ///   An example of a fully qualified field name might be
  ///   `"User:id"`. The model name is separated from the field
  ///   name by a colon `:`. A short hand, *not fully qualified
  ///   field name* example would be simply `"id"`. Since this
  ///   contains no model prefix, this is "short hand", and is
  ///   **not** a fully qualified field name.
  /// Return: Array<string> | null
  ///   An array of fully qualified field names to specify the ORDER.
  ///   Prefix a field name with `+` to specify ASCending order, i.e.
  ///   `[ "+User:id" ]`. Prefix the field name with `-` to specify
  ///   DESCending order, i.e. `[ "-User:id" ]`.
  static defaultOrder(options) {
  }

  /// This method is called anytime a `Model.where` or `Model.$`
  /// attribute is accessed. It will provide the instantiated
  /// <see>QueryEngine</see> with the connection specified (if any).
  /// A connection can be specified for example by doing `Model.where(connection)`
  /// or `Model.$(connection)`. This is generally only useful if you
  /// have chosen not to bind your models to a connection.
  ///
  /// Return: <see>QueryEngine</see>
  ///   A <see>QueryEngine</see> instance (a query) for
  ///   this model.
  /// Arguments:
  ///   options?: `object { connection: Connection }`
  ///     An object, which if supplied, should contain a
  ///     `connection` key, specifying the connection.
  ///     If no connection is provided, then this will
  ///     fallback to `this.getConnection()` to try and
  ///     find the connection itself.
  static getWhereWithConnection(options) {
    if (options && options.connection)
      return this.where(options.connection);
    else
      return this.where;
  }

  getWhereWithConnection(options) {
    return this.constructor.getWhereWithConnection(options);
  }

  /// Create (insert) new models into the database.
  /// The `models` argument can be a single model instance,
  /// a single object containing model attributes,
  /// an array of model instances, or an array of
  /// objects containing model attributes. This is
  /// a "bulk" create method, though it can be used
  /// to create a single model.
  ///
  /// Return: <see>Model</see>
  ///   Return the model instance(s) created.
  /// Arguments:
  ///   models: Array<<see>Model</see>> | <see>Model</see> | Array<object> | object
  ///     Specify the model(s) to create.
  ///   options?: object
  ///     An "options" object to pass off to the underlying <see>Connection</see> methods.
  ///     If a `connection` key is specified in this object, then that will be used
  ///     as the connection for the operation. This can be important if for example
  ///     you are calling this from a `transaction`, in which case you most certainly
  ///     would want to provide the `connection` for the transaction.
  static async create(models, options) {
    let connection = this.getConnection() || (options && options.connection);
    if (!connection)
      throw new Error(`${this.constructor.name}::create: Connection not found. A connection is required to be bound to the model. Is your model not yet initialized through a connection?`);

    let result = await connection.insert(this.getModel(), models, options);
    if (Array.isArray(models))
      return result;

    return (Array.isArray(result)) ? result[0] : result;
  }

  /// Count the rows in the database for this model.
  ///
  /// Return: number
  ///   Return the number of models stored in the database for this model type.
  /// Arguments:
  ///   options?: object
  ///     An "options" object to pass off to the underlying <see>Connection</see> methods.
  ///     If a `connection` key is specified in this object, then that will be used
  ///     as the connection for the operation. This can be important if for example
  ///     you are calling this from a `transaction`, in which case you most certainly
  ///     would want to provide the `connection` for the transaction.
  static count(options) {
    return this.getWhereWithConnection(options).count(null, options);
  }

  /// Fetch all models from the database for this model type.
  ///
  /// Note:
  ///   This will fetch ALL rows for the model. If this is not what you want,
  ///   then construct a query first, and call `all` from the query itself, i.e.
  ///   `User.where.firstName.EQ('Bob').all(options)`.
  /// Return: Array<<see>Model</see>>
  ///   Return all models of this type from the database.
  /// Arguments:
  ///   options?: object
  ///     An "options" object to pass off to the underlying <see>Connection</see> methods.
  ///     If a `connection` key is specified in this object, then that will be used
  ///     as the connection for the operation. This can be important if for example
  ///     you are calling this from a `transaction`, in which case you most certainly
  ///     would want to provide the `connection` for the transaction.
  ///     If a `stream: true` option is provided, then an async generator will be
  ///     returned, allowing you to "stream" the rows from the database in batches.
  ///     The default `batchSize` is `500`, but can be overridden by setting the `batchSize`
  ///     option to some valid positive number, or `Infinity` to pull everything at once.
  ///     If the `stream` option is not specified, or is `false`, then all rows will be
  ///     fetched from the database in batches, collected into an array, and returned
  ///     only once all rows have been fetched. This is the default behavior.
  static all(options) {
    return this.getWhereWithConnection(options).all(options);
  }

  /// Get the first (limit) rows from the database for this model type.
  ///
  /// Note:
  ///   This will fetch the first rows for the model, in database order.
  ///   If you wish to filter, and specify the order, then construct a
  ///   query first, and call the `first` method from the query itself, i.e.
  ///   `User.where.firstName.EQ('Bob').ORDER('+User:firstName').first(options)`
  /// Return: <see>Model</see> | Array<<see>Model</see>>
  ///   Return `limit` models of this type from the database. If
  ///   `limit` is `null`, `undefined`, or `1`, then a single model
  ///   instance will be returned (or `undefined` if nothing is found).
  ///   If the `limit` argument is more than `1`, then an array
  ///   of model instances will be returned, or an empty array if
  ///   nothing is found.
  /// Arguments:
  ///   limit?: number = 1
  ///     The number of model instances to fetch from the database.
  ///   options?: object
  ///     An "options" object to pass off to the underlying <see>Connection</see> methods.
  ///     If a `connection` key is specified in this object, then that will be used
  ///     as the connection for the operation. This can be important if for example
  ///     you are calling this from a `transaction`, in which case you most certainly
  ///     would want to provide the `connection` for the transaction.
  static first(limit, options) {
    if (limit != null && !Nife.instanceOf(limit, 'number'))
      throw new Error(`${this.getModelName()}::first: "limit" must be null, or a number. If you want to supply a query, use "${this.getModelName()}.where.{query}.first(limit)" instead.`);

    return this.getWhereWithConnection(options).first(limit, options);
  }

  /// Get the last (limit) rows from the database for this model type.
  ///
  /// Note:
  ///   This will fetch the last rows for the model, in database order.
  ///   If you wish to filter, and specify the order, then construct a
  ///   query first, and call the `last` method from the query itself, i.e.
  ///   `User.where.firstName.EQ('Bob').ORDER('+User:firstName').last(options)`
  /// Note:
  ///   This works by telling the underlying query generator to invert
  ///   the specified ORDER of the query, and then it selects the first
  ///   `limit` rows from the result.
  /// Return: <see>Model</see> | Array<<see>Model</see>>
  ///   Return `limit` models of this type from the database. If
  ///   `limit` is `null`, `undefined`, or `1`, then a single model
  ///   instance will be returned (or `undefined` if nothing is found).
  ///   If the `limit` argument is more than `1`, then an array
  ///   of model instances will be returned, or an empty array if
  ///   nothing is found.
  /// Arguments:
  ///   limit?: number = 1
  ///     The number of model instances to fetch from the database.
  ///   options?: object
  ///     An "options" object to pass off to the underlying <see>Connection</see> methods.
  ///     If a `connection` key is specified in this object, then that will be used
  ///     as the connection for the operation. This can be important if for example
  ///     you are calling this from a `transaction`, in which case you most certainly
  ///     would want to provide the `connection` for the transaction.
  static last(limit, options) {
    if (limit != null && !Nife.instanceOf(limit, 'number'))
      throw new Error(`${this.getModelName()}::last: "limit" must be null, or a number. If you want to supply a query, use "${this.getModelName()}.where.{query}.last(limit)" instead.`);

    return this.getWhereWithConnection(options).last(limit, options);
  }

  /// Pluck specific fields (columns) from the database for this model type.
  ///
  /// Note:
  ///   This will pluck across ALL rows for the model, in database order.
  ///   If you wish to filter and limit, and specify the order, then construct a
  ///   query first, and call the `pluck` method from the query itself, i.e.
  ///   `User.where.firstName.EQ('Bob').LIMIT(50).ORDER('+User:firstName').pluck([ 'User:firstName' ], options)`
  /// Note:
  ///   In Mythix ORM you **never** specify a field via its `columnName`.
  ///   You always use a field's defined `fieldName` to match against
  ///   a field. Using a `columnName` simply won't work, unless `columnName`
  ///   and `fieldName` just happen to have the same value.
  /// Return: Array<any> | Array<Array<any>>
  ///   If only a single field is specified, then a flat array
  ///   of values will be returned across all rows. If more than
  ///   one field is specified, then an array of arrays (rows) will
  ///   be returned for the fields specified.
  /// Arguments:
  ///   fields?: Array<string>
  ///     An array of fully qualified field names to pluck from the underlying
  ///     database table for this model. **Do not use column names here**... these
  ///     must be fully qualified field names.
  ///   options?: object
  ///     An "options" object to pass off to the underlying <see>Connection</see> methods.
  ///     If a `connection` key is specified in this object, then that will be used
  ///     as the connection for the operation. This can be important if for example
  ///     you are calling this from a `transaction`, in which case you most certainly
  ///     would want to provide the `connection` for the transaction.
  static pluck(fields, options) {
    return this.getWhereWithConnection(options).pluck(fields);
  }

  /// Construct a new <see>Model</see> instance.
  ///
  /// Return: <see>Model</see>
  /// Arguments:
  ///   data?: object
  ///     Attributes to provide to the new model instance.
  ///     The key names should be a `fieldName`, not a `columnName`.
  ///     Any model attribute missing from `data`, or whose value is
  ///     `undefined`, will be given a default value from the `defaultValue`,
  ///     field property for the field. If the `defaultValue` for the
  ///     field is marked as a "remote" value (a value provided by the database)
  ///     or if `defaultValue` is *not* marked with the flag
  ///     `FLAG_ON_INITIALIZE`, then the field will remain with
  ///     a value of `undefined`, at least until the model is stored
  ///     to the database. The default for all methods provided to the
  ///     `defaultValue` key of the field is to have the `FLAG_ON_INITIALIZE`
  ///     flag, so the value will be created on model instantiation.
  ///     Some default value methods however, such as `AUTOINCREMENT`,
  ///     do not have the `FLAG_ON_INITIALIZE` set, because the defaultValue
  ///     is provided by the database.
  ///     `null` is a valid value, and if provided for
  ///     an attribute, then the `defaultValue` will not be used.
  ///     Any attribute value provided will go through
  ///     `field.type.castValue` from the field `type` property
  ///     (this includes `null` values). Any attributes provided
  ///     will immediately be marked as "dirty" as soon as the model
  ///     is instantiated.
  ///   options?: object
  ///     Options for the model. The only property Mythix ORM
  ///     will use is the `connection` key. If provided, it will set
  ///     this as the `_connection` property on the instance. Otherwise,
  ///     these options are intended to be used by you, the developer,
  ///     to do with as you please on your models. They can be fetched
  ///     from any model instance simply by calling <see>Model.getOptions</see>.
  constructor(data, _options) {
    let options = _options || {};

    const whereProp = {
      enumberable:  false,
      configurable: true,
      get:          () => {
        return this.constructor.where(this._getConnection());
      },
      set:          () => {},
    };

    Object.defineProperties(this, {
      '_options': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        options,
      },
      '_mythixModelInstance': {
        writable:     false,
        enumberable:  false,
        configurable: false,
        value:        true,
      },
      '_connection': {
        writable:     false,
        enumberable:  false,
        configurable: false,
        value:        this._getConnection(options.connection),
      },
      '_fieldData': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        {},
      },
      '_dirtyFieldData': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        {},
      },
      '_typeData': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        {},
      },
      'dirtyID': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        new CacheKey(),
      },
      '_persisted': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        false,
      },
      '__order': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        0,
      },
      '__assignedRelatedModels': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        new Map(),
      },
      'changes': {
        enumberable:  false,
        configurable: true,
        get:          () => {
          return this._getDirtyFields();
        },
        set:          () => {},
      },
      'where':        whereProp,
      '$':            whereProp,
    });

    this._constructor(data);
  }

  /// Get the options provided to the model, if any.
  /// This will always return an object, even if no
  /// options were provided, in which case the object
  /// will simply be an empty object.
  ///
  /// Return: object
  ///   Options object provided by the user to the model
  ///   when the model was constructed.
  getOptions() {
    return this._options;
  }

  /// This is the final call the `constructor` makes
  /// before it finalizes instantiating the model.
  /// It calls <see>Model._constructFields</see> to
  /// construct the model's fields, and then it calls
  /// <see>Model._initializeModelData</see> to initialize
  /// the field values for the model.
  ///
  /// Return: `undefined`
  /// Arguments:
  ///   data?: object
  ///     Attributes provided to the model through
  ///     the `constructor` call.
  _constructor(data) {
    this._constructFields();
    this._initializeModelData(data);
  }

  /// Construct the model's fields on the model instance.
  /// This method will iterate the `static Model.fields`
  /// fields for the model. For each field (except virtual
  /// fields), it will call `field.type.exposeToModel()`.
  /// If this method returns `true`, then the field will
  /// be added as a getter/setter to the model instance,
  /// with the name defined as the field's `fieldName`.
  /// For example, if a `User` model had a field named
  /// `fieldName: 'firstName'`, then the `firstName`
  /// property will be added to the model instance, as a
  /// getter/setter property on the instance. The getter/
  /// setter provide the functionality of sending data in
  /// and out of `get` and `set` methods on the field itself,
  /// and through each `field.type.castValue` method on set.
  /// The setter will also mark a field as dirty if it is set
  /// to a value that is different from the current value of
  /// the field. The <see>Model.getDataValue</see> and
  /// <see>Model.setDataValue</see> instance methods will bypass
  /// the getter/setter of each field, allowing direct access
  /// to the underlying attribute value.
  _constructFields() {
    this.iterateFields(({ field, fieldName }) => {
      field.type.initialize(this._getConnection(), this);
      if (field.type.exposeToModel())
        this._constructField(fieldName, field);
    });
  }

  /// This method simply creates the getters/setters
  /// for fields as described by <see>Model._constructFields</see>.
  _constructField(fieldName, field) {
    Object.defineProperties(this, {
      [fieldName]: {
        enumberable:  false,
        configurable: true,
        get:          () => {
          return this._getFieldValue(fieldName, field);
        },
        set:          (value) => {
          this._setFieldValue(fieldName, field, value);
        },
      },
    });
  }

  /// Initialize the field value for each field/attribute.
  /// If no `data` argument is passed to the constructor,
  /// then each field will be initialized to its default
  /// value, as defined by the `defaultValue` property on
  /// each field. A `null` value is a valid value, and
  /// though it will still be piped through each field's
  /// `field.type.castValue` method, it will discount the
  /// need to call `defaultValue` on the field.
  /// This method calls <see>Model._initializeFieldData</see>
  /// to actually set the value of each field. Field values
  /// are set from the provided `data` object to the `constructor`
  /// first, for all fields, before any `defaultValue` calls
  /// for any fields are attempted.
  ///
  /// Note:
  ///   All fields have already been constructed before this
  ///   method is called via <see>Model._constructFields</see>.
  /// Note:
  ///   Virtual fields are not constructed against the model instance,
  ///   however, field types *can* modify the instance, and some do.
  ///   For example, the `Types.Model` and `Types.Models` virtual field
  ///   types inject relational methods on the model.
  /// Note:
  ///   Only concrete fields are considered "attributes" of the model,
  ///   and can have an initial value provided via the `data` argument
  ///   to the constructor, or via a `defaultValue` property on the field.
  /// Arguments:
  ///   data?: object
  ///     Attributes values for fields. Can be nothing, in which case
  ///     the `defaultValue` property for each field will be used instead.
  ///     `null` is a valid value, and if an attribute has a `null` value,
  ///     it will not be given a default value from `defaultValue`.
  _initializeModelData(_data) {
    let dirtyFieldData  = this._dirtyFieldData;
    let data            = _data || {};

    // First initialize field values from data
    this.iterateFields(({ field, fieldName }) => {
      if (!field.type.exposeToModel())
        return;

      let fieldValue = data[fieldName];
      if (fieldValue === undefined)
        return;

      dirtyFieldData[fieldName] = fieldValue;
    });

    // Next initialize default values
    this.iterateFields(({ field, fieldName }) => {
      field.type.initialize(this._getConnection(), this);
      if (!field.type.exposeToModel())
        return;

      let fieldValue = (data) ? data[fieldName] : undefined;
      this._initializeFieldData(fieldName, field, fieldValue, data);
    });
  }

  /// This casts a field value to its type,
  /// by calling the field's `field.type.castValue`
  /// method for the type.
  ///
  /// Return: any
  ///   The value provided, cast to the type of the field.
  /// Arguments:
  ///   field: <see>Field</see>
  ///     The field to use to cast the value. The `type` property
  ///     on the field will be accessed, and the `type` properties
  ///     `castValue` method will be called on the provided `value`
  ///     argument.
  ///   value: any
  ///     The value to cast to the field's type. `null` is a valid
  ///     value, and all `castValue` methods on types should properly
  ///     handle it.
  _castFieldValue(field, value) {
    let type = field.type;
    if (!type)
      return value;

    return type.castToType({
      connection:     this.getConnection(),
      Model:          this.getModel(),
      self:           this,
      field,
      value,
    });
  }

  /// Set each field's initial value. Each field's value
  /// will first be fetched from the `data` argument
  /// given to the `constructor`--if any was given. After
  /// all field attributes have been set this way, then
  /// this method is called, being provided that value (if
  /// any) via the `fieldValue` argument. If `fieldValue`
  /// is `undefined`, then call the `defaultValue` property
  /// of the field to get the field's initial value.
  ///
  /// Arguments:
  ///   fieldName: string
  ///     The name of the field we are initializing.
  ///   field: <see>Field</see>
  ///     The field definition itself for the field
  ///     we are initializing.
  ///   fieldValue: any
  ///     The field value as provided by the `data`
  ///     argument to the model `constructor`, if
  ///     any was provided.
  ///   data: object
  ///     The `data` argument, as provided to the
  ///     `constructor` of the model.
  _initializeFieldData(fieldName, field, fieldValue, data) {
    let dirtyFieldData  = this._dirtyFieldData;
    let fieldData       = this._fieldData;
    let defaultValue    = fieldValue;

    // If the attribute given by "data" is a function
    // then we always want to call it
    if (typeof defaultValue === 'function')
      defaultValue = defaultValue({ field, fieldName, fieldValue, data, _initial: true });

    // If data provided no value, then fallback
    // to trying "defaultValue" key from field schema
    if (defaultValue === undefined)
      defaultValue = field.defaultValue;

    if (typeof defaultValue === 'function') {
      const shouldRunDefaultValueOnInitialize = () => {
        // No flags means we are running on initialize
        if (!defaultValue.mythixFlags)
          return true;

        // Are we running on initialize?
        if (defaultValue.mythixFlags & DefaultHelpers.FLAG_ON_INITIALIZE)
          return true;

        return false;
      };

      if (shouldRunDefaultValueOnInitialize()) {
        defaultValue = defaultValue({
          model:              this,
          self:               this,
          connection:         this.getConnection(),
          _initial:           true,
          field,
          fieldName,
          fieldValue,
          data,
        });
      } else {
        defaultValue = undefined;
      }
    }

    let initialValue;

    if (defaultValue === undefined || !data) {
      initialValue = defaultValue;

      if (defaultValue != null)
        initialValue = this._castFieldValue(field, defaultValue);

      fieldData[fieldName] = initialValue;
    } else {
      initialValue = this._castFieldValue(field, defaultValue);
      dirtyFieldData[fieldName] = initialValue;
    }

    field.type.onSetFieldValue({
      self:   this,
      value:  initialValue,
      field,
      fieldName,
    });
  }

  /// Get the dirty fields of the model instance.
  /// The model instance property `Model.changes`
  /// calls this method to get the list of dirty
  /// fields for the model instance. Unlike
  /// `Model.changes`, which is a getter, you can
  /// pass `options` to this method, which can change
  /// its behavior.
  ///
  /// If this method is provided a <see>Connection</see> via the
  /// `connection` options value, then it will assume it is being
  /// used in a connection method to fetch the dirty fields for
  /// a database operation (update, or insert). If it has a
  /// <see>Connection</see> then it will behave a little differently.
  ///
  /// The first difference is that if a model field is not marked
  /// as dirty, then it will be passed through <see>connection.dirtyFieldHelper</see>.
  /// If this method returns any value other than `undefined`, then
  /// that value will be used as the "dirty" value for the field, and
  /// the field will be marked as dirty. This can be used when you
  /// want the connection itself to control the dirty state of a field.
  /// This is what is done with SQLite BigInt AUTOINCREMENT emulation.
  /// Any time an autoincrementing BigInt is being stored to a SQLite
  /// database with the `emulateBigIntAutoIncrement` option enabled,
  /// an internal counter for this field is incremented, and the field
  /// is automatically updated as "dirty" (on insert only, if the field
  /// has and `undefined` value).
  ///
  /// The next difference when using a `connection` in this method is
  /// that it will check if this is an "update" or an "insert" operation
  /// by checking the `update` and `insert` "option" keys respectively.
  /// If one of these is `true`, then it will invoke `defaultValue` property
  /// on the field, if said `defaultValue` has the `onUpdate`, or `onInsert`
  /// flags set to `true`. This can be used for example on `created_at`,
  /// and `updated_at` fields, which will only be marked dirty and given
  /// a value on an `insert` or `update` operation through the connection.
  ///
  /// Without a `connection` provided to this method, it will simply
  /// return the "change list" of all dirty fields on the model. The
  /// "change list" is an object, where each key is the name of the field,
  /// and each value is another object, with `previous` and `current` keys
  /// describe the change that took place to the field. For example:
  /// ```javascript
  /// DirtyFieldChangeList = {
  ///   firstName: {
  ///     previous: undefined,
  ///     current: 'Bob',
  ///   },
  /// }
  /// ```
  ///
  /// If no fields on the model are dirty, then an empty object
  /// will be returned. A field is considered "dirty" if its value
  /// is different from what it was when the model was first instantiated,
  /// or since the last time the <see>Model.clearDirty</see> method was
  /// called.
  ///
  /// The `options` argument has the
  /// following shape:
  ///
  /// Interface:
  ///   interface DirtyFieldOptions {
  ///     connection: Connection; // The connection for this operation.
  ///     update: boolean;        // If `true`, then this is an update operation.
  ///     insert: boolean;        // If `true`, then this is an insert operation.
  ///     // ... any options you wish to pass to `connection.dirtyFieldHelper`
  ///   }
  ///
  /// Note:
  ///   If this is an "update" or "insert" operation, as defined by
  ///   the `update` or `insert` "options", then the default value
  ///   for the field will be fetched via the <see>QueryGenerator.getFieldDefaultValue</see>
  ///   method through the provided `connection`.
  /// Return: DirtyFieldChangeList
  /// Arguments:
  ///   options?: DirtyFieldOptions
  ///     Options to provide to the method.
  _getDirtyFields(_options) {
    let options         = _options || {};
    let fieldData       = this._fieldData;
    let dirtyFieldData  = this._dirtyFieldData;
    let dirtyFields     = {};
    let connection      = this.getConnection();
    let queryGenerator  = (connection) ? connection.getQueryGenerator() : null;

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      if (Object.prototype.hasOwnProperty.call(dirtyFieldData, fieldName)) {
        dirtyFields[fieldName] = { previous: fieldData[fieldName], current: dirtyFieldData[fieldName] };
        return;
      }

      // Does the type itself report that we are dirty?
      let value = field.type.isDirty({
        self:   this,
        value:  this.getDataValue(fieldName),
        field,
        fieldName,
        connection,
      });

      if (value !== undefined) {
        // Cache this result so subsequent calls
        // to dirty fields doesn't get a new value
        dirtyFieldData[fieldName] = value;

        dirtyFields[fieldName] = {
          previous: fieldData[fieldName],
          current:  value,
        };

        return;
      }

      // Does the connection dirtyFieldHelper report that we are
      // dirty?
      if (connection && typeof connection.dirtyFieldHelper === 'function') {
        let value = connection.dirtyFieldHelper({ options, fieldData, dirtyFieldData, dirtyFields, field, fieldName });
        if (value !== undefined) {
          // Cache this result so subsequent calls
          // to dirty fields doesn't get a new value
          dirtyFieldData[fieldName] = value;

          dirtyFields[fieldName] = {
            previous: fieldData[fieldName],
            current:  value,
          };

          return;
        }
      }

      if (!queryGenerator)
        return;

      if (options.update && DefaultHelpers.checkDefaultValueFlags(field.defaultValue, [ 'onUpdate' ])) {
        let value = queryGenerator.getFieldDefaultValue(field, fieldName, Object.assign({ isUpdateOperation: true, rawLiterals: true, escape: false }));

        // Cache this result so subsequent calls
        // to dirty fields doesn't get a new value
        dirtyFieldData[fieldName] = value;

        dirtyFields[fieldName] = {
          previous: fieldData[fieldName],
          current:  value,
        };
      } else if (options.insert && DefaultHelpers.checkDefaultValueFlags(field.defaultValue, [ 'onInsert' ])) {
        let value = queryGenerator.getFieldDefaultValue(field, fieldName, Object.assign({ isInsertOperation: true, rawLiterals: true, escape: false }));

        // Cache this result so subsequent calls
        // to dirty fields doesn't get a new value
        dirtyFieldData[fieldName] = value;

        dirtyFields[fieldName] = {
          previous: fieldData[fieldName],
          current:  value,
        };
      }
    });

    return dirtyFields;
  }

  /// Get a field's value. Calling this is no different
  /// from accessing the field directly on the model instance
  /// itself. For example, `userInstance.firstName` is identical
  /// to calling `userInstance._getFieldValue('firstName', User.fields.firstName)`.
  ///
  /// This will call the `get` method defined on the field, if any is defined.
  /// If a `get` method is defined, then whatever value it returns will be
  /// the result of this method. If no `get` method is defined on the field,
  /// then `this.getDataValue(fieldName)` is called instead, and will provide
  /// the resulting return value for this method.
  ///
  /// Note:
  ///   Never call this method (or the model attribute by name), while
  ///   inside a field's `get` method. If you do, you will enter an
  ///   infinite recursive loop. Instead, call the provided `get`
  ///   method (as provided by the context), or call <see>Model.getDataValue</see>.
  /// Return: any
  /// Arguments:
  ///   fieldName: string
  ///     The name of the field whose value we wish to get.
  ///   field: <see>Field</see>
  ///     The field definition itself for the value we wish to get.
  /// See: Field
  _getFieldValue(fieldName, field) {
    let value = this.getDataValue(fieldName);

    if (typeof field.get === 'function') {
      return field.get.call(this, {
        model:  this,
        self:   this,
        set:    this.setDataValue.bind(this, fieldName),
        get:    this.getDataValue.bind(this, fieldName),
        value,
        field,
        fieldName,
      });
    }

    return value;
  }

  /// Set a field's value. Calling this is no different
  /// from setting the field directly on the model instance
  /// itself. For example, `userInstance.firstName = 'Bob'` is identical
  /// to calling `userInstance._setFieldValue('firstName', User.fields.firstName, 'Bob')`.
  ///
  /// This will call the `set` method defined on the field, if any is defined.
  /// If a `set` method is defined, then it is entirely up to the `set` method
  /// to call `this.setDataValue` to set the field's value.
  /// If no `set` method is defined on the field, then `this.setDataValue(fieldName, value)`
  /// is called instead, and will set the value on the field.
  ///
  /// Note:
  ///   This method will mark the field as dirty if the value provided
  ///   differs from the current value the field has.
  /// Note:
  ///   Never call this method (or set the model attribute by name), while
  ///   inside a field's `set` method. If you do, you will enter an
  ///   infinite recursive loop. Instead, call the provided `set`
  ///   method (as provided by the context), or call <see>Model.setDataValue</see>.
  /// Return: undefined
  /// Arguments:
  ///   fieldName: string
  ///     The name of the field whose value we wish to get.
  ///   field: <see>Field</see>
  ///     The field definition itself for the value we wish to get.
  /// See: Field
  _setFieldValue(fieldName, field, value) {
    if (typeof field.set === 'function') {
      field.set.call(this, {
        model:  this,
        self:   this,
        set:    this.setDataValue.bind(this, fieldName),
        get:    this.getDataValue.bind(this, fieldName),
        value,
        field,
        fieldName,
      });

      return;
    }

    this.setDataValue(fieldName, value);
  }

  /// Check if the model is persisted or not.
  /// This has nothing to do with the model's "dirty" state.
  /// if the model is dirty, then this will still report
  /// `true` if the model was initially loaded from the database.
  ///
  /// Return: boolean
  ///   `true` if the model was loaded from the database,
  ///   `false` otherwise.
  isPersisted() {
    return this._persisted;
  }

  /// Update the `this.dirtyID` cache key of the model.
  /// This is called any time a field is marked as "dirty" on the model.
  /// The `dirtyID` property of a model is an instance of
  /// a <see>CacheKey</see> that is intended to be used for
  /// caching systems. The <see>CacheKey</see> instance can
  /// be used for a key into a WeakMap, which can cache things.
  /// Because this is updated to a new instance of a <see>CacheKey</see>
  /// any time *any* field on the model is marked as dirty,
  /// any cache for this model will be invalidated as soon
  /// as any field on the model is dirty.
  updateDirtyID() {
    this.dirtyID = new CacheKey(this.dirtyID);
  }

  /// Check if the model is dirty or not.
  /// If a `fieldName` is provided as an argument,
  /// it will only check if the specified field
  /// is dirty or not. If not provided, then the
  /// entire model will be considered "dirty" if
  /// one or more fields are marked as "dirty".
  ///
  /// Return: boolean
  /// Arguments:
  ///   fieldName?: string
  ///     If provided, check if this one field is dirty. If not
  ///     provided, then check if any field on the model is dirty.
  isDirty(fieldName) {
    if (!fieldName) {
      // Do this check first, because
      // it takes way less computation
      let isDirty = (Object.keys(this._dirtyFieldData).length > 0);
      if (isDirty)
        return true;

      let changes = this.changes;
      return (Object.keys(changes).length > 0);
    } else {
      // Do this check first, because
      // it takes way less computation
      let hasDirtyField = Object.prototype.hasOwnProperty.call(this._dirtyFieldData, fieldName);
      if (hasDirtyField)
        return true;

      let changes = this.changes;
      return Object.prototype.hasOwnProperty.call(changes, fieldName);
    }
  }

  /// Clear the dirty state of the model, or of a single field.
  /// If a `fieldName` argument is provided, then only the specified
  /// field's "dirty" status will be cleared. If no `fieldName`
  /// argument is provided, then all field's on the model will have
  /// their "dirty" status cleared, and the model will no longer be
  /// "dirty".
  ///
  /// The "dirty" status of a field is cleared by 1) setting the real
  /// underlying field value (this._fieldData) to the dirty field value,
  /// and 2) removing the entry in `this._dirtyFieldData` on the model for
  /// the field.
  ///
  /// Return: boolean
  /// Arguments:
  ///   fieldName?: string
  ///     If specified, clear only this field's "dirty" status.
  ///     If not specified, clear all fields on the model, making
  ///     the entire model no longer dirty.
  clearDirty(fieldName) {
    if (fieldName && Object.prototype.hasOwnProperty.call(this._dirtyFieldData, fieldName)) {
      this._fieldData[fieldName] = this._dirtyFieldData[fieldName];
      delete this._dirtyFieldData[fieldName];
    } else {
      Object.assign(this._fieldData, this._dirtyFieldData);
      this._dirtyFieldData = {};
    }

    this.updateDirtyID();
  }

  /// Get the dirty field descriptors themselves
  /// for all fields that are dirty.
  ///
  /// Whereas <see>Model._getDirtyFields</see> returns
  /// a "change list" of dirty fields, this method
  /// will return the field descriptors themselves.
  /// An array of <see>Field</see> will be returned
  /// for each dirty field, or an empty array if no
  /// fields are dirty.
  ///
  /// Return: Array<<see>Field</see>>
  ///   The dirty fields of the model.
  /// Arguments:
  ///   options?: object
  ///     If provided, this will be the "options" supplied to
  ///     <see>Model._getDirtyFields</see>.
  getDirtyFields(options) {
    let modelChanges    = this._getDirtyFields(options);
    let dirtyFieldNames = Object.keys(modelChanges);

    return this.getFields(dirtyFieldNames);
  }

  /// Get the current value of any field on the model.
  /// The "current" value will be the dirty value if
  /// the field is dirty, or it will be the clean
  /// value of the field if the field isn't dirty.
  ///
  /// This method is a "raw" get of the field's value
  /// in that it won't call `get` on the field descriptor,
  /// if any is defined. Instead it will first attempt to
  /// fetch the current value of the field from `this._dirtyFieldData`,
  /// if there is an entry for the field in the `this._dirtyFieldData`
  /// property of the model instance. If there is no "dirty" entry for
  /// the field, then its "clean value" (or raw value) will be fetched
  /// from the underlying `this._fieldData` storage area for field values.
  ///
  /// Return: any
  /// Arguments:
  ///   fieldName: string
  ///     The name of the field whose value you wish to fetch.
  getDataValue(fieldName) {
    let fieldData       = this._fieldData;
    let dirtyFieldData  = this._dirtyFieldData;
    let value;

    if (Object.prototype.hasOwnProperty.call(dirtyFieldData, fieldName))
      value = dirtyFieldData[fieldName];
    else
      value = fieldData[fieldName];

    return value;
  }

  /// Set the current value of any field on the model.
  /// The "current" value will be the dirty value if
  /// the field is dirty, or it will be the clean
  /// value of the field if the field isn't dirty.
  ///
  /// This method is a "raw" set of the field's value
  /// in that it won't call `set` on the field descriptor,
  /// if any is defined. Instead, it will mark the field as
  /// dirty by setting the value provided as an entry for
  /// the field in the `this._dirtyFieldData` property of
  /// the model instance. If the value set is equal to the
  /// "clean value" of the field, as found in `this._fieldData`,
  /// then the dirty entry for the field will be cleared (deleted)
  /// instead.
  ///
  /// Note:
  ///   This method will always call <see>Model.updateDirtyID</see>,
  ///   invalidating all down-stream cache for this model instance.
  /// Return: undefined
  /// Arguments:
  ///   fieldName: string
  ///     The name of the field whose value you wish to set.
  ///   value: any
  ///     Value to set the field to. The will be sent through
  ///     `field.type.castValue` to cast the incoming value
  ///     to the field's type.
  setDataValue(fieldName, value) {
    let fieldData       = this._fieldData;
    let dirtyFieldData  = this._dirtyFieldData;
    let field           = this.getField(fieldName);

    if (!field)
      throw new Error(`${this.getModelName()}::setDataValue: Unable to find field named "${fieldName}".`);

    let newValue = this._castFieldValue(field, value);

    field.type.onSetFieldValue({
      self:   this,
      value:  newValue,
      field,
      fieldName,
    });

    // If the values are exactly the same,
    // then we are no longer dirty, and
    // can just return
    if (fieldData[fieldName] === newValue) {
      delete dirtyFieldData[fieldName];

      this.updateDirtyID();

      return;
    }

    dirtyFieldData[fieldName] = newValue;

    this.updateDirtyID();
  }

  /// Get all the models current raw attributes.
  /// This will return an object, where each key
  /// is the `fieldName` of each non-virtual (concrete)
  /// field on the model, and each value is the "current"
  /// value of the field on the model. The "current"
  /// value for each field could either be a dirty field
  /// value, or a clean (or raw) field value.
  ///
  /// This method is nearly identical to <see>Model.toJSON</see>
  /// except that <see>Model.toJSON</see> always adds the primary
  /// key to the result first, which only matters for JavaScript
  /// engines that honor the insertion order of keys in native
  /// Objects, which is never guaranteed. <see>Model.toJSON</see>
  /// also differs from this method by calling `field.type.serialize`
  /// for every attribute on the model.
  ///
  /// Note:
  ///   Field's with `undefined` values will not be included in the
  ///   resulting object.
  /// Return: object
  ///   All models attributes (field values).
  getAttributes() {
    let result = {};

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      let fieldValue = this[fieldName];
      if (fieldValue === undefined)
        return;

      result[fieldName] = fieldValue;
    });

    return result;
  }

  /// Set attributes on the model via the provided object.
  /// The provided object should contain keys that are
  /// field names for each field you wish to set. The
  /// value of each key will be what is set onto the field.
  /// This method ignores virtual fields, so if any virtual
  /// field names are supplied, then will be silently skipped.
  /// This method is identical to calling `modelInstance[fieldName] = value`
  /// on every key in the object provided.
  ///
  /// Note:
  ///   Any `undefined` value in the provided object will be
  ///   ignored, and no update will occur for the field.
  /// Note:
  ///   If a raw object instead of a model instance is provided,
  ///   then `hasOwnProperty` is called for each field name,
  ///   and if the provided object doesn't contain its "own"
  ///   property matching the field name, the field will be
  ///   silently skipped, and not set.
  /// Return: undefined
  /// Arguments:
  ///   attributes: object | Model
  ///     The attributes to set on the model. The keys
  ///     of the model should be field names.
  ///   noPrimaryKey: boolean
  ///     If `true`, then the primary key of the model
  ///     (if any is defined) will be skipped, and won't
  ///     be set, even if the provided `attributes` argument
  ///     contains a value for the primary key.
  setAttributes(attributes, noPrimaryKey) {
    let isObject = Nife.instanceOf(attributes, 'object');

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      if (field.primaryKey === true && noPrimaryKey === true)
        return;

      if (isObject && !Object.prototype.hasOwnProperty.call(attributes, fieldName))
        return;

      let fieldValue = attributes[fieldName];
      if (fieldValue === undefined)
        return;

      this[fieldName] = fieldValue;
    });
  }

  /// Checks if the primary key's value is valid.
  /// Validity checks are handled by the field's `type`.
  /// When this method is called, the primary key
  /// field for the model is fetched, and if found,
  /// the `field.type.isValidValue` method will be called
  /// and provided with the current value of the
  /// primary key field.
  ///
  /// <see>Type.isValidValue</see>
  /// is commonly implemented to be a strict check
  /// against the type. `null` will generally
  /// return `false` for an `isValidValue` call, however
  /// implementation details are entirely left up to
  /// the `isValidValue` method defined on the type.
  ///
  /// Return: boolean
  hasValidPrimaryKey() {
    let pkField = this.getPrimaryKeyField();
    if (!pkField)
      return false;

    let pkFieldName = pkField.fieldName;
    let pkValue     = this[pkFieldName];
    if (pkValue == null)
      return false;

    return pkField.type.isValidValue(pkValue, {
      connection:     this.getConnection(),
      Model:          this.getModel(),
      self:           this,
    });
  }

  /// Validate all concrete fields on the model.
  /// This method is called before any "insert"
  /// or "update" operation happens for the model.
  /// It will iterate all the fields of the model,
  /// and call the `validate` method defined on
  /// each field descriptor (<see>Field</see>),
  /// if any is defined.
  /// Each `validate` method is asynchronous, so
  /// all promises will be collected from all `validate`
  /// method calls, and will be awaited upon via
  /// `await Promise.all(validatePromises);`.
  ///
  /// Model validation will fail if any `validate`
  /// method throws an exception.
  ///
  /// `onValidate` is called directly from <see>Model.onBeforeSave</see>,
  /// so make certain you call `super.onBeforeSave(...args)` if you
  /// overload `onBeforeSave`. If you don't, your validation hooks
  /// won't be called. This design was deliberate, so the developer
  /// could choose to bypass model validation on purpose simply
  /// by not calling `super.onBeforeSave(...args)` should they choose not to.
  ///
  /// Feel free to overload `onValidate` to provide your own implementation.
  ///
  /// Note:
  ///   `validate` properties on fields **must** always be methods.
  ///   Unlike other ORMs, we don't allow a pattern, a RegExp, or
  ///   anything else. You must provide the implementation of each
  ///   `validate` method directly yourself (which could for example
  ///   use a RegExp to check the field's value).
  /// Return: Array<any>
  ///   Results of each `validate` call from each field. Will generally
  ///   be `undefined` for each call. The validation for a field will
  ///   only fail if an exception is thrown. `false` or `null` as a
  ///   return value from a `validate` call is meaningless
  ///   (unless you yourself wish to do something with it).
  /// Arguments:
  ///   context: object
  ///     A context object that is provided to all model hooks.
  ///     The shape of this object is `{ connection, Model, options }`.
  ///     It will contain the current `connection` for the operation
  ///     (which might be a transaction connection), the `Model` for
  ///     the operation, which is the "root model" of the operation,
  ///     not necessarily "this" model (the model whose hook is being
  ///     called), and the "options" defined for the current operation.
  async onValidate(context) {
    let promises = [];

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      if (typeof field.validate !== 'function')
        return;

      try {
        let fieldValue  = this[fieldName];
        let promise     = field.validate.call(this, fieldValue, context);
        if (!Nife.instanceOf(promise, 'promise'))
          promise = Promise.resolve(promise);

        promises.push(promise);
      } catch (error) {
        promises.push(Promise.reject(error));
      }
    });

    await Promise.all(promises);
  }

  /// Before insert model hook.
  ///
  /// This method will be called on each model instance
  /// before it is inserted into the database. This method
  /// is called before dirty fields are calculated for
  /// the insert operation, so you can set field values
  /// here before insert, and they will be captured and
  /// inserted into the database.
  ///
  /// Arguments:
  ///   context: object
  ///     A context object that is provided to all model hooks.
  ///     The shape of this object is `{ connection, Model, options }`.
  ///     It will contain the current `connection` for the operation
  ///     (which might be a transaction connection), the `Model` for
  ///     the operation, which is the "root model" of the operation,
  ///     not necessarily "this" model (the model whose hook is being
  ///     called), and the "options" defined for the current operation.
  // eslint-disable-next-line no-unused-vars
  async onBeforeCreate(context) {
  }

  /// Before update model hook.
  ///
  /// This method will be called on each model instance
  /// before it is updated in the database. This method
  /// is called before dirty fields are calculated for
  /// the update operation, so you can set field values
  /// here before update, and they will be captured and
  /// updated in the database.
  ///
  /// Arguments:
  ///   context: object
  ///     A context object that is provided to all model hooks.
  ///     The shape of this object is `{ connection, Model, options }`.
  ///     It will contain the current `connection` for the operation
  ///     (which might be a transaction connection), the `Model` for
  ///     the operation, which is the "root model" of the operation,
  ///     not necessarily "this" model (the model whose hook is being
  ///     called), and the "options" defined for the current operation.
  // eslint-disable-next-line no-unused-vars
  async onBeforeUpdate(context) {
  }

  /// Before save model hook.
  ///
  /// This method will be called on each model instance
  /// before it is inserted or updated in the database.
  /// This method is called before dirty fields are calculated
  /// for the insert or update operation, so you can set field
  /// values here before update, and they will be captured and
  /// updated in the database.
  ///
  /// Note:
  ///   This is the final hook call before the insert or update
  ///   operation sends your data off to the database.
  ///
  /// Arguments:
  ///   context: object
  ///     A context object that is provided to all model hooks.
  ///     The shape of this object is `{ connection, Model, options }`.
  ///     It will contain the current `connection` for the operation
  ///     (which might be a transaction connection), the `Model` for
  ///     the operation, which is the "root model" of the operation,
  ///     not necessarily "this" model (the model whose hook is being
  ///     called), and the "options" defined for the current operation.
  // eslint-disable-next-line no-unused-vars
  async onBeforeSave(context) {
    await this.onValidate(context);
  }

  /// After insert model hook.
  ///
  /// This method will be called on each model instance
  /// after it has been inserted into the database.
  /// This method is called **after** each model instance
  /// has been fully stored, marked as persisted, and marked
  /// as not dirty. If you set any model fields in this method,
  /// then the model will be marked as dirty, and will be
  /// re-saved at the end of the insert operation.
  ///
  /// Note:
  ///   **Use care when updating model attributes in this method**.
  ///   At the end of each insert operation, the models just stored
  ///   are scanned to see if any of them are dirty. If they are dirty
  ///   they will be re-stored to the database using an update operation.
  ///   This can happen for example if foreign keys are in use. We might
  ///   not be able to update a foreign key field value until after the insert
  ///   operation is completed. When a foreign key value is updated
  ///   post-insert, then the model might again become dirty, and be
  ///   updated post-insert. Because of this, you should be careful
  ///   about making your model dirty in any "after" hooks, as you
  ///   might unintentionally incur the cost of a post-insert update.
  ///
  /// Arguments:
  ///   context: object
  ///     A context object that is provided to all model hooks.
  ///     The shape of this object is `{ connection, Model, options }`.
  ///     It will contain the current `connection` for the operation
  ///     (which might be a transaction connection), the `Model` for
  ///     the operation, which is the "root model" of the operation,
  ///     not necessarily "this" model (the model whose hook is being
  ///     called), and the "options" defined for the current operation.
  // eslint-disable-next-line no-unused-vars
  async onAfterCreate(context) {
  }

  /// After update model hook.
  ///
  /// This method will be called on each model instance
  /// after it has been update in the database.
  /// This method is called **after** each model instance
  /// has been fully stored, marked as persisted, and marked
  /// as not dirty. Unlike an insert operation, update operations
  /// do not have a post-operation dirty model scan and re-update.
  /// You can safely update model attributes in this method without
  /// the worry of incurring the cost of an extra store operation.
  /// However, if you do set model attributes in this method, then
  /// the model will be marked dirty when the update operation is complete.
  ///
  /// Arguments:
  ///   context: object
  ///     A context object that is provided to all model hooks.
  ///     The shape of this object is `{ connection, Model, options }`.
  ///     It will contain the current `connection` for the operation
  ///     (which might be a transaction connection), the `Model` for
  ///     the operation, which is the "root model" of the operation,
  ///     not necessarily "this" model (the model whose hook is being
  ///     called), and the "options" defined for the current operation.
  // eslint-disable-next-line no-unused-vars
  async onAfterUpdate(context) {
  }

  /// After insert or update model hook.
  ///
  /// This method will be called on each model instance
  /// after it has been inserted or updated in the database.
  /// This method is called **after** each model instance
  /// has been fully stored, marked as persisted, and marked
  /// as not dirty. If this is an insert operation, and
  /// if you set any model fields in this method,
  /// then the model will be marked as dirty, and will be
  /// re-saved at the end of the insert operation.
  ///
  /// Note:
  ///   **Use care when updating model attributes in this method**.
  ///   At the end of each insert operation, the models just stored
  ///   are scanned to see if any of them are dirty. If they are dirty
  ///   they will be re-stored to the database using an update operation.
  ///   This can happen for example if foreign keys are in use. We might
  ///   not be able to update a foreign key field value until after the insert
  ///   operation is completed. When a foreign key value is updated
  ///   post-insert, then the model might again become dirty, and be
  ///   updated post-insert. Because of this, you should be careful
  ///   about making your model dirty in any "after" hooks, as you
  ///   might unintentionally incur the cost of a post-insert update.
  ///
  /// Arguments:
  ///   context: object
  ///     A context object that is provided to all model hooks.
  ///     The shape of this object is `{ connection, Model, options }`.
  ///     It will contain the current `connection` for the operation
  ///     (which might be a transaction connection), the `Model` for
  ///     the operation, which is the "root model" of the operation,
  ///     not necessarily "this" model (the model whose hook is being
  ///     called), and the "options" defined for the current operation.
  // eslint-disable-next-line no-unused-vars
  async onAfterSave(context) {
  }

  /// Persist the model to the database.
  ///
  /// This method will either initiate an insert
  /// operation against the model (if `this.isPersisted()`
  /// returns `false`), or initiate an update operation (
  /// if `this.isPersisted()` returns `true`).
  ///
  /// This method will do nothing, and immediately return
  /// `false` if the model is not dirty. If you provide
  /// the option `force: true` as a property in the `options`
  /// argument, then all fields will forcefully be marked as
  /// dirty, using the current field's value as the "dirty"
  /// value for each field, and then the model will be either
  /// inserted or updated, based on the result from `isPersisted`.
  /// Using `force: true` will essentially send and insert or
  /// update operation through the connection that will update
  /// ALL columns for the model row in the database, regardless
  /// of if they were actually dirty or not.
  ///
  /// At the end of this method--assuming the operation was successful--
  /// `this.clearDirty()` will be called on the model instance to clear its
  /// dirty state, and then the model will be marked as persisted.
  /// Finally, `true` will be returned, letting the caller
  /// know the operation completed successfully.
  ///
  /// Return: boolean
  ///   `true` if the model was persisted through the database,
  ///   or `false` otherwise. `false` doesn't mean there was an
  ///   error. Errors will be thrown. `false` simply means that
  ///   the model had no dirty fields, so it was never sent to
  ///   the database.
  /// Arguments:
  ///   options?: object
  ///     Options to pass to the underlying connection
  ///     <see>Connection.insert</see> or <see>Connection.update</see>
  ///     call. If a `connection` property is present on this
  ///     options object, then it will be used as the connection
  ///     for this operation.
  async save(_options) {
    let options = _options || {};

    if (this.isPersisted() && options.force !== true && Nife.isEmpty(this.changes)) {
      return false;
    } else if (options.force) {
      // Mark all fields as dirty
      let dirtyFieldData = this._dirtyFieldData;

      this.iterateFields(({ field, fieldName }) => {
        if (field.type.isVirtual())
          return;

        dirtyFieldData[fieldName] = this[fieldName];
      });
    }

    let connection = this.getConnection(options.connection);

    if (this.isPersisted())
      await connection.update(this.getModel(), [ this ], options);
    else
      await connection.insert(this.getModel(), [ this ], options);

    this.clearDirty();

    return this;
  }

  /// Reload the model in-place.
  ///
  /// This method will create a query selecting this model
  /// from the database by its primary key. It will then
  /// take the columns from the select operation, and will
  /// apply the values to the model's fields in-place.
  ///
  /// If a row is returned from the database, then the model's
  /// fields will be updated, `this.clearDirty()` will be
  /// called to clear the dirty status of the model, and
  /// finally the model will be marked as persisted.
  ///
  /// If no row is returned from the database, then this method
  /// will immediately return, and the model won't be updated.
  ///
  /// If the model has no primary key field defined, then an
  /// exception will be thrown.
  ///
  /// Note:
  ///   A primary key is required on the model for this method
  ///   to work. If you don't have a primary key defined, then
  ///   you will need to create your own "reload" operation.
  /// Return: undefined
  /// Arguments:
  ///   options?: object
  ///     Options to pass to the underlying <see>QueryEngine.first</see>
  ///     call. If a `connection` property is present on this options object,
  ///     then it will be used as the connection for this operation.
  async reload(options) {
    let pkFieldName = this.getPrimaryKeyFieldName();
    if (!pkFieldName)
      throw new Error(`${this.getModelName()}::reload: Unable to reload models that have no primary key defined.`);

    let id = this[pkFieldName];
    if (!id)
      return;

    let storedModel = await this.getWhereWithConnection(options)[pkFieldName].EQ(id).first();
    if (!storedModel)
      return;

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      let fieldValue = storedModel[fieldName];
      this[fieldName] = fieldValue;
    });

    this._persisted = true;
    this.clearDirty();
  }

  /// Destroy this model instance.
  ///
  /// Calling this method will destroy this model from the database.
  /// If the model is not persisted, then nothing will happen, and
  /// `0` will be returned. If the model is persisted, the result
  /// of <see>Connection.destroy</see> is returned, which is the number
  /// of rows modified in the database, which in this case should always
  /// be `1`.
  ///
  /// If the model has no primary key field defined, then an
  /// exception will be thrown.
  ///
  /// Note:
  ///   A primary key is required on the model for this method
  ///   to work. If you don't have a primary key defined, then
  ///   you will need to create your own "destroy" operation.
  /// Return: number
  ///   The number of rows modified in the database.
  /// Arguments:
  ///   options?: object
  ///     Options to pass to the underlying <see>QueryEngine.destroy</see>
  ///     call. If a `connection` property is present on this options object,
  ///     then it will be used as the connection for this operation.
  async destroy(options) {
    if (!this.isPersisted())
      return 0;

    let primaryKeyFieldName = this.getPrimaryKeyFieldName();
    if (!primaryKeyFieldName || !this[primaryKeyFieldName])
      throw new Error(`${this.getModelName()}::destroy: Model has no primary key field, or value. Refusing to destroy this model to prevent possible data loss. Please call "connection.destroy" yourself, manually providing your own query.`);

    return await this.getModel().getWhereWithConnection(options)[primaryKeyFieldName].EQ(this.id).destroy(options);
  }

  /// Get a string representation of this model.
  ///
  /// Return: string
  ///   The model's name and attributes as a string.
  toString() {
    return `${this.getModelName()} ${JSON.stringify(this.toJSON(), undefined, 2)}`;
  }

  /// Return a raw Object of the model's attributes,
  /// ready to be passed through `JSON.stringify`. This
  /// method is nearly identical to <see>Model.getAttributes</see>
  /// except that it always inserts the primary key of the model
  /// (if the model has one) first. The other difference
  /// between this method and <see>Model.getAttributes</see>
  /// is that this method will call `field.type.serialize` on
  /// each field value.
  ///
  /// Return: object
  ///   All model attributes, ready to be serialized.
  /// See: Type.serialize
  toJSON() {
    let result  = {};
    let pkField = this.getPrimaryKeyField();

    // PK always comes first
    if (pkField) {
      let pkFieldName = pkField.fieldName;
      let fieldValue  = this[pkFieldName];

      if (fieldValue !== undefined)
        result[pkFieldName] = pkField.type.serialize(fieldValue);
    }

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      if (field.primaryKey)
        return;

      let fieldValue = this[fieldName];
      if (fieldValue === undefined)
        return;

      result[fieldName] = field.type.serialize(fieldValue);
    });

    return result;
  }

  // eslint-disable-next-line no-unused-vars
  [Symbol.for('nodejs.util.inspect.custom')]() {
    return this.toJSON();
  }
}

bindStaticWhereToModelClass(Model);

module.exports = Model;
