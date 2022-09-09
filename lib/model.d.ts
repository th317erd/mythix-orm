import { GenericObject } from "./interfaces/common";
import { ConnectionBase } from "./connection/connection-base";
import { QueryEngine } from "./query-engine";

export class CacheKey {
  constructor(number: number);
  valueOf(): number;
}

export declare interface ModelClass {
  new(data: GenericObject, options?: GenericObject): Model;
}

export declare interface Models {
  [ key: string ]: ModelClass;
}

// TODO: Needs to be removed
type Field = any;

export declare type Fields = Array<Field> | { [ key: string ]: Field } | Map<string, Field> | Set<Field>;

export declare interface IterateFieldsContext {
  field: Field;
  fieldName: string;
  fields: Fields;
  index: number;
  stop: () => void;
  isStopped: () => boolean;
}

export declare type IterateFieldsCallback = (context: IterateFieldsContext) => any;

export declare class Model {
  declare public static fields: Fields | undefined;
  declare public static _sortedFields: Array<Field> | null;
  declare public static _isMythixModel: boolean;
  declare public static _mythixBoundConnection: ConnectionBase | null;
  declare public static where: QueryEngine;
  declare public static $: QueryEngine;

  public static isModelClass(value: any): boolean;
  public static isModel(value: any): boolean;
  public static toString(showFields: boolean): string;

  public static _getConnection(connection?: ConnectionBase): ConnectionBase;
  _getConnection(connection?: ConnectionBase): ConnectionBase;

  static getConnection(connection?: ConnectionBase): ConnectionBase;
  getConnection(connection?: ConnectionBase): ConnectionBase;

  static bindConnection(connection: ConnectionBase): ModelClass;

  static getQueryEngineClass(connection) {
    return this.getConnection(connection).getQueryEngineClass();
  }

  getQueryEngineClass(connection) {
    return this.constructor.getQueryEngineClass(connection);
  }

  static getUnscopedQueryEngine(connection, options) {
    let QueryEngineClass = this.getQueryEngineClass(connection);
    let queryEngine = new QueryEngineClass(
      Object.assign(
        {},
        (options || {}),
        {
          connection: this.getConnection(connection),
        },
      ),
    );

    let modelName = this.getModelName();
    return queryEngine[ modelName ];
  }

  getUnscopedQueryEngine(connection, options) {
    return this.constructor.getUnscopedQueryEngine(connection, options);
  }

  static defaultScope(query) {
    return query;
  }

  defaultScope(queryEngine) {
    return this.constructor.defaultScope(queryEngine);
  }

  static getQueryEngine(connection, options) {
    let queryEngine = this.getUnscopedQueryEngine(connection, options);
    let defaultScope = this.defaultScope(queryEngine, options);

    return defaultScope;
  }

  getQueryEngine(connection, options) {
    return this.constructor.getQueryEngine(connection, options);
  }

  static getForeignKeyFieldsMap(_connection) {
    let connection = this.getConnection(_connection);
    let foreignKeyFieldsMap = connection._getFromModelCache(this, 'foreignKeyFieldsMap');
    if (foreignKeyFieldsMap)
      return foreignKeyFieldsMap;

    let foreignKeyFields = new Map();

    Nife.iterate(this.getFields(), ({ value: field }) => {
      let fieldName = field.fieldName;

      if (field.type.isForeignKey()) {
        let targetModelName = field.type.getTargetModelName(connection);
        let targetFieldName = field.type.getTargetFieldName(connection);
        let relationSet = foreignKeyFields.get(targetModelName);

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

  static getForeignKeysTargetModels(_connection) {
    let connection = this.getConnection(_connection);
    let foreignKeyTargetModels = connection._getFromModelCache(this, 'foreignKeyTargetModels');
    if (foreignKeyTargetModels)
      return foreignKeyTargetModels;

    let foreignKeyFieldsMap = this.getForeignKeyFieldsMap(connection);
    let foreignKeyTargetModelNames = Array.from(foreignKeyFieldsMap.keys());

    foreignKeyTargetModels = new Map();

    for (let i = 0, il = foreignKeyTargetModelNames.length; i < il; i++) {
      let modelName = foreignKeyTargetModelNames[ i ];
      let Model = connection.getModel(modelName);

      foreignKeyTargetModels.set(modelName, Model);
    }

    connection._setToModelCache(this, 'foreignKeyTargetModels', foreignKeyTargetModels);

    return foreignKeyTargetModels;
  }

  getForeignKeysTargetModels(connection) {
    return this.constructor.getForeignKeysTargetModels(connection);
  }

  static getForeignKeysTargetModelNames(_connection) {
    let connection = this.getConnection(_connection);
    let foreignKeyTargetModelNames = connection._getFromModelCache(this, 'foreignKeyTargetModelNames');
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

  static getForeignKeysTargetFieldNames(_connection, modelName) {
    let connection = this.getConnection(_connection);
    let cacheKey = `${modelName}:foreignKeyFieldNames`;
    let foreignKeyFieldNames = connection._getFromModelCache(this, cacheKey);
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

  static getForeignKeysTargetField(_connection, modelName, fieldName) {
    let connection = this.getConnection(_connection);
    let cacheKey = `${modelName}:${fieldName}:foreignKeyTargetField`;
    let foreignKeyFieldNames = connection._getFromModelCache(this, cacheKey);
    if (foreignKeyFieldNames)
      return foreignKeyFieldNames;

    let foreignKeyFieldsMap = this.getForeignKeyFieldsMap(connection);
    let fields = foreignKeyFieldsMap.get(modelName);
    if (!fields)
      return;

    let fieldInfo = fields.find((fieldInfo) => (fieldInfo.targetFieldName === fieldName));

    connection._setToModelCache(this, cacheKey, fieldInfo);

    return fieldInfo;
  }

  getForeignKeysTargetField(connection, modelName, fieldName) {
    return this.constructor.getForeignKeysTargetField(connection, modelName, fieldName);
  }

  static isForeignKeyTargetModel(_connection, modelName) {
    let connection = this.getConnection(_connection);
    let foreignKeyFieldsMap = this.getForeignKeyFieldsMap(connection);
    return foreignKeyFieldsMap.has(modelName);
  }

  isForeignKeyTargetModel(connection, modelName) {
    return this.constructor.isForeignKeyTargetModel(connection, modelName);
  }

  static getTableName(connection) {
    if (!connection) {
      let tableName = Nife.camelCaseToSnakeCase(this.getPluralModelName());
      return tableName;
    }

    let cacheKey = `${this.getModelName()}:tableName`;
    let modelTableName = connection._getFromModelCache(this, cacheKey);
    if (modelTableName)
      return modelTableName;

    let tableName = Nife.camelCaseToSnakeCase(this.getPluralModelName());

    connection._setToModelCache(this, cacheKey, tableName);

    return tableName;
  }

  getTableName(connection) {
    return this.constructor.getTableName(connection);
  }

  static getModelName() {
    return this.name;
  }

  getModelName() {
    return this.constructor.getModelName();
  }

  static getSingularName() {
    return this.getModelName();
  }

  getSingularName() {
    return this.constructor.getSingularName();
  }

  static getPluralModelName() {
    return Inflection.pluralize(this.getSingularName());
  }

  getPluralModelName() {
    return this.constructor.getPluralModelName();
  }

  static getModel() {
    return this;
  }

  getModel() {
    return this.constructor.getModel();
  }

  static getFields(fieldNames) {
    let fields = this.initializeFields(this.fields);
    if (fields !== this.fields) {
      Object.defineProperties(this, {
        'fields': {
          writable: true,
          enumerable: true,
          configurable: true,
          value: fields,
        },
        '_sortedFields': {
          writable: true,
          enumerable: true,
          configurable: true,
          value: null,
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

  static getSortedFields(fieldNames) {
    if (Object.prototype.hasOwnProperty.call(this, '_sortedFields') && this._sortedFields)
      return this._sortedFields;

    let fields = this.getFields(fieldNames);
    let sortedFields = [];
    let primaryKeyFieldName = this.getPrimaryKeyFieldName();

    Nife.iterate(fields, ({ value, context }) => context.push(value), sortedFields);

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
        obj[ key ] = value;

      return obj;
    };

    let ModelClass = this;
    let fields = this.fields;
    let clonedFields = new Map();

    Nife.iterate(fields, cloneField);
    Nife.iterate(mergeFields, cloneField);

    return (Nife.instanceOf(fields, 'array', 'set')) ? Array.from(clonedFields.values()) : mapToObject(clonedFields);
  }

  static initializeFields(fields) {
    if (!fields)
      return fields;

    if (fields._mythixFieldsInitialized)
      return fields;

    let ModelClass = this;
    let isArray = Array.isArray(fields);
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
        finalizedFields[ fieldName ] = field;
    });

    Object.defineProperties(finalizedFields, {
      '_mythixFieldsInitialized': {
        writable: true,
        enumerable: false,
        configurable: true,
        value: true,
      },
      '_primaryKeyField': {
        writable: true,
        enumerable: false,
        configurable: true,
        value: primaryKeyField,
      },
    });

    return finalizedFields;
  }

  //        Map<string, Field>;

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

  static getPrimaryKeyField() {
    let fields = this.getFields();
    if (!fields)
      return;

    return fields._primaryKeyField;
  }

  getPrimaryKeyField() {
    return this.constructor.getPrimaryKeyField();
  }

  static getPrimaryKeyFieldName() {
    let primaryKeyField = this.getPrimaryKeyField();
    if (!primaryKeyField)
      return;

    return primaryKeyField.fieldName;
  }

  getPrimaryKeyFieldName() {
    return this.constructor.getPrimaryKeyFieldName();
  }

  static primaryKeyHasRemoteValue() {
    let primaryKeyField = this.getPrimaryKeyField();
    if (!primaryKeyField)
      return false;

    return primaryKeyField.type.isRemote();
  }

  primaryKeyHasRemoteValue() {
    return this.constructor.primaryKeyHasRemoteValue();
  }

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

  static hasField(fieldName) {
    return !!this.getField(fieldName);
  }

  hasField(fieldName) {
    return this.constructor.hasField(fieldName);
  }

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

  static defaultOrder(options) {
  }

  static getWhereWithConnection(options) {
    if (options && options.connection)
      return this.where(options.connection);
    else
      return this.where;
  }

  getWhereWithConnection(options) {
    return this.constructor.getWhereWithConnection(options);
  }

  static async create(models, options) {
    let connection = this.getConnection() || (options && options.connection);
    if (!connection)
      throw new Error(`${this.constructor.name}::create: Connection not found. A connection is required to be bound to the model. Is your model not yet initialized through a connection?`);

    let result = await connection.insert(this.getModel(), models, options);
    if (Array.isArray(models))
      return result;

    return (Array.isArray(result)) ? result[ 0 ] : result;
  }

  static count(options) {
    return this.getWhereWithConnection(options).count(null, options);
  }

  static all(options) {
    return this.getWhereWithConnection(options).all(options);
  }

  static first(limit, options) {
    if (limit != null && !Nife.instanceOf(limit, 'number'))
      throw new Error(`${this.getModelName()}::first: "limit" must be null, or a number. If you want to supply a query, use "${this.getModelName()}.where.{query}.first(limit)" instead.`);

    return this.getWhereWithConnection(options).first(limit, options);
  }

  static last(limit, options) {
    if (limit != null && !Nife.instanceOf(limit, 'number'))
      throw new Error(`${this.getModelName()}::last: "limit" must be null, or a number. If you want to supply a query, use "${this.getModelName()}.where.{query}.last(limit)" instead.`);

    return this.getWhereWithConnection(options).last(limit, options);
  }

  static pluck(fields, options) {
    return this.getWhereWithConnection(options).pluck(fields);
  }

  constructor(data, _options) {
    let options = _options || {};

    const whereProp = {
      enumerable: false,
      configurable: true,
      get: () => {
        return this.constructor.where(this._getConnection());
      },
      set: () => { },
    };

    Object.defineProperties(this, {
      '_options': {
        writable: true,
        enumerable: false,
        configurable: true,
        value: options,
      },
      '_mythixModelInstance': {
        writable: false,
        enumerable: false,
        configurable: false,
        value: true,
      },
      '_connection': {
        writable: false,
        enumerable: false,
        configurable: false,
        value: this._getConnection(options.connection),
      },
      '_fieldData': {
        writable: true,
        enumerable: false,
        configurable: true,
        value: {},
      },
      '_dirtyFieldData': {
        writable: true,
        enumerable: false,
        configurable: true,
        value: {},
      },
      '_typeData': {
        writable: true,
        enumerable: false,
        configurable: true,
        value: {},
      },
      'dirtyID': {
        writable: true,
        enumerable: false,
        configurable: true,
        value: new CacheKey(),
      },
      '_persisted': {
        writable: true,
        enumerable: false,
        configurable: true,
        value: false,
      },
      '__order': {
        writable: true,
        enumerable: false,
        configurable: true,
        value: 0,
      },
      '__assignedRelatedModels': {
        writable: true,
        enumerable: false,
        configurable: true,
        value: new Map(),
      },
      'changes': {
        enumerable: false,
        configurable: true,
        get: () => {
          return this._getDirtyFields();
        },
        set: () => { },
      },
      'where': whereProp,
      '$': whereProp,
    });

    this._constructor(data);
  }

  getOptions() {
    return this._options;
  }

  _constructor(data) {
    this._constructFields();
    this._initializeModelData(data);
  }

  _constructFields() {
    this.iterateFields(({ field, fieldName }) => {
      field.type.initialize(this._getConnection(), this);
      if (field.type.exposeToModel())
        this._constructField(fieldName, field);
    });
  }

  _constructField(fieldName, field) {
    Object.defineProperties(this, {
      [ fieldName ]: {
        enumerable: false,
        configurable: true,
        get: () => {
          return this._getFieldValue(fieldName, field);
        },
        set: (value) => {
          this._setFieldValue(fieldName, field, value);
        },
      },
    });
  }

  _initializeModelData(_data) {
    let dirtyFieldData = this._dirtyFieldData;
    let data = _data || {};

    // First initialize field values from data
    this.iterateFields(({ field, fieldName }) => {
      if (!field.type.exposeToModel())
        return;

      let fieldValue = data[ fieldName ];
      if (fieldValue === undefined)
        return;

      dirtyFieldData[ fieldName ] = fieldValue;
    });

    // Next initialize default values
    this.iterateFields(({ field, fieldName }) => {
      field.type.initialize(this._getConnection(), this);
      if (!field.type.exposeToModel())
        return;

      let fieldValue = (data) ? data[ fieldName ] : undefined;
      this._initializeFieldData(fieldName, field, fieldValue, data);
    });
  }

  _castFieldValue(field, value) {
    let type = field.type;
    if (!type)
      return value;

    return type.castToType({
      connection: this.getConnection(),
      Model: this.getModel(),
      self: this,
      field,
      value,
    });
  }

  _initializeFieldData(fieldName, field, fieldValue, data) {
    let dirtyFieldData = this._dirtyFieldData;
    let fieldData = this._fieldData;
    let defaultValue = fieldValue;

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
          model: this,
          self: this,
          connection: this.getConnection(),
          _initial: true,
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

      fieldData[ fieldName ] = initialValue;
    } else {
      initialValue = this._castFieldValue(field, defaultValue);
      dirtyFieldData[ fieldName ] = initialValue;
    }

    field.type.onSetFieldValue({
      self: this,
      value: initialValue,
      field,
      fieldName,
    });
  }

  _getDirtyFields(_options) {
    let options = _options || {};
    let fieldData = this._fieldData;
    let dirtyFieldData = this._dirtyFieldData;
    let dirtyFields = {};
    let connection = this.getConnection();
    let queryGenerator = (connection) ? connection.getQueryGenerator() : null;

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      if (Object.prototype.hasOwnProperty.call(dirtyFieldData, fieldName)) {
        dirtyFields[ fieldName ] = { previous: fieldData[ fieldName ], current: dirtyFieldData[ fieldName ] };
        return;
      }

      // Does the type itself report that we are dirty?
      let value = field.type.isDirty({
        self: this,
        value: this.getDataValue(fieldName),
        field,
        fieldName,
        connection,
      });

      if (value !== undefined) {
        // Cache this result so subsequent calls
        // to dirty fields doesn't get a new value
        dirtyFieldData[ fieldName ] = value;

        dirtyFields[ fieldName ] = {
          previous: fieldData[ fieldName ],
          current: value,
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
          dirtyFieldData[ fieldName ] = value;

          dirtyFields[ fieldName ] = {
            previous: fieldData[ fieldName ],
            current: value,
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
        dirtyFieldData[ fieldName ] = value;

        dirtyFields[ fieldName ] = {
          previous: fieldData[ fieldName ],
          current: value,
        };
      } else if (options.insert && DefaultHelpers.checkDefaultValueFlags(field.defaultValue, [ 'onInsert' ])) {
        let value = queryGenerator.getFieldDefaultValue(field, fieldName, Object.assign({ isInsertOperation: true, rawLiterals: true, escape: false }));

        // Cache this result so subsequent calls
        // to dirty fields doesn't get a new value
        dirtyFieldData[ fieldName ] = value;

        dirtyFields[ fieldName ] = {
          previous: fieldData[ fieldName ],
          current: value,
        };
      }
    });

    return dirtyFields;
  }

  _getFieldValue(fieldName, field) {
    let value = this.getDataValue(fieldName);

    if (typeof field.get === 'function') {
      return field.get.call(this, {
        model: this,
        self: this,
        set: this.setDataValue.bind(this, fieldName),
        get: this.getDataValue.bind(this, fieldName),
        value,
        field,
        fieldName,
      });
    }

    return value;
  }

  _setFieldValue(fieldName, field, value) {
    if (typeof field.set === 'function') {
      field.set.call(this, {
        model: this,
        self: this,
        set: this.setDataValue.bind(this, fieldName),
        get: this.getDataValue.bind(this, fieldName),
        value,
        field,
        fieldName,
      });

      return;
    }

    this.setDataValue(fieldName, value);
  }

  isPersisted() {
    return this._persisted;
  }

  updateDirtyID() {
    this.dirtyID = new CacheKey(this.dirtyID);
  }

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

  clearDirty(fieldName) {
    if (fieldName && Object.prototype.hasOwnProperty.call(this._dirtyFieldData, fieldName)) {
      this._fieldData[ fieldName ] = this._dirtyFieldData[ fieldName ];
      delete this._dirtyFieldData[ fieldName ];
    } else {
      Object.assign(this._fieldData, this._dirtyFieldData);
      this._dirtyFieldData = {};
    }

    this.updateDirtyID();
  }

  getDirtyFields(options) {
    let modelChanges = this._getDirtyFields(options);
    let dirtyFieldNames = Object.keys(modelChanges);

    return this.getFields(dirtyFieldNames);
  }

  getDataValue(fieldName) {
    let fieldData = this._fieldData;
    let dirtyFieldData = this._dirtyFieldData;
    let value;

    if (Object.prototype.hasOwnProperty.call(dirtyFieldData, fieldName))
      value = dirtyFieldData[ fieldName ];
    else
      value = fieldData[ fieldName ];

    return value;
  }

  setDataValue(fieldName, value) {
    let fieldData = this._fieldData;
    let dirtyFieldData = this._dirtyFieldData;
    let field = this.getField(fieldName);

    if (!field)
      throw new Error(`${this.getModelName()}::setDataValue: Unable to find field named "${fieldName}".`);

    let newValue = this._castFieldValue(field, value);

    field.type.onSetFieldValue({
      self: this,
      value: newValue,
      field,
      fieldName,
    });

    // If the values are exactly the same,
    // then we are no longer dirty, and
    // can just return
    if (fieldData[ fieldName ] === newValue) {
      delete dirtyFieldData[ fieldName ];

      this.updateDirtyID();

      return;
    }

    dirtyFieldData[ fieldName ] = newValue;

    this.updateDirtyID();
  }

  getAttributes() {
    let result = {};

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      let fieldValue = this[ fieldName ];
      if (fieldValue === undefined)
        return;

      result[ fieldName ] = fieldValue;
    });

    return result;
  }

  setAttributes(attributes, noPrimaryKey) {
    let isObject = Nife.instanceOf(attributes, 'object');

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      if (field.primaryKey === true && noPrimaryKey === true)
        return;

      if (isObject && !Object.prototype.hasOwnProperty.call(attributes, fieldName))
        return;

      let fieldValue = attributes[ fieldName ];
      if (fieldValue === undefined)
        return;

      this[ fieldName ] = fieldValue;
    });
  }

  hasValidPrimaryKey() {
    let pkField = this.getPrimaryKeyField();
    if (!pkField)
      return false;

    let pkFieldName = pkField.fieldName;
    let pkValue = this[ pkFieldName ];
    if (pkValue == null)
      return false;

    return pkField.type.isValidValue(pkValue, {
      connection: this.getConnection(),
      Model: this.getModel(),
      self: this,
    });
  }

  async onValidate(context) {
    let promises = [];

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      if (typeof field.validate !== 'function')
        return;

      try {
        let fieldValue = this[ fieldName ];
        let promise = field.validate.call(this, fieldValue, context);
        if (!Nife.instanceOf(promise, 'promise'))
          promise = Promise.resolve(promise);

        promises.push(promise);
      } catch (error) {
        promises.push(Promise.reject(error));
      }
    });

    await Promise.all(promises);
  }

  // eslint-disable-next-line no-unused-vars
  async onBeforeCreate(context) {
  }

  // eslint-disable-next-line no-unused-vars
  async onBeforeUpdate(context) {
  }

  // eslint-disable-next-line no-unused-vars
  async onBeforeSave(context) {
    await this.onValidate(context);
  }

  // eslint-disable-next-line no-unused-vars
  async onAfterCreate(context) {
  }

  // eslint-disable-next-line no-unused-vars
  async onAfterUpdate(context) {
  }

  // eslint-disable-next-line no-unused-vars
  async onAfterSave(context) {
  }

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

        dirtyFieldData[ fieldName ] = this[ fieldName ];
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

  async reload(options) {
    let pkFieldName = this.getPrimaryKeyFieldName();
    if (!pkFieldName)
      throw new Error(`${this.getModelName()}::reload: Unable to reload models that have no primary key defined.`);

    let id = this[ pkFieldName ];
    if (!id)
      return;

    let storedModel = await this.getWhereWithConnection(options)[ pkFieldName ].EQ(id).first();
    if (!storedModel)
      return;

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      let fieldValue = storedModel[ fieldName ];
      this[ fieldName ] = fieldValue;
    });

    this._persisted = true;
    this.clearDirty();
  }

  async destroy(options) {
    if (!this.isPersisted())
      return 0;

    let primaryKeyFieldName = this.getPrimaryKeyFieldName();
    if (!primaryKeyFieldName || !this[ primaryKeyFieldName ])
      throw new Error(`${this.getModelName()}::destroy: Model has no primary key field, or value. Refusing to destroy this model to prevent possible data loss. Please call "connection.destroy" yourself, manually providing your own query.`);

    return await this.getModel().getWhereWithConnection(options)[ primaryKeyFieldName ].EQ(this.id).destroy(options);
  }

  toString() {
    return `${this.getModelName()} ${JSON.stringify(this.toJSON(), undefined, 2)}`;
  }

  toJSON() {
    let result = {};
    let pkField = this.getPrimaryKeyField();

    // PK always comes first
    if (pkField) {
      let pkFieldName = pkField.fieldName;
      let fieldValue = this[ pkFieldName ];

      if (fieldValue !== undefined)
        result[ pkFieldName ] = pkField.type.serialize(fieldValue);
    }

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      if (field.primaryKey)
        return;

      let fieldValue = this[ fieldName ];
      if (fieldValue === undefined)
        return;

      result[ fieldName ] = field.type.serialize(fieldValue);
    });

    return result;
  }

  // eslint-disable-next-line no-unused-vars
  [ Symbol.for('nodejs.util.inspect.custom') ]() {
    return this.toJSON();
  }
}