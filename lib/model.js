'use strict';

const Nife                    = require('nife');
const Inflection              = require('inflection');
const { iterateStaticProps }  = require('./utils/misc-utils');
const Type                    = require('./types/type');
const DefaultHelpers          = require('./types/helpers/default-helpers');

class Model {
  static _isModelClass = true;

  static isModelClass(value) {
    if (!value)
      return false;

    if (value.prototype instanceof Model)
      return true;

    if (value._isModelClass)
      return true;

    return false;
  }

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

  static cloneFields(mergeFields) {
    let isArray       = Array.isArray(this.getFields());
    let clonedFields  = (isArray) ? [] : {};

    this.iterateFields(({ field, fieldName }) => {
      let fieldCopy = Object.assign({}, field, { type: field.type.clone() });

      if (isArray)
        clonedFields.push(fieldCopy);
      else
        clonedFields[fieldName] = fieldCopy;
    });

    if (mergeFields) {
      this.iterateFields(({ field, fieldName }) => {
        let fieldCopy = Object.assign({}, field, { type: field.type.clone() });

        if (isArray) {
          let currentFieldIndex = clonedFields.findIndex((thisField) => (thisField.fieldName === fieldName));
          if (currentFieldIndex >= 0)
            clonedFields[currentFieldIndex] = fieldCopy;
          else
            clonedFields.push(fieldCopy);
        } else {
          clonedFields[fieldName] = fieldCopy;
        }
      }, mergeFields);
    }

    return clonedFields;
  }

  static getConnection() {
    if (typeof this._getConnection === 'function')
      return this._getConnection();

    return null;
  }

  static _getQueryEngineClass = function() {
    return this.getConnection().getQueryEngineClass();
  };

  static getQueryEngineClass() {
    return this._getQueryEngineClass();
  }

  static getUnscopedQueryEngine() {
    let QueryEngineClass  = this.getQueryEngineClass();
    let queryEngine       = new QueryEngineClass({
      connection: this.getConnection(),
    });

    return queryEngine[this.getModelName()];
  }

  static defaultScope(queryEngine) {
    return queryEngine;
  }

  static getQueryEngine() {
    let queryEngine = this.getUnscopedQueryEngine();
    return this.defaultScope(queryEngine);
  }

  static initializeConnection(_Model, connection) {
    let ConnectionModel = _Model;
    if (Object.prototype.hasOwnProperty.call(ConnectionModel, 'mythixConnectionInitialized') && ConnectionModel.mythixConnectionInitialized)
      return ConnectionModel;

    let getModelName = ConnectionModel.getModelName;
    if (getModelName === Model.getModelName) {
      let thisModelName = ConnectionModel.getModelName();
      getModelName = () => thisModelName;
    }

    return class ModelWithBoundConnection extends ConnectionModel {
      static fields = Model.cloneFields(ConnectionModel.fields);

      constructor(...args) {
        super(...args);
      }

      static mythixConnectionInitialized = true;

      static _getConnection() {
        return connection;
      }

      static getModelName = getModelName;

      getModelName() {
        return getModelName();
      }
    };
  }

  static initializeModel(Model, connection) {
    if (Object.prototype.hasOwnProperty.call(Model, 'mythixModelInitialized') && Model.mythixModelInitialized)
      return Model;

    let foreignFields = new Map();

    // Initialize model fields
    Model.iterateFields(({ field, fieldName }) => {
      field.fieldName = fieldName;
      field.Model = Model;

      if (typeof field.type.onModelInitialize === 'function')
        field.type.onModelInitialize.call(field.type, Model, field, field.type, connection);

      if (field.type.isForeignKey()) {
        let targetModelName = field.type.getTargetModelName();
        let targetFieldName = field.type.getTargetFieldName();
        let relationSet     = foreignFields.get(targetModelName);

        if (!relationSet) {
          relationSet = [];
          foreignFields.set(targetModelName, relationSet);
        }

        relationSet.push({ targetFieldName: targetFieldName, sourceFieldName: fieldName });
      }
    });

    Object.defineProperties(Model, {
      'mythixModelInitialized': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        true,
      },
      'where': {
        enumberable:  false,
        configurable: true,
        get:          () => {
          return Model.getQueryEngine();
        },
        set:          () => {},
      },
      '_foreignFields': {
        writable:     false,
        enumberable:  false,
        configurable: false,
        value:        foreignFields,
      },
    });

    return Model;
  }

  static getForeignKeysTargetModelNames() {
    if (!this._foreignFields)
      return [];

    return Array.from(this._foreignFields.keys());
  }

  static getForeignKeysTargetFieldNames(modelName) {
    if (!this._foreignFields)
      return [];

    let fieldNames = this._foreignFields.get(modelName);
    return (fieldNames || []);
  }

  static getForeignKeysTargetField(modelName, fieldName) {
    if (!this._foreignFields)
      return false;

    let fields = this._foreignFields.get(modelName);
    if (!fields)
      return;

    return fields.find((field) => (field.targetFieldName === fieldName));
  }

  static isForeignKeyTargetModel(modelName) {
    if (!this._foreignFields)
      return false;

    return this._foreignFields.has(modelName);
  }

  static getTablePrefix() {
    return '';
  }

  static getTableName() {
    let tableName = Nife.camelCaseToSnakeCase(this.getPluralModelName());
    return `${this.getTablePrefix() || ''}${tableName}`;
  }

  static getModel() {
    return this;
  }

  static getModelName() {
    return this.name;
  }

  static getSingularName() {
    return this.getModelName();
  }

  static getPluralModelName() {
    return Inflection.pluralize(this.getSingularName());
  }

  static getFields(fieldNames) {
    if (fieldNames) {
      let filteredFields = [];

      this.iterateFields(({ field, fieldName }) => {
        if (fieldNames.indexOf(fieldName) < 0)
          return;

        filteredFields.push(field);
      }, this.fields);

      return filteredFields;
    } else {
      return this.fields;
    }
  }

  static iterateFields(callback, _fields) {
    const sortFieldNames = (fieldNames, fields) => {
      return fieldNames.sort((a, b) => {
        let x = fields[a];
        let y = fields[b];

        if (x.primaryKey)
          return -1;
        else if (y.primaryKey)
          return 1;

        if (a === b)
          return 0;

        return (a < b) ? -1 : 1;
      });
    };

    let fields = (_fields) ? _fields : this.getFields();
    if (!fields || typeof callback !== 'function')
      return [];

    const stop = () => {
      _stop = true;
    };

    let fieldNames = Object.keys(fields);
    let isArray = Array.isArray(fields);
    if (!isArray)
      fieldNames = sortFieldNames(fieldNames, fields);

    let results = [];
    let _stop = false;

    for (let i = 0, il = fieldNames.length; i < il; i++) {
      let fieldName = fieldNames[i];
      let field = fields[fieldName];

      if (field.fieldName) {
        fieldName = field.fieldName;
      } else {
        if (isArray)
          throw new Error(`${this.name}::iterateFields: "fieldName" is missing on field index ${i}.`);

        field.fieldName = fieldName;
      }

      if (!field.columnName)
        field.columnName = fieldName;

      if (!field.type)
        throw new Error(`${this.name}::iterateFields: "type" not found on "${this.name}.${fieldName}". "type" is required for all fields.`);

      if (field.type.uninitializedType || field.type.mythixType || field.type._initialized !== true)
        field.type = Type.instantiateType(this.getModel(), this, field, field.type);

      let result = callback({ field, fieldName, fields, stop, index: i });

      if (_stop)
        break;

      results.push(result);
    }

    return results;
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

  static getPrimaryKeyField() {
    if (this._primaryKeyField)
      return this._primaryKeyField;

    let primaryKeyField;
    this.iterateFields(({ field, stop }) => {
      if (field.type.isVirtual())
        return;

      if (field.primaryKey) {
        primaryKeyField = field;
        return stop();
      }
    });

    Object.defineProperties(this, {
      '_primaryKeyField': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        primaryKeyField,
      },
    });

    return primaryKeyField;
  }

  static getPrimaryKeyFieldName() {
    let primaryKeyField = this.getPrimaryKeyField();
    return primaryKeyField.fieldName;
  }

  static primaryKeyHasRemoteValue() {
    let pkField = this.getPrimaryKeyField();
    if (!pkField)
      return false;

    return pkField.type.isRemote();
  }

  static getField(findFieldName) {
    let fields = this.getFields();
    if (!fields)
      return;

    if (Array.isArray(fields)) {
      let foundField;

      this.iterateFields(({ field, fieldName, stop }) => {
        if (fieldName === findFieldName) {
          foundField = field;
          stop();
        }
      });

      return foundField;
    } else {
      return fields[findFieldName];
    }
  }

  static hasField(fieldName) {
    return !!this.getField(fieldName);
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

  static getDefaultOrder() {
  }

  static async create(models, options) {
    let connection = this.getConnection();
    if (!connection)
      throw new Error(`${this.constructor.name}::create: Connection not found. A connection is required to be bound to the model. Is your model not yet initialized through a connection?`);

    let result = await connection.insert(this.getModel(), models, options);
    if (Array.isArray(models))
      return result;

    return (Array.isArray(result)) ? result[0] : result;
  }

  static count(options) {
    return this.where.count(options);
  }

  static all(options) {
    return this.where.all(options);
  }

  static first(limit, options) {
    if (limit != null && !Nife.instanceOf(limit, 'number'))
      throw new Error(`${this.getModelName()}::first: "limit" must be null, or a number. If you want to supply a query, use "${this.getModelName()}.where.{query}.first(limit)" instead.`);

    return this.where.first(limit, options);
  }

  static last(limit, options) {
    if (limit != null && !Nife.instanceOf(limit, 'number'))
      throw new Error(`${this.getModelName()}::last: "limit" must be null, or a number. If you want to supply a query, use "${this.getModelName()}.where.{query}.last(limit)" instead.`);

    return this.where.last(limit, options);
  }

  static pluck(...fields) {
    return this.where.pluck(...fields);
  }

  constructor(data) {
    Object.defineProperties(this, {
      '_mythixModelInstance': {
        writable:     false,
        enumberable:  false,
        configurable: false,
        value:        true,
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
      'changes': {
        enumberable:  false,
        configurable: true,
        get:          () => {
          return this._getDirtyFields();
        },
        set:          () => {},
      },
      // loaded relations
      '_': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        {},
      },
      'where': {
        enumberable:  false,
        configurable: true,
        get:          () => {
          return this.constructor.where;
        },
        set:          () => {},
      },
    });

    this._constructor(data);
  }

  _constructor(data) {
    this._constructFields();
    this._initializeModelData(data);
  }

  _constructFields() {
    this.iterateFields(({ field, fieldName }) => {
      if (field.type.exposeToModel())
        this._constructField(fieldName, field);

      field.type.onModelInstantiated(this, field, field.type);
    });
  }

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
      if (!field.type.exposeToModel())
        return;

      let fieldValue = (data) ? data[fieldName] : undefined;
      this._initializeFieldData(fieldName, field, fieldValue, data);
    });
  }

  _castFieldValue(field, value) {
    let type = field.type;
    if (!type)
      return value;

    return type._castToType({ value });
  }

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

    if (defaultValue === undefined || !data)
      fieldData[fieldName] = (defaultValue != null) ? this._castFieldValue(field, defaultValue) : defaultValue;
    else
      dirtyFieldData[fieldName] = this._castFieldValue(field, defaultValue);
  }

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

      if (!queryGenerator)
        return;

      if (options.update && DefaultHelpers.checkDefaultValueFlags(field.defaultValue, [ 'onUpdate' ])) {
        dirtyFields[fieldName] = {
          previous: fieldData[fieldName],
          current:  queryGenerator.getFieldDefaultValue(field, fieldName, Object.assign({ isUpdateOperation: true, rawLiterals: true, escape: false })),
        };
      } else if (options.insert && DefaultHelpers.checkDefaultValueFlags(field.defaultValue, [ 'onInsert' ])) {
        dirtyFields[fieldName] = {
          previous: fieldData[fieldName],
          current:  queryGenerator.getFieldDefaultValue(field, fieldName, Object.assign({ isInsertOperation: true, rawLiterals: true, escape: false })),
        };
      }
    });

    return dirtyFields;
  }

  _getFieldValue(fieldName, field) {
    let value = this.getDataValue(fieldName);

    if (typeof field.get === 'function')
      return field.get.call(this, { value, field, fieldName });

    return value;
  }

  _setFieldValue(fieldName, field, value) {
    if (typeof field.set === 'function') {
      field.set.call(this, { value, field, fieldName });
      return;
    }

    this.setDataValue(fieldName, value);
  }

  isPersisted() {
    return this._persisted;
  }

  isDirty(fieldName) {
    if (!fieldName)
      return (Object.keys(this._dirtyFieldData).length > 0);
    else
      return Object.prototype.hasOwnProperty.call(this._dirtyFieldData, fieldName);
  }

  clearDirty(fieldName) {
    if (fieldName && Object.prototype.hasOwnProperty.call(this._dirtyFieldData, fieldName)) {
      this._fieldData[fieldName] = this._dirtyFieldData[fieldName];
      delete this._dirtyFieldData[fieldName];
    } else {
      Object.assign(this._fieldData, this._dirtyFieldData);
      this._dirtyFieldData = {};
    }
  }

  getDirtyFields(options) {
    let modelChanges    = this._getDirtyFields(options);
    let dirtyFieldNames = Object.keys(modelChanges);

    return this.getFields(dirtyFieldNames);
  }

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

  setDataValue(fieldName, value) {
    let fieldData       = this._fieldData;
    let dirtyFieldData  = this._dirtyFieldData;
    let field           = this.getField(fieldName);

    if (!field)
      throw new Error(`${this.getModelName()}::setDataValue: Unable to find field named "${fieldName}".`);

    let newValue = this._castFieldValue(field, value);

    // If the values are exactly the same,
    // then we are no longer dirty, and
    // can just return
    if (fieldData[fieldName] === newValue) {
      delete dirtyFieldData[fieldName];
      return;
    }

    dirtyFieldData[fieldName] = newValue;
  }

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

  hasValidPrimaryKey() {
    let pkField = this.getPrimaryKeyField();
    if (!pkField)
      return false;

    let pkFieldName = pkField.fieldName;
    let pkValue     = this[pkFieldName];

    return pkField.type.isValidValue(pkValue);
  }

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
        if (!(promise instanceof Promise))
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

    if (options.force !== true && Nife.isEmpty(this.changes)) {
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

    let connection = this.getConnection();

    if (this.isPersisted())
      await connection.update(this.getModel(), [ this ], options);
    else
      await connection.insert(this.getModel(), [ this ], options);

    return this;
  }

  async reload() {
    let pkFieldName = this.getPrimaryKeyFieldName();
    if (!pkFieldName)
      throw new Error(`${this.getModelName()}::reload: Unable to reload models that have no primary key defined.`);

    let id = this[pkFieldName];
    if (!id)
      return;

    let storedModel = await this.where[pkFieldName].EQ(id).first();
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

  toString() {
    return `${this.getModelName()} ${JSON.stringify(this.toJSON(), undefined, 2)}`;
  }

  toJSON() {
    let result = {};

    this.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      let fieldValue = this[fieldName];
      if (fieldValue === undefined)
        return;

      result[fieldName] = field.type.serialize(fieldValue);
    });

    return result;
  }

  // eslint-disable-next-line no-unused-vars
  [Symbol.for('nodejs.util.inspect.custom')](depth, inspectOptions, inspect) {
    return inspect(this.toJSON(), inspectOptions);
  }
}

const staticMethodToSkip = [
  'initializeModel',
  'cloneFields',
  'mythixConnectionInitialized',
  'mythixModelInitialized',
  'where',
];

// Make static methods callable from an instance
// by extending the prototype
iterateStaticProps(Model, ({ value, key, prototype }) => {
  if (typeof value !== 'function')
    return;

  if (staticMethodToSkip.indexOf(key) >= 0)
    return;

  if (!(key in prototype)) {
    prototype[key] = function(...args) {
      return value.apply(this.constructor, args);
    };
  }
});

Object.defineProperties(Model, {
  'where': {
    enumberable:  false,
    configurable: true,
    get:          () => {
      throw new Error('Model::where: This is a raw uninitialized model. You need to fetch an initialized model from a connection in order to use "where".');
    },
  },
});

module.exports = Model;
