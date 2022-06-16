'use strict';

const Nife                    = require('nife');
const { iterateStaticProps }  = require('./utils/misc-utils');
const Type                    = require('./types/type');
const Inflection              = require('inflection');
const { FLAG_ON_INITIALIZE }  = require('./helpers/default-helpers');

class Model {
  static isModelClass() {
    return true;
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

  static initializeModel(_Model, connection) {
    let ConnectionModel = _Model;

    if (Object.prototype.hasOwnProperty.call(ConnectionModel, 'mythixInitialized') && ConnectionModel.mythixInitialized)
      return ConnectionModel;

    let modelName = ConnectionModel.getModelName();

    // Unfortunately we have no choice but to use
    // eval to get the correct class name

    // eslint-disable-next-line no-eval
    let classBuilderFunc = eval(`(function(ConnectionModel, connection) {
      return class ${modelName} extends ConnectionModel {
        static fields = Model.cloneFields(ConnectionModel.fields);

        constructor(...args) {
          super(...args);
        }

        static _getConnection = function() {
          return connection;
        };
      };
    })`);

    ConnectionModel = classBuilderFunc(ConnectionModel, connection);

    // Initialize model fields
    ConnectionModel.iterateFields(({ field, fieldName }) => {
      field.fieldName = fieldName;
      field.Model = ConnectionModel;

      if (typeof field.type.onModelInitialize === 'function')
        field.type.onModelInitialize.call(field.type, ConnectionModel, field, field.type, connection);
    });

    Object.defineProperties(ConnectionModel, {
      'mythixInitialized': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        true,
      },
      'where': {
        enumberable:  false,
        configurable: true,
        get:          () => {
          return ConnectionModel.getQueryEngine();
        },
        set:          () => {},
      },
    });

    return ConnectionModel;
  }

  static getTablePrefix() {
    return '';
  }

  static getTableName() {
    let tableName = Nife.camelCaseToSnakeCase(this.getPluralName());
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

  static getPluralName() {
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

  static getPrimaryKeyField() {
    if (this._primaryKeyField)
      return this._primaryKeyField;

    let primaryKeyField;
    this.iterateFields(({ field, stop }) => {
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

  constructor(data) {
    Object.defineProperties(this, {
      '_isModelInstance': {
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
    });

    this._constructor(data);
  }

  _constructor(data) {
    this._constructFields();
    this._initializeModelData(data);
  }

  _constructFields() {
    this.iterateFields(({ field, fieldName }) => {
      if (!field.type.isVirtual())
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

  _initializeModelData(data) {
    let dirtyFieldData = this._dirtyFieldData;

    // First initialize field values from data
    if (data) {
      this.iterateFields(({ fieldName }) => {
        let fieldValue  = (data) ? data[fieldName] : undefined;
        if (fieldValue === undefined)
          return;

        dirtyFieldData[fieldName] = fieldValue;
      });
    }

    // Next initialize default values
    this.iterateFields(({ field, fieldName }) => {
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
      defaultValue = defaultValue.call(this, { field, fieldName, fieldValue, data });

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
        if (defaultValue.mythixFlags & FLAG_ON_INITIALIZE)
          return true;

        return false;
      };

      if (shouldRunDefaultValueOnInitialize())
        defaultValue = defaultValue({ field, fieldName, fieldValue, data, modelInstance: this });
      else
        defaultValue = undefined;
    }

    if (defaultValue === undefined || !data)
      fieldData[fieldName] = (defaultValue != null) ? this._castFieldValue(field, defaultValue) : defaultValue;
    else
      dirtyFieldData[fieldName] = this._castFieldValue(field, defaultValue);
  }

  _getDirtyFields() {
    let fieldData       = this._fieldData;
    let dirtyFieldData  = this._dirtyFieldData;
    let keys            = Object.keys(dirtyFieldData);
    let dirtyFields     = {};

    for (let i = 0, il = keys.length; i < il; i++) {
      let key = keys[i];
      dirtyFields[key] = { previous: fieldData[key], current: dirtyFieldData[key] };
    }

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

  async save(_options) {
    let options = _options || {};

    if (options.force !== true && Nife.isEmpty(this.changes))
      return false;

    let connection = this.getConnection();
    await connection.update(this.getModel(), [ this ], options);

    return true;
  }

  toJSON() {
    return this.getAttributes();
  }
}

const staticMethodToSkip = [
  'initializeModel',
  'cloneFields',
  'mythixInitialized',
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
