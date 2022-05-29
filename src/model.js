'use strict';

const { iterateStaticProps }  = require('./utils/misc-utils');
const Type                    = require('./types/type');
const Inflection              = require('inflection');

class Model {
  static cloneFields() {
    let clonedFields = {};

    this.iterateFields(({ field, fieldName }) => {
      clonedFields[fieldName] = Object.assign({}, field, { type: field.type.clone() });
    });

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

  static initializeModel(Model, connection) {
    if (Object.prototype.hasOwnProperty.call(Model, 'mythixInitialized') && Model.mythixInitialized)
      return Model;

    Model._getConnection = function() {
      return connection;
    };

    // Initialize model fields
    Model.iterateFields(({ field, fieldName }) => {
      field.fieldName = fieldName;
      field.Model = Model;

      if (typeof field.type.onModelInitialize === 'function')
        field.type.onModelInitialize(Model, field, field.type);
    });

    Object.defineProperties(Model, {
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
          return Model.getQueryEngine();
        },
        set:          () => {},
      },
    });

    return Model;
  }

  static getTablePrefix() {
    return '';
  }

  static getTableName() {
    let tableName = this.getPluralName().toLowerCase();
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

  static getFields() {
    return this.fields;
  }

  static iterateFields(callback) {
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

    let fields = this.getFields();
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

  constructor(data) {
    Object.defineProperties(this, {
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
    let fieldData = this._fieldData;

    // First initialize field values from data
    if (data) {
      this.iterateFields(({ fieldName }) => {
        let fieldValue  = (data) ? data[fieldName] : undefined;
        fieldData[fieldName] = fieldValue;
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

    return type.castToType({ value });
  }

  _initializeFieldData(fieldName, field, fieldValue, data) {
    let fieldData     = this._fieldData;
    let defaultValue  = fieldValue;

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
        if (defaultValue.mythixFlags == null)
          return true;

        // Zero, or one means we want to run on initialize
        if (defaultValue.mythixFlags < 2)
          return true;

        return false;
      };

      if (shouldRunDefaultValueOnInitialize())
        defaultValue = defaultValue({ field, fieldName, fieldValue, data, modelInstance: this });
      else
        defaultValue = undefined;
    }

    fieldData[fieldName] = this._castFieldValue(field, defaultValue);
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

  isDirty() {
    return (Object.keys(this._dirtyFieldData).length > 0);
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
      throw new Error(`${this.getModelName()}::setDataValue: Unable to find field named ${fieldName}.`);

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

module.exports = Model;
