'use strict';

class Model {
  static getFields() {
    return this.fields;
  }

  static getField(fieldName) {
    let fields = this.getFields();
    return fields[fieldName];
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

    this._constructor(this.getFields(), data);
  }

  _constructor(fields, data) {
    this._constructFields(fields);
    this._initializeModelData(fields, data);
  }

  _constructFields(fields) {
    let fieldNames = Object.keys(fields || {});

    for (let i = 0, il = fieldNames.length; i < il; i++) {
      let fieldName   = fieldNames[i];
      let field       = fields[fieldName];

      this._constructField(fieldName, field);
    }
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

  _initializeModelData(fields, data) {
    let fieldNames  = Object.keys(fields || {});
    let fieldData   = this._fieldData;

    // First initialize field values from data
    if (data) {
      for (let i = 0, il = fieldNames.length; i < il; i++) {
        let fieldName   = fieldNames[i];
        let fieldValue  = (data) ? data[fieldName] : undefined;

        fieldData[fieldName] = fieldValue;
      }
    }

    // Next initialize default values
    for (let i = 0, il = fieldNames.length; i < il; i++) {
      let fieldName   = fieldNames[i];
      let field       = fields[fieldName];
      let fieldValue  = (data) ? data[fieldName] : undefined;

      this._initializeFieldData(fieldName, field, fieldValue, data);
    }
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
        defaultValue = defaultValue.call(this, { field, fieldName, fieldValue, data });
      else
        defaultValue = undefined;
    }

    fieldData[fieldName] = defaultValue;
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

  getFields() {
    return this.constructor.getFields();
  }

  getField(fieldName) {
    return this.constructor.getField(fieldName);
  }

  hasField(fieldName) {
    let fields = this.getFields();
    return (Object.prototype.hasOwnProperty.call(fields, fieldName));
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
    let dirtyFieldData = this._dirtyFieldData;
    dirtyFieldData[fieldName] = value;
  }
}

module.exports = Model;
