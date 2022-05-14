'use strict';

class Model {
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

    this._constructor(this.constructor.fields, data);
  }

  _constructor(fields, data) {
    let fieldNames = Object.keys(fields || {});

    for (let i = 0, il = fieldNames.length; i < il; i++) {
      let fieldName   = fieldNames[i];
      let field       = fields[fieldName];
      let fieldValue  = (data) ? data[fieldName] : undefined;

      this._constructField(fieldName, field, fieldValue, data);
    }
  }

  _constructField(fieldName, field, fieldValue, data) {
    let fieldData     = this._fieldData;
    let defaultValue  = fieldValue;

    if (defaultValue === 'function')
      defaultValue = defaultValue.call(this, field, fieldName, fieldValue, data);

    if (defaultValue === undefined)
      defaultValue = field.defaultValue;

    if (defaultValue === 'function')
      defaultValue = defaultValue.call(this, field, fieldName, fieldValue, data);

    fieldData[fieldName] = defaultValue;

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
      return field.get.call(this, value, field, fieldName);

    return value;
  }

  _setFieldValue(fieldName, field, newValue) {
    if (typeof field.set === 'function') {
      field.set.call(this, newValue, field, fieldName);
      return;
    }

    this.setDataValue(fieldName, newValue);
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

  setDataValue(fieldName, newValue) {
    let dirtyFieldData = this._dirtyFieldData;
    dirtyFieldData[fieldName] = newValue;
  }
}

module.exports = Model;
