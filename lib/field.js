'use strict';

class Field {
  static _isMythixField = true;

  static isFieldClass(value) {
    if (!value)
      return false;

    if (value.prototype instanceof Field)
      return true;

    if (value._isMythixField)
      return true;

    return false;
  }

  static isField(value) {
    if (!value)
      return false;

    if (value instanceof Field)
      return true;

    if (value.constructor && value.constructor._isMythixField)
      return true;

    return false;
  }

  constructor(fieldDefinition) {
    Object.assign(this, fieldDefinition || {});
  }

  clone() {
    const FieldClass = this.constructor;
    return new FieldClass(this);
  }

  setModel(Model) {
    this.Model = Model;
  }
}

module.exports = Field;
