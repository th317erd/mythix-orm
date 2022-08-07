'use strict';

class Field {
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
