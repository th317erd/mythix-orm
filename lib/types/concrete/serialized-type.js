'use strict';

const Type = require('../type');

class SerializedType extends Type {
  static getDisplayName() {
    return 'SERIALIZED';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  constructor(_options) {
    let options = _options;
    if (Type.isTypeClass(options) || Type.isType(options))
      options = { type: options };

    options = {
      serialize:    ({ value }) => {
        if (value == null)
          return value;

        return JSON.stringify(value);
      },
      deserialize:  ({ value }) => {
        if (value == null)
          return value;

        return JSON.parse(value);
      },
      ...(options || {}),
    };

    if (!options.type)
      throw new TypeError('SerializedType::constructor: "type" must be specified on the "options" for this type. "type" defines the storage type for this field. i.e. Types.SERIALIZED({ type: Types.STRING(1024) })');

    super(options);

    options.type = Type.instantiateType(options.type);

    Object.defineProperties(this, {
      'options': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        options,
      },
    });
  }

  getOptions() {
    return this.options;
  }

  initialize(connection, self) {
    let options = this.getOptions();
    options.type.initialize(connection, self);
  }

  castToType({ value, connection }) {
    return this.deserialize(value, connection);
  }

  onSetFieldValue({ value, fieldName, self }) {
    // we make a copy here
    // so that when the value is modified
    // we can detect the changes later
    let clonedValue = this.deserialize(this.serialize(value, self.getConnection() || {}));
    self._typeData[fieldName] = clonedValue;
  }

  isDirty(context) {
    let options = this.getOptions();
    if (typeof options.isDirty === 'function')
      return options.isDirty.call(this, context);

    let { value, fieldName, self, connection } = context;
    let currentValue = self._typeData[fieldName];
    if (value == null && currentValue == null)
      return;

    if (this.serialize(value, connection) !== this.serialize(currentValue, connection))
      return value;
  }

  isValidValue() {
    return true;
  }

  serialize(value, connection) {
    if (!connection)
      return value;

    let options   = this.getOptions();
    let innerType = options.type;

    if (value != null && innerType.isValidValue(value))
      return value;

    return options.serialize({ value, connection });
  }

  deserialize(value, connection) {
    let options   = this.getOptions();
    let innerType = options.type;

    if (value != null && !innerType.isValidValue(value))
      return value;

    return options.deserialize({ value, connection });
  }

  toString(connection) {
    let options   = this.getOptions();
    let innerType = options.type;
    return innerType.toString(connection);
  }

  toConnectionType(connection, options) {
    let typeOptions = this.getOptions();
    let innerType   = typeOptions.type;

    if (!connection)
      return innerType.toString();

    return connection.typeToString(innerType, options);
  }
}

module.exports = {
  SERIALIZED: Type.wrapConstructor(SerializedType),
  SerializedType,
};
