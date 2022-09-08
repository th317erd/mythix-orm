'use strict';

const Nife  = require('nife');
const Type  = require('../type');

class UUIDBaseType extends Type {
  constructor(_options) {
    let options = _options || {};

    super(options);

    Object.defineProperties(this, {
      'options': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        options,
      },
    });
  }

  getOptions() {
    if (typeof this.options === 'function')
      return this.options(this);

    return this.options;
  }

  getPrefix(_options) {
    let options = _options || this.getOptions();
    return options.prefix || '';
  }

  stripPrefix(value) {
    if (!Nife.instanceOf(value, 'string'))
      return { prefix: '', id: value };

    let baseLength  = this.getBaseLength();
    let valueLen    = value.length;
    if (valueLen > baseLength) {
      let diff = valueLen - baseLength;
      return { prefix: value.substring(0, diff), id: value.substring(diff) };
    }

    return { prefix: '', id: value };
  }

  addPrefix(value) {
    if (!Nife.instanceOf(value, 'string'))
      return value;

    let baseLength = this.getBaseLength();
    if (value > baseLength)
      return value;

    let { id } = this.stripPrefix(value);
    return `${this.getPrefix()}${id}`;
  }

  getBaseLength() {
    // eslint-disable-next-line no-magic-numbers
    return 36;
  }

  getTotalLength() {
    let prefix = this.getPrefix();
    return this.getBaseLength() + prefix.length;
  }

  toString(connection) {
    return (connection)
      ? this.toConnectionType(connection)
      : `VARCHAR(${this.getTotalLength()})`;
  }
}

module.exports = UUIDBaseType;
