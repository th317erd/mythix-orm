'use strict';

const MiscUtils = require('../utils/misc-utils');

class Type {
  static isVirtual() {
    return false;
  }

  static wrapConstructor(TypeKlass) {
    let func = function(...args) {
      return new TypeKlass(...args);
    };

    Object.defineProperties(func, {
      'mythixType': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        TypeKlass,
      },
    });

    MiscUtils.copyStaticProps(TypeKlass, func);

    return func;
  }

  constructor(...args) {
    Object.defineProperties(this, {
      '_args': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        args,
      },
    });
  }

  castToType(...args) {
    return this.constructor.castToType.apply(this.constructor, args);
  }
}

module.exports = Type;
