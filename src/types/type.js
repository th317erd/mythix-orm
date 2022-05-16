'use strict';

const MiscUtils = require('../utils/misc-utils');

class Type {
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

    MiscUtils.copyClassStaticProps(TypeKlass, func);

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
}

module.exports = Type;
