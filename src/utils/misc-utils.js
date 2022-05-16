'use strict';

function copyClassStaticProps(Klass, destination) {
  if (typeof Klass !== 'function')
    throw new TypeError('Utils::copyClassStaticProps: "Klass" argument must be a function');

  let keys = Object.getOwnPropertyNames(Klass);

  for (let i = 0, il = keys.length; i < il; i++) {
    let key = keys[i];
    if (key === 'length' || key === 'name' || key === 'prototype')
      continue;

    let value = Klass[key];
    if (typeof value === 'function')
      value = value.bind(Klass);

    Object.defineProperties(destination, {
      [key]: {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value,
      },
    });
  }
}

module.exports = {
  copyClassStaticProps,
};
