'use strict';

function iterateStaticProps(Klass, callback) {
  if (typeof Klass !== 'function')
    throw new TypeError('Utils::iterateStaticProps: "Klass" argument must be a function.');

  let prototype = Klass.prototype;
  let keys = Object.getOwnPropertyNames(Klass);

  for (let i = 0, il = keys.length; i < il; i++) {
    let key = keys[i];
    if (key === 'length' || key === 'name' || key === 'prototype')
      continue;

    let value = Klass[key];
    callback({ value, key, Klass, prototype });
  }
}

function copyStaticProps(Klass, destination) {
  if (typeof Klass !== 'function')
    throw new TypeError('Utils::copyStaticProps: "Klass" argument must be a function.');

  iterateStaticProps(Klass, ({ value, key }) => {
    Object.defineProperties(destination, {
      [key]: {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        value,
      },
    });
  });
}

async function collect(iterator) {
  let items = [];

  for await (let item of iterator)
    items.push(item);

  return items;
}

function objectAssignSpecial(obj, proto, skipKeys) {
  const isKeySkipped = (key) => {
    if (Array.isArray(skipKeys))
      return (skipKeys.indexOf(key) >= 0);

    return Object.prototype.hasOwnProperty.call(skipKeys, key);
  };

  let newObj  = Object.create(proto || {});
  let keys    = Object.keys(obj);

  // eslint-disable-next-line guard-for-in
  for (let i = 0, il = keys.length; i < il; i++) {
    let key = keys[i];
    if (skipKeys && isKeySkipped(key))
      continue;

    newObj[key] = obj[key];
  }

  return newObj;
}

module.exports = {
  collect,
  copyStaticProps,
  objectAssignSpecial,
  iterateStaticProps,
};
