'use strict';

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
  objectAssignSpecial,
};
