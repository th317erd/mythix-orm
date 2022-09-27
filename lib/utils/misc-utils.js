'use strict';

const Nife          = require('nife');
const { DateTime }  = require('luxon');

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

function valueToDateTime(value, format) {
  if (DateTime.isDateTime(value)) {
    return value;
  } else if (Nife.instanceOf(value, 'number')) {
    return DateTime.fromMillis(value);
  } else if (Nife.instanceOf(value, 'bigint')) {
    return DateTime.fromMillis(Number(value).valueOf());
  } else if (value instanceof Date || (value && value.constructor && value.constructor.name === 'Date')) {
    return DateTime.fromJSDate(value);
  } else if (Nife.instanceOf(value, 'string')) {
    if ((/^\d+$/).test(value))
      return DateTime.fromMillis(parseInt(value, 10));

    if (Nife.isNotEmpty(format))
      return DateTime.fromFormat(value, format);

    return DateTime.fromISO(value);
  } else {
    throw new Error(`MiscUtils::valueToDateTime: Invalid value provided: "${value}"`);
  }
}

module.exports = {
  collect,
  objectAssignSpecial,
  valueToDateTime,
};
