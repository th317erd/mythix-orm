'use strict';

const Nife          = require('nife');
const { DateTime }  = require('luxon');

async function collect(iterator) {
  let items = [];

  for await (let item of iterator)
    items.push(item);

  return items;
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
  valueToDateTime,
};
