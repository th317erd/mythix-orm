///! import `var { Utils: { MiscUtils } } = require('mythix-orm');`
///!
///! MiscUtils utilities provide some miscellaneous utility
///! functions for assisting with some common operations.
///!
///! DocScope: MiscUtils

'use strict';

const Nife          = require('nife');
const { DateTime }  = require('luxon');

/// When provided an async iterator,
/// collect all results from the iterator
/// until the iterator is exhausted.
///
/// Arguments:
///   iterator: async * iterator
///     The async generator iterator to collect items from.
///
/// Return: Promise<Array<any>>
///   Return the collected results as an array.
async function collect(iterator) {
  let items = [];

  for await (let item of iterator)
    items.push(item);

  return items;
}

/// Take a timestamp, a string, a Date instance,
/// or a Luxon [DateTime](https://moment.github.io/luxon/#/) instance
/// and convert the input to a Luxon [DateTime](https://moment.github.io/luxon/#/) instance.
///
/// Arguments:
///   value: number | string | Date | [DateTime](https://moment.github.io/luxon/#/)
///     The value to use to create a new Luxon [DateTime](https://moment.github.io/luxon/#/) instance.
///     If this is a `number` or `bigint`, then it will be assumed this is a timestamp, and
///     the provided `format` will be ignored. If this is a `string`, the provided `format`
///     will be used to parse it into a `DateTime` instance. If this is a `Date` instance, then
///     it will be converted to a Luxon `DateTime` instance. A Luxon `DateTime` instance will
///     simply be returned.
///   format?: string
///     The Luxon [DateTime](https://moment.github.io/luxon/#/) format used to parse
///     the date/time if the provided `value` is a string. This is only required if
///     the format is something that Luxon doesn't natively understand (i.e. an ISO format).
///
/// Return: [DateTime](https://moment.github.io/luxon/#/)
///   The newly created Luxon `DateTime` instance. If a Luxon `DateTime`
///   instance was provided as the `value` argument, then it will simply
///   be returned.
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
