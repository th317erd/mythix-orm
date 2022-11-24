///! Import `const { Types: { Helpers } } = require('mythix-orm');`
///!
///! Helpers are a set of utilities used to assist with
///! `defaultValue` attributes on model fields.
///!
///! Default values for fields in Mythix ORM are generally used in
///! two different places: As column values directly, or as `DEFAULT VALUE`
///! for columns when defining tables in the database. When creating tables
///! the `defaultValue` of a field will generally only ever be used if it
///! has the `remote` flag set. Otherwise, Mythix ORM simply provides the
///! `defaultValue` to the database directly during `INSERT` and `UPDATE`
///! operations.
///!
///! ```javascript
///! const { Model, Types } = require('mythix-orm');
///!
///! class TestModel extends Model {
///!   static fields = {
///!     myCustomDate: {
///!       type: Types.STRING(32),
///!       // Inform Mythix ORM that our `defaultValue` should be
///!       // applied on every `UPDATE` operation... even if the
///!       // field already has a value.
///!       defaultValue: Types.Helpers.defaultValueFlags(() => {
///!         return (new Date()).toISOString();
///!       }, { onUpdate: true }),
///!       allowNull: false,
///!       index: true,
///!     }
///!   };
///! }
///! ```
///!
///! Properties:
///!   FLAG_ON_INITIALIZE: number = 0x01
///!     This flag informs Mythix ORM that the `defaultValue` should
///!     be fetched and used as soon as a model instance is first
///!     initialized.
///!   FLAG_ON_INSERT: number = 0x02
///!     This flag informs Mythix ORM that the `defaultValue` should
///!     only be used on `INSERT` operations.
///!   FLAG_ON_UPDATE: number = 0x04
///!     This flag informs Mythix ORM that the `defaultValue` should
///!     only be used on `UPDATE` operations. This is used for example
///!     by `Types.DATE.Defaults.NOW.UPDATE`.
///!   FLAG_ON_STORE: number = 0x06
///!     This flag informs Mythix ORM that the `defaultValue` should
///!     only be used on `INSERT` and `UPDATE` operations.
///!   FLAG_LITERAL: number = 0x08
///!     This flag informs Mythix ORM that the `defaultValue` is intended
///!     to be a literal value, and should not be escaped.
///!   FLAG_REMOTE: number = 0x10
///!     This flag informs Mythix ORM that the `defaultValue` is provided
///!     by the database itself. For example the `Types.BIGINT.Defaults.AUTO_INCREMENT`
///!     type uses this flag.
///!
///! DocScope: Helpers
'use strict';

const FLAG_ON_INITIALIZE  = 0x01;
const FLAG_ON_INSERT      = 0x02;
const FLAG_ON_UPDATE      = 0x04;
const FLAG_ON_STORE       = FLAG_ON_INSERT | FLAG_ON_UPDATE;
const FLAG_LITERAL        = 0x08;
const FLAG_REMOTE         = 0x10;

/// Give a `defaultValue` method certain flags to
/// assist Mythix ORM in working with a field's
/// `defaultValue`.
///
/// `defaultValue` flags change the way a `defaultValue`
/// for a field behaves. For example, if the `FLAG_LITERAL`
/// is set, then the database will not escape the default value.
///
/// By default, all `defaultValue` attributes on fields (that
/// are methods) have the flags `FLAG_ON_INITIALIZE`, which
/// simply tells Mythix ORM to call the `defaultValue` method
/// and assign the result to the field as soon as the model is
/// instantiated. This is the default behavior of all `defaultValue`
/// attributes... unless the defined flags change that default
/// behavior. As soon as any flag is set on a `defaultValue` method,
/// the `FLAG_ON_INITIALIZE` value will be cleared.
///
/// This adds an attribute named `mythixFlags` to
/// the provided method. This attribute is then
/// used by Mythix ORM to know what to do with the
/// `defaultValue` method provided on a field.
///
/// Arguments:
///   func: Function
///     The `defaultValue` function to apply flags to.
///   flags: object
///     An object, specifying which flags to enable
///     or disable. The allowed properties are listed in
///     the table below.
///     | Option | Type | Default Value | Description |
///     | ------------- | ---- | ------------- | ----------- |
///     | `onInitialize` | `boolean` | `true` | If `true`, then assign the `defaultValue` as soon as a model is instantiated. |
///     | `onInsert` | `boolean` | `false` | If `true`, then only assign the `defaultValue` when an `INSERT` operation is being executed. |
///     | `onUpdate` | `boolean` | `false` | If `true`, then only assign the `defaultValue` when an `UPDATE` operation is being executed. |
///     | `onStore` | `boolean` | `false` | If `true`, then only assign the `defaultValue` when an `INSERT` or `UPDATE` operation is being executed. |
///     | `literal` | `boolean` | `false` | If `true`, then inform Mythix ORM that the value is a literal, and not to escape it. |
///     | `remote` | `boolean` | `false` | If `true`, then inform Mythix ORM that the value is provided by the database itself. |
///
/// Return: Function
///   The method provided, with a new `mythixFlags` assigned to it.
function defaultValueFlags(func, _flagsObj) {
  let flags = FLAG_ON_INITIALIZE;
  let flagsObj = _flagsObj || {};

  if (flagsObj.onInsert === true)
    flags |= FLAG_ON_INSERT;

  if (flagsObj.onUpdate === true)
    flags |= FLAG_ON_UPDATE;

  if (flagsObj.onStore === true)
    flags |= FLAG_ON_STORE;

  if (flagsObj.literal === true)
    flags |= FLAG_LITERAL;

  if (flagsObj.remote === true)
    flags |= FLAG_REMOTE;

  if (flagsObj.remote === true || flagsObj.onInitialize === false)
    flags = flags & ~FLAG_ON_INITIALIZE;
  else if (flags > 1)
    flags = flags & ~FLAG_ON_INITIALIZE;

  func.mythixFlags = flags;

  return func;
}

/// Fetch the `mythixFlags` attribute on the provided
/// function. If none is found, or no method is provided,
/// then the value `0` will be returned instead.
///
/// Return: number
///   The `defaultValue` flags as a bitmask. If none is found, or no
///   method is provided, then `0` will be returned instead.
///
/// See: Helpers.defaultValueFlags
function getDefaultValueFlags(func) {
  if (!func)
    return 0;

  return func.mythixFlags || 0;
}

/// Check if the provided `defaultValue` method has
/// specific flags set. The flags are provided via
/// their name.
///
/// Arguments:
///   func: Function
///     The `defaultValue` function to check.
///   checkFlags: Array<string>
///     The flags to check. Acceptable values are:
///     `[ 'onInitialize', 'onInsert', 'onUpdate', 'onStore', 'literal', 'remote' ]`
///
/// Return: boolean
///   Return `true` if all provided `checkFlags` are `true`. If any
///   provided flag results in `false`, then the method will return
///   `false`. In short, all provided flags must be `true`, otherwise
///   `false` is returned.
///
/// See: Helpers.defaultValueFlags
function checkDefaultValueFlags(func, checkFlags) {
  if (!checkFlags || !checkFlags.length || !func)
    return false;

  let flags = getDefaultValueFlags(func);
  for (let i = 0, il = checkFlags.length; i < il; i++) {
    let flag = checkFlags[i];

    if ((flag === 'onInitialize' || flag === FLAG_ON_INITIALIZE) && !(flags & FLAG_ON_INITIALIZE))
      return false;

    if ((flag === 'onInsert' || flag === FLAG_ON_INSERT) && !(flags & FLAG_ON_INSERT))
      return false;

    if ((flag === 'onUpdate' || flag === FLAG_ON_UPDATE) && !(flags & FLAG_ON_UPDATE))
      return false;

    if ((flag === 'onStore' || flag === FLAG_ON_STORE) && (flags & FLAG_ON_STORE) !== FLAG_ON_STORE)
      return false;

    if ((flag === 'literal' || flag === FLAG_LITERAL) && !(flags & FLAG_LITERAL))
      return false;

    if ((flag === 'remote' || flag === FLAG_REMOTE) && !(flags & FLAG_REMOTE))
      return false;
  }

  return true;
}

function generatePermutations(func, flags) {
  const masterFunc = defaultValueFlags(func, flags);

  masterFunc.UPDATE = defaultValueFlags(function(...args) {
    return func.apply(this, args);
  }, Object.assign({}, flags, { onUpdate: true }));

  // masterFunc.INSERT = defaultValueFlags(function(...args) {
  //   return func.apply(this, args);
  // }, Object.assign({}, flags, { onInsert: true }));

  // masterFunc.ALWAYS = defaultValueFlags(function(...args) {
  //   return func.apply(this, args);
  // }, Object.assign({}, flags, { onStore: true }));

  return masterFunc;
}

const AUTO_INCREMENT = defaultValueFlags(function(context) {
  return context.connection.getDefaultFieldValue('AUTO_INCREMENT', context);
}, { literal: true, remote: true });

AUTO_INCREMENT._mythixIsAutoIncrement = true;

const DATETIME_NOW = generatePermutations(function(context) {
  return context.connection.getDefaultFieldValue('DATETIME_NOW', context);
}, { literal: true, remote: true });

const DATE_NOW = generatePermutations(function(context) {
  return context.connection.getDefaultFieldValue('DATE_NOW', context);
}, { literal: true, remote: true });

DATETIME_NOW.LOCAL = generatePermutations(function(context) {
  return context.connection.getDefaultFieldValue('DATETIME_NOW_LOCAL', context);
});

DATE_NOW.LOCAL = generatePermutations(function(context) {
  return context.connection.getDefaultFieldValue('DATE_NOW_LOCAL', context);
});

module.exports = {
  FLAG_ON_INITIALIZE,
  FLAG_ON_INSERT,
  FLAG_ON_UPDATE,
  FLAG_ON_STORE,
  FLAG_LITERAL,
  FLAG_REMOTE,
  defaultValueFlags,
  getDefaultValueFlags,
  checkDefaultValueFlags,
  AUTO_INCREMENT,
  DATETIME_NOW,
  DATE_NOW,
};
