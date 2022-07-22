'use strict';

const FLAG_ON_INITIALIZE  = 0x01;
const FLAG_ON_CREATE      = 0x02;
const FLAG_ON_UPDATE      = 0x04;
const FLAG_ON_STORE       = 0x06;
const FLAG_LITERAL        = 0x08;
const FLAG_REMOTE         = 0x10;

function defaultValueFlags(func, _flagsObj) {
  let flags = FLAG_ON_INITIALIZE;
  let flagsObj = _flagsObj || {};

  if (flagsObj.onCreate === true)
    flags |= FLAG_ON_CREATE;

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

function getDefaultValueFlags(func) {
  if (!func)
    return 0;

  return func.mythixFlags || 0;
}

function checkDefaultValueFlags(func, checkFlags) {
  if (!checkFlags || !func)
    return false;

  let flags = getDefaultValueFlags(func);
  for (let i = 0, il = checkFlags.length; i < il; i++) {
    let flag = checkFlags[i];

    if ((flag === 'onInitialize' || flag === FLAG_ON_INITIALIZE) && !(flags & FLAG_ON_INITIALIZE))
      return false;

    if ((flag === 'onCreate' || flag === FLAG_ON_CREATE) && !(flags & FLAG_ON_CREATE))
      return false;

    if ((flag === 'onUpdate' || flag === FLAG_ON_UPDATE) && !(flags & FLAG_ON_UPDATE))
      return false;

    if ((flag === 'onStore' || flag === FLAG_ON_STORE) && !(flags & FLAG_ON_STORE))
      return false;

    if ((flag === 'literal' || flag === FLAG_LITERAL) && !(flags & FLAG_LITERAL))
      return false;

    if ((flag === 'remote' || flag === FLAG_REMOTE) && !(flags & FLAG_REMOTE))
      return false;
  }

  return true;
}

const AUTO_INCREMENT = defaultValueFlags(function(context) {
  return context.connection.getDefaultFieldValue('AUTO_INCREMENT');
}, { literal: true, remote: true });

const DATETIME_NOW = defaultValueFlags(function(context) {
  return context.connection.getDefaultFieldValue('DATETIME_NOW');
}, { literal: true, remote: true });

const DATE_NOW = defaultValueFlags(function(context) {
  return context.connection.getDefaultFieldValue('DATE_NOW');
}, { literal: true, remote: true });

const DATETIME_NOW_LOCAL = defaultValueFlags(function(context) {
  return context.connection.getDefaultFieldValue('DATETIME_NOW_LOCAL');
}, { literal: true, remote: true });

const DATE_NOW_LOCAL = defaultValueFlags(function(context) {
  return context.connection.getDefaultFieldValue('DATE_NOW_LOCAL');
}, { literal: true });

module.exports = {
  FLAG_ON_INITIALIZE,
  FLAG_ON_CREATE,
  FLAG_ON_UPDATE,
  FLAG_ON_STORE,
  FLAG_LITERAL,
  FLAG_REMOTE,
  defaultValueFlags,
  getDefaultValueFlags,
  checkDefaultValueFlags,
  AUTO_INCREMENT,
  DATETIME_NOW,
  DATETIME_NOW_LOCAL,
  DATE_NOW,
  DATE_NOW_LOCAL,
};
