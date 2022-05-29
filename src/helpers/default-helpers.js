'use strict';

const FLAG_ON_INITIALIZE  = 0x01;
const FLAG_ON_CREATE      = 0x02;
const FLAG_ON_UPDATE      = 0x04;
const FLAG_ON_STORE       = 0x06;
const FLAG_REMOTE         = 0x08;

function defaultValueFlags(func, _flagsObj) {
  let flags = FLAG_ON_INITIALIZE;
  let flagsObj = _flagsObj || {};

  if (flagsObj.onCreate === true)
    flags |= FLAG_ON_CREATE;

  if (flagsObj.onUpdate === true)
    flags |= FLAG_ON_UPDATE;

  if (flagsObj.onStore === true)
    flags |= FLAG_ON_STORE;

  if (flagsObj.remote === true)
    flags |= FLAG_REMOTE;

  if (flagsObj.onInitialize === false)
    flags = flags & ~FLAG_ON_INITIALIZE;
  else if (flags > 1)
    flags = flags & ~FLAG_ON_INITIALIZE;

  func.mythixFlags = flags;

  return func;
}

const AUTO_INCREMENT = defaultValueFlags(function(context) {
  return context.connection.getDefaultFieldValue('AUTO_INCREMENT');
}, { remote: true });

const DATETIME_NOW = defaultValueFlags(function(context) {
  return context.connection.getDefaultFieldValue('DATETIME_NOW');
}, { remote: true });

const DATE_NOW = defaultValueFlags(function(context) {
  return context.connection.getDefaultFieldValue('DATE_NOW');
}, { remote: true });

module.exports = {
  FLAG_ON_INITIALIZE,
  FLAG_ON_CREATE,
  FLAG_ON_UPDATE,
  FLAG_ON_STORE,
  FLAG_REMOTE,
  defaultValueFlags,
  AUTO_INCREMENT,
  DATETIME_NOW,
  DATE_NOW,
};
