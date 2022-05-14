'use strict';

const Type                      = require('./type');
const { BIGINT, BigIntType }    = require('./bigint');
const { BOOLEAN, BooleanType }  = require('./boolean');
const { INTEGER, IntegerType }  = require('./integer');
const { STRING, StringType }    = require('./string');

module.exports = {
  BigIntType,
  BooleanType,
  IntegerType,
  StringType,
  Type,
  BIGINT,
  BOOLEAN,
  INTEGER,
  STRING,
};
