'use strict';

const Type                      = require('./type');
const { BIGINT, BigIntType }    = require('./bigint-type');
const { BOOLEAN, BooleanType }  = require('./boolean-type');
const { INTEGER, IntegerType }  = require('./integer-type');
const { STRING, StringType }    = require('./string-type');

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
