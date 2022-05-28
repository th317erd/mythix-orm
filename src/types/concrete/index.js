'use strict';

const { BIGINT, BigIntType }    = require('./bigint-type');
const { BOOLEAN, BooleanType }  = require('./boolean-type');
const { INTEGER, IntegerType }  = require('./integer-type');
const { STRING, StringType }    = require('./string-type');
const { UUIDV1, UUIDV1Type }    = require('./uuid-v1-type');
const { UUIDV4, UUIDV4Type }    = require('./uuid-v4-type');

module.exports = {
  BigIntType,
  BooleanType,
  IntegerType,
  StringType,
  UUIDV1Type,
  UUIDV4Type,
  BIGINT,
  BOOLEAN,
  INTEGER,
  STRING,
  UUIDV1,
  UUIDV4,
};
