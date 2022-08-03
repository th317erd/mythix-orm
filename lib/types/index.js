'use strict';

const Type          = require('./type');
const ConcreteTypes = require('./concrete');
const VirtualTypes  = require('./virtual');
const Helpers       = require('./helpers');

const {
  BigIntType,
  BlobType,
  BooleanType,
  CharType,
  DateTimeType,
  DateType,
  FloatType,
  ForeignKeyType,
  IntegerType,
  StringType,
  TextType,
  UUIDV1Type,
  UUIDV3Type,
  UUIDV4Type,
  UUIDV5Type,
  XIDType,
  BIGINT,
  BLOB,
  BOOLEAN,
  CHAR,
  DATE,
  DATETIME,
  FLOAT,
  FOREIGN_KEY,
  INTEGER,
  STRING,
  TEXT,
  UUIDV1,
  UUIDV3,
  UUIDV4,
  UUIDV5,
  XID,
} = ConcreteTypes;

const {
  ModelType,
  ModelsType,
  Model,
  Models,
} = VirtualTypes;

module.exports = {
  DefaultHelpers: Helpers.DefaultHelpers,
  Type,
  Helpers,

  // Concrete Types
  BigIntType,
  BlobType,
  BooleanType,
  CharType,
  DateTimeType,
  DateType,
  FloatType,
  ForeignKeyType,
  IntegerType,
  StringType,
  TextType,
  UUIDV1Type,
  UUIDV3Type,
  UUIDV4Type,
  UUIDV5Type,
  XIDType,
  BIGINT,
  BLOB,
  BOOLEAN,
  CHAR,
  DATE,
  DATETIME,
  FLOAT,
  FOREIGN_KEY,
  INTEGER,
  STRING,
  TEXT,
  UUIDV1,
  UUIDV3,
  UUIDV4,
  UUIDV5,
  XID,

  // Virtual Types
  ModelType,
  ModelsType,
  Model,
  Models,
};
