'use strict';

const Errors            = require('./errors');
const Model             = require('./model');
const ProxyClass        = require('./proxy-class');
const QueryEngineScope  = require('./query-engine');
const ConnectionScope   = require('./connection');
const Types             = require('./types');
const Utils             = require('./utils');

const {
  FieldScope,
  ModelScope,
  QueryEngine,
  QueryEngineBase,
} = QueryEngineScope;

const {
  ConnectionBase,
  QueryGeneratorBase,
  SQLConnectionBase,
  SQLLiterals,
} = ConnectionScope;

module.exports = {
  Errors,
  Model,
  ProxyClass,
  Types,
  Utils,

  // Query Engine
  FieldScope,
  ModelScope,
  QueryEngine,
  QueryEngineBase,

  // Connection
  ConnectionBase,
  QueryGeneratorBase,
  SQLConnectionBase,
  SQLLiterals,
};
