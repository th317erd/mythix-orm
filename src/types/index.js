'use strict';

const Type          = require('./type');
const ConcreteTypes = require('./concrete');
const VirtualTypes  = require('./virtual');

module.exports = Object.assign(
  {
    Type,
  },
  ConcreteTypes,
  VirtualTypes,
);
