'use strict';

const Type          = require('./type');
const ConcreteTypes = require('./concrete');
const VirtualTypes  = require('./virtual');
const Helpers       = require('./helpers');

module.exports = Object.assign(
  {
    Type,
    Helpers,
  },
  ConcreteTypes,
  VirtualTypes,
);
