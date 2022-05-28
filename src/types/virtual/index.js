'use strict';

// One to one relationships
const { ModelType, Model } = require('./model-type');

// One to many, or many to many relationships
const { ModelsType, Models } = require('./models-type');

module.exports = {
  ModelType,
  ModelsType,
  Model,
  Models,
};
