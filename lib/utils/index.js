'use strict';

const MiscUtils   = require('./misc-utils');
const ModelUtils  = require('./model-utils');

const {
  collect,
  copyStaticProps,
  flattenObjectProperties,
  iterateStaticProps,
} = MiscUtils;

const {
  assignRelatedModels,
  buildQueryFromModelsAttributes,
  constructModelsForCreationFromOriginField,
  createAndSaveAllRelatedModels,
  fieldToFullyQualifiedName,
  getPrimaryKeysForModels,
  getRelationalModelStatusForField,
  injectModelMethod,
  isUUID,
  parseQualifiedName,
  sanitizeFieldString,
  setRelationalValues,
  sortModelNamesByCreationOrder,
  sortModelNamesByDependencyOrder,
} = ModelUtils;

module.exports = {
  MiscUtils,
  ModelUtils,

  // MiscUtils
  collect,
  copyStaticProps,
  flattenObjectProperties,
  iterateStaticProps,

  // ModelUtils
  assignRelatedModels,
  buildQueryFromModelsAttributes,
  constructModelsForCreationFromOriginField,
  createAndSaveAllRelatedModels,
  fieldToFullyQualifiedName,
  getPrimaryKeysForModels,
  getRelationalModelStatusForField,
  injectModelMethod,
  isUUID,
  parseQualifiedName,
  sanitizeFieldString,
  setRelationalValues,
  sortModelNamesByCreationOrder,
  sortModelNamesByDependencyOrder,
};
