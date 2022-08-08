'use strict';

const MiscUtils   = require('./misc-utils');
const ModelUtils  = require('./model-utils');
const QueryUtils  = require('./query-utils');

const {
  collect,
  copyStaticProps,
  objectAssignSpecial,
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

const {
  parseFilterFieldAndOperator,
  generateQueryFromFilter,
} = QueryUtils;

module.exports = {
  MiscUtils,
  ModelUtils,

  // MiscUtils
  collect,
  copyStaticProps,
  objectAssignSpecial,
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

  // QueryUtils
  parseFilterFieldAndOperator,
  generateQueryFromFilter,
};
