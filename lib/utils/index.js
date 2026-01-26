'use strict';

const MiscUtils   = require('./misc-utils');
const ModelUtils  = require('./model-utils');
const QueryUtils  = require('./query-utils');
const AsyncStore  = require('./async-store');

const {
  collect,
  valueToDateTime,
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
  mergeFields,
} = QueryUtils;

const {
  CONNECTION_KEY,
  getContextStore,
  getContextValue,
  setContextValue,
  runInContext,
  hasContext,
  hasConnection,
  captureContext,
  bindCallback,
  setDebug,
  isDebugEnabled,
  getContextDebugInfo,
} = AsyncStore;

module.exports = {
  MiscUtils,
  ModelUtils,
  QueryUtils,
  AsyncStore,

  // MiscUtils
  collect,
  valueToDateTime,

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
  mergeFields,

  // AsyncStore
  CONNECTION_KEY,
  getContextStore,
  getContextValue,
  setContextValue,
  runInContext,
  hasContext,
  hasConnection,
  captureContext,
  bindCallback,
  setDebug,
  isDebugEnabled,
  getContextDebugInfo,
};
