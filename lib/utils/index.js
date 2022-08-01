'use strict';

const MiscUtils   = require('./misc-utils');
const ModelUtils  = require('./model-utils');
const JSONUtils   = require('./json-utils');

module.exports = {
  MiscUtils,
  ModelUtils,
  JSONUtils,

  // MiscUtils
  collect:                                    MiscUtils.collect,
  copyStaticProps:                            MiscUtils.copyStaticProps,
  flattenObjectProperties:                    MiscUtils.flattenObjectProperties,
  iterateStaticProps:                         MiscUtils.iterateStaticProps,

  // ModelUtils
  assignRelatedModels:                        ModelUtils.assignRelatedModels,
  buildQueryFromModelsAttributes:             ModelUtils.buildQueryFromModelsAttributes,
  constructModelsForCreationFromOriginField:  ModelUtils.constructModelsForCreationFromOriginField,
  createAndSaveAllRelatedModels:              ModelUtils.createAndSaveAllRelatedModels,
  fieldToFullyQualifiedName:                  ModelUtils.fieldToFullyQualifiedName,
  getPrimaryKeysForModels:                    ModelUtils.getPrimaryKeysForModels,
  getRelationalModelStatusForField:           ModelUtils.getRelationalModelStatusForField,
  injectModelMethod:                          ModelUtils.injectModelMethod,
  isUUID:                                     ModelUtils.isUUID,
  parseQualifiedName:                         ModelUtils.parseQualifiedName,
  sanitizeFieldString:                        ModelUtils.sanitizeFieldString,
  setRelationalValues:                        ModelUtils.setRelationalValues,
  sortModelNamesByCreationOrder:              ModelUtils.sortModelNamesByCreationOrder,

  // JSONUtils
  serialize:                                  JSONUtils.serialize,
  deserialize:                                JSONUtils.deserialize,
};
