'use strict';

const Nife                = require('nife');
const Inflection          = require('inflection');
const RelationalTypeBase  = require('./relational-type-base');
const Utils               = require('../../utils');

const NAMED_METHOD = false;

async function destroyRelatedModels(connection, field, type, TargetModel, storedModels, invertMatch, options, operation) {
  // Destroy all relations not matching stored set
  let thisMethodName      = type.fieldNameToOperationName(field, operation, NAMED_METHOD);
  let parentModelName     = this.getModelName();
  let idMap               = Utils.getPrimaryKeysForModels(TargetModel, storedModels, { includeRelations: true });
  let { relationalMap }   = await Utils.getRelationalModelStatusForField(connection, this, field);
  let sortedModelNames    = Nife.subtractFromArray(Utils.sortModelNamesByCreationOrder(connection, Object.keys(relationalMap)), [ parentModelName ]);
  let targetRelationName  = sortedModelNames[0];
  let destroyModelNames   = sortedModelNames.slice(1).reverse();
  let targetIDs           = idMap[targetRelationName];
  let targetPKFieldName   = TargetModel.getPrimaryKeyFieldName();

  if (destroyModelNames.length === 0) {
    // No through-table... this is a one-to-many relation.
    // We can only destroy if the TargetModel has a foreign key
    // to the parent model.

    // Set destroyModelNames to the first relation.
    // The following code will ensure they only get
    // deleted if there is a foreign key relationship
    // to the parent model.
    destroyModelNames = [ targetRelationName ];
  }

  for (let i = 0, il = destroyModelNames.length; i < il; i++) {
    let modelName = destroyModelNames[i];
    let ids       = idMap[modelName];

    // If we have no ids for this model, then
    // there is nothing we can destroy
    if (Nife.isEmpty(ids))
      continue;

    let ThisModel             = connection.getModel(modelName);
    let thisModelPKFieldName  = ThisModel.getPrimaryKeyFieldName();
    if (!thisModelPKFieldName) {
      // Not sure how this would happen... since we obviously
      // have a list of ids already... but let's be safe anyway
      continue;
    }

    // We can only destroy models that have a foreign key
    // to the parent model.
    let foreignFieldNames = ThisModel.getForeignKeysTargetFieldNames(connection, parentModelName);
    if (Nife.isEmpty(foreignFieldNames))
      continue;

    // Create query to destroy related models
    let query = ThisModel.where(connection)[thisModelPKFieldName];
    if (invertMatch)
      query = query.EQ(ids);
    else
      query = query.NOT.EQ(ids);

    for (let j = 0, jl = foreignFieldNames.length; j < jl; j++) {
      let { targetFieldName, sourceFieldName } = foreignFieldNames[j];
      let parentModelValue = this[targetFieldName];

      // If this value is empty then something is wrong...
      // so fail early to prevent unwanted data loss
      if (parentModelValue == null)
        throw new Error(`${parentModelName}::${thisMethodName}: Field "${parentModelName}.${targetFieldName}" can not be empty for this operation.`);

      query = query.AND[sourceFieldName].EQ(parentModelValue);
    }

    // If there is a foreign field that points to the target
    // relation, then also match against those
    if (targetPKFieldName && Nife.isNotEmpty(targetIDs)) {
      let targetForeignField = ThisModel.getForeignKeysTargetField(connection, targetRelationName, targetPKFieldName);
      if (targetForeignField) {
        if (invertMatch)
          query = query.AND[modelName][targetForeignField.sourceFieldName].EQ(targetIDs);
        else
          query = query.AND[modelName][targetForeignField.sourceFieldName].NOT.EQ(targetIDs);
      }
    }

    // Now destroy relations to update set
    await query.destroy(options);
  }
}

// These get injected into the class as
// addTo{fieldName}, get{fieldName}, etc...
const TYPE_OPERATIONS = {
  'queryFor': async function({ field, type }, userQuery, options, ...args) {
    return await type.prepareQuery({ connection: null, self: this, field, userQuery, options }, args);
  },
  'addTo': async function({ field }, _models, options) {
    let models = Nife.toArray(_models).filter(Boolean);
    if (Nife.isEmpty(models))
      return [];

    // TODO: Needs to manage unique constraints
    // for through tables. Even though the model
    // might be persisted, the through table record
    // might still be created, which might blow up
    // if a constraint fails because the records
    // already exist
    return this.getConnection(options && options.connection).transaction(async (connection) => {
      let currentModels = this[field.fieldName];
      if (Nife.isEmpty(currentModels))
        currentModels = [];

      let storedModels  = await Utils.createAndSaveAllRelatedModels(connection, this, field, models, options);
      let allModels     = currentModels.concat(storedModels);

      this[field.fieldName] = allModels;

      return allModels;
    }, options);
  },
  'get': function({ field, type }, userQuery, options, ...args) {
    const doGet = async function*() {
      let query                       = await type.prepareQuery({ connection: null, self: this, field, userQuery, options }, args);
      let results                     = query.all(Object.assign({}, options, { stream: true }));
      let primaryModelRelationalArray = [];

      this[field.fieldName] = primaryModelRelationalArray;

      for await (let item of results) {
        primaryModelRelationalArray.push(item);

        yield item;
      }
    };

    if (options && options.stream === true)
      return doGet.call(this);
    else
      return Utils.collect(doGet.call(this));
  },
  'set': async function({ field, type, addTo }, models, options) {
    return this.getConnection(options && options.connection).transaction(async (connection) => {
      let TargetModel = type.getTargetModel(connection);

      // Reset relation so we don't end up with
      // current persisted model instances
      this[field.fieldName] = [];

      // First, create models in set
      let storedModels = await addTo.call(this, models, Object.assign({}, options || {}, { connection }));

      await destroyRelatedModels.call(this, connection, field, type, TargetModel, storedModels, false, options, 'set');

      return storedModels;
    }, options);
  },
  'removeFrom': async function({ field, type, get }, _models, _options) {
    return this.getConnection(_options && _options.connection).transaction(async (connection) => {
      let { relationalMap, TargetModel } = await Utils.getRelationalModelStatusForField(connection, this, field);
      let targetModelName = TargetModel.getModelName();
      let models          = Nife.toArray(_models);

      const needsLoad = async () => {
        let sortedModelNames = Utils.sortModelNamesByCreationOrder(connection, Object.keys(relationalMap));

        for (let i = 0, il = models.length; i < il; i++) {
          let model = models[i];

          if (!(model instanceof TargetModel))
            return true;

          if (!model.isPersisted())
            return true;

          for (let j = 0, jl = sortedModelNames.length; j < jl; j++) {
            let modelName = sortedModelNames[j];
            if (modelName === targetModelName)
              continue;

            if (!model._)
              return true;

            let relation        = relationalMap[modelName];
            let ThisModel       = relation.Model;
            let pluralModelName = ThisModel.getPluralModelName();
            let relationSet     = model._[pluralModelName];

            if (Nife.isEmpty(relationSet))
              return true;

            if (!relationSet.every((relatedModel) => relatedModel.isPersisted()))
              return true;
          }
        }

        return false;
      };

      let options = Object.assign({}, _options || {}, { connection });
      let storedModels;

      if (await needsLoad()) {
        let query = Utils.generateQueryFromFilter(connection, TargetModel, models);
        if (!query)
          throw new Error(`${this.getModelName()}::${type.fieldNameToOperationName(field, 'removeFrom', NAMED_METHOD)}: Data provided is insufficient to complete operation.`);

        storedModels = await get.call(this, TargetModel.where(connection).AND(query), Object.assign({}, options, { stream: false, includeRelations: true, connection }));
      } else {
        storedModels = models;
      }

      // Destroy target relations to removed
      // requested models from set
      await destroyRelatedModels.call(this, connection, field, type, TargetModel, storedModels, true, options, 'removeFrom');

      return storedModels.length;
    });
  },
  'destroy': async function({ field, type }, userQuery, options, ...args) {
    let query = await type.prepareQuery({ connection: null, self: this, field, userQuery, options }, args);
    return await query.destroy(options);
  },
  'count': async function({ field, type }, userQuery, options, ...args) {
    let query = await type.prepareQuery({ connection: null, self: this, field, userQuery, options }, args);
    return await query.count(null, options);
  },
  'has': async function({ count }, userQuery, options, ...args) {
    let itemCount = await count.call(this, userQuery, options, ...args);
    return (itemCount > 0);
  },
};

class ModelsType extends RelationalTypeBase {
  static exposeToModel() {
    return false;
  }

  isManyRelation() {
    return true;
  }

  fieldNameToOperationName(field, operation, rootMethod) {
    let fieldName = field.pluralName;
    if (!fieldName)
      fieldName = Nife.capitalize(Inflection.pluralize(field.fieldName));

    if (rootMethod)
      return `_${operation}${fieldName}`;
    else
      return `${operation}${fieldName}`;
  }

  initialize(connection, self) {
    return super.initialize(connection, self, TYPE_OPERATIONS);
  }
}

module.exports = {
  Models: RelationalTypeBase.wrapConstructor(ModelsType),
  ModelsType,
};
