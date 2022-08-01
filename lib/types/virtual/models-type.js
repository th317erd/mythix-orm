'use strict';

const Nife                = require('nife');
const Inflection          = require('inflection');
const RelationalTypeBase  = require('./relational-type-base');
const Utils               = require('../../utils');

const NAMED_METHOD  = false;
const ROOT_METHOD   = true;

async function destroyRelatedModels(connection, field, type, TargetModel, storedModels, invertMatch, options) {
  // Destroy all relations not matching stored set
  let thisMethodName      = type.fieldNameToOperationName(field, 'set', NAMED_METHOD);
  let parentModelName     = this.getModelName();
  let idMap               = Utils.getPrimaryKeysForModels(TargetModel, storedModels, { includeRelations: true });
  let relationMap         = Utils.getRelationalModelStatusForField(connection, this, field);
  let sortedModelNames    = Nife.subtractFromArray(Utils.sortModelNamesByCreationOrder(connection, Object.keys(relationMap)), [ parentModelName ]);
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
    let foreignFieldNames = ThisModel.getForeignKeysTargetFieldNames(parentModelName);
    if (Nife.isEmpty(foreignFieldNames))
      continue;

    // Create query to destroy related models
    let query = ThisModel.where[thisModelPKFieldName];
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
      let targetForeignField = ThisModel.getForeignKeysTargetField(targetRelationName, targetPKFieldName);
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
const INJECT_TYPE_METHODS = {
  'addTo': async function({ field }, _models, options) {
    if (Nife.isEmpty(_models))
      return [];

    // TODO: Needs to manage unique constraints
    // for through tables. Even though the model
    // might be persisted, the through table record
    // might still be created, which might blow up
    // if a constraint fails because the records
    // already exist
    let models = Nife.toArray(_models);
    return this.getConnection().transaction(async (connection) => {
      let currentModels = this[field.fieldName];
      if (Nife.isEmpty(currentModels))
        currentModels = [];

      let storedModels  = await Utils.createAndSaveAllRelatedModels(connection, this, field, models, options);
      let allModels     = currentModels.concat(storedModels);

      this[field.fieldName] = allModels;

      return allModels;
    }, options);
  },
  'get': async function*({ field, type }, queryEngine, options) {
    let query                       = type.prepareQuery(this, field, queryEngine);
    let results                     = query.all(options);
    let primaryModelRelationalArray = [];

    this[field.fieldName] = primaryModelRelationalArray;

    for await (let item of results) {
      primaryModelRelationalArray.push(item);

      yield item;
    }
  },
  'set': async function({ field, type }, models, options) {
    let TargetModel = type.getTargetModel({ recursive: true, followForeignKeys: true });

    return this.getConnection().transaction(async (connection) => {
      // Reset relation so we don't end up with
      // current persisted model instances
      this[field.fieldName] = [];

      // First, create models in set
      let addToMethodName = type.fieldNameToOperationName(field, 'addTo', NAMED_METHOD);
      let storedModels    = await this[addToMethodName](models, Object.assign({}, options || {}, { transaction: connection }));

      await destroyRelatedModels.call(this, connection, field, type, TargetModel, storedModels, false, options);

      return storedModels;
    }, options);
  },
  'removeFrom': async function({ field, type }, _models, _options) {
    let TargetModel     = type.getTargetModel({ recursive: true, followForeignKeys: true });
    let targetModelName = TargetModel.getModelName();
    let models          = Nife.toArray(_models);

    return this.getConnection().transaction(async (connection) => {
      const needsLoad = () => {
        let relationMap       = Utils.getRelationalModelStatusForField(connection, this, field);
        let sortedModelNames  = Utils.sortModelNamesByCreationOrder(connection, Object.keys(relationMap));

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

            let relation        = relationMap[modelName];
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

      let options = Object.assign({}, _options || {}, { transaction: connection });
      let storedModels;

      if (needsLoad()) {
        let query = Utils.buildQueryFromModelsAttributes(TargetModel, models);
        if (!query)
          throw new Error(`${this.getModelName()}::${type.fieldNameToOperationName(field, 'removeFrom', NAMED_METHOD)}: Data provided is insufficient to complete operation.`);

        let getMethodName = type.fieldNameToOperationName(field, 'get', NAMED_METHOD);
        storedModels      = await Utils.collect(this[getMethodName](query, options));
      } else {
        storedModels = models;
      }

      // Destroy target relations to removed
      // requested models from set
      await destroyRelatedModels.call(this, connection, field, type, TargetModel, storedModels, true, options);

      return storedModels.length;
    });
  },
  'destroy': function({ field, type }, queryEngine, options) {
    let query = type.prepareQuery(this, field, queryEngine);
    return query.destroy(options);
  },
  'count': function({ field, type }, queryEngine, options) {
    let query = type.prepareQuery(this, field, queryEngine);
    return query.count(null, options);
  },
  'has': async function({ field, type }, queryEngine, options) {
    let countMethodName = type.fieldNameToOperationName(field, 'count', NAMED_METHOD);
    let count           = await this[countMethodName](queryEngine, options);

    return (count > 0);
  },
};

const INJECT_TYPE_METHODS_KEYS = Object.keys(INJECT_TYPE_METHODS);

class ModelsType extends RelationalTypeBase {
  static exposeToModel() {
    return false;
  }

  castToType({ value, connection }) {
    if (value == null)
      return [];

    let Model = this.getTargetModel({ recursive: true, followForeignKeys: true }, connection);
    if (!Model)
      throw new TypeError('ModelsType::castToType: Failed when attempting to fetch the required model.');

    let values = Nife.toArray(value).filter(Boolean).map((value, index) => {
      if (value instanceof Model)
        return value;

      if (!Nife.instanceOf(value, 'object'))
        throw new TypeError(`ModelsType::castToType: Unable to cast provided value at index ${index}. Value must be a model instance, or a raw object.`);

      return new Model(value);
    });

    return values;
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

  onModelInstantiated(modelInstance, field) {
    for (let i = 0, il = INJECT_TYPE_METHODS_KEYS.length; i < il; i++) {
      let operation       = INJECT_TYPE_METHODS_KEYS[i];
      let method          = INJECT_TYPE_METHODS[operation];
      let methodName      = this.fieldNameToOperationName(field, operation, NAMED_METHOD);
      let fullMethodName  = this.fieldNameToOperationName(field, operation, ROOT_METHOD);

      Utils.injectModelMethod(modelInstance, method.bind(modelInstance, { methodName, fullMethodName, field, type: this }), methodName, fullMethodName);
    }
  }
}

module.exports = {
  Models: RelationalTypeBase.wrapConstructor(ModelsType),
  ModelsType,
};
