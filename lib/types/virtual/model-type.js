'use strict';

const Nife                = require('nife');
const RelationalTypeBase  = require('./relational-type-base');
const ModelUtils          = require('../../utils/model-utils');

// These get injected into the class as
// create{fieldName}, get{fieldName}, etc...
const TYPE_OPERATIONS = {
  'queryFor': async function({ field, type }, userQuery, options, ...args) {
    return await type.prepareQuery({ connection: null, self: this, field, userQuery, options }, args);
  },
  'create': async function({ field, type, get }, model, options) {
    if (!model)
      return false;

    let result = await this.getConnection(options && options.connection).transaction(async (connection) => {
      let TargetModel = type.getTargetModel(connection);

      // Why are we fetching on get? Because this
      // could be a complex relationship, and there
      // might already be something that exists.
      // TODO: Likely a good area for performance
      // improvements... we can likely detect if
      // the relationship is too complex to guess
      // or not.
      let fetchedModel  = await get.call(this, TargetModel.where(connection).LIMIT(1), options);
      if (fetchedModel && fetchedModel instanceof TargetModel) {
        fetchedModel.setAttributes(model, true);
        if (fetchedModel.isDirty())
          await fetchedModel.save(options);

        return fetchedModel;
      }

      let storedModels  = await ModelUtils.createAndSaveAllRelatedModels(connection, this, field, [ model ], options);
      let resultModel   = storedModels[0];

      // Update this model to reflect the update
      ModelUtils.setRelationalValues(connection, this.getModel(), this, resultModel.getModel(), resultModel);

      return resultModel;
    }, options);

    // Save needs to go outside of the transaction
    // otherwise some DBs will violate foreign key
    // constraints
    if (this.isDirty())
      await this.save(options);

    return result;
  },
  'get': async function({ field, type }, userQuery, options, ...args) {
    let query = await type.prepareQuery({ connection: null, self: this, field, userQuery, options }, args);
    return await query.first(null, options);
  },
  'update': async function({ fullMethodName, field, type, create }, attributes, _options, ...args) {
    let options = _options || {};

    let result = await this.getConnection(options.connection).transaction(async (connection) => {
      let query = await type.prepareQuery({ connection, self: this, field, options }, args);
      let model = await query.first(null, options);

      if (!model) {
        if (options.force === true)
          return await create.call(this, attributes, options);

        throw new Error(`${field.Model.getModelName}::${fullMethodName}: Model not found to update. You can pass "{ force: true }" to the options to force a creation instead.`);
      }

      model.setAttributes(attributes, true);
      if (!model.isDirty())
        return model;

      let [ storedModel ] = await connection.update(model.getModel(), [ model ], options);

      // Update this model to reflect the update
      ModelUtils.setRelationalValues(connection, this.getModel(), this, storedModel.getModel(), storedModel);

      return storedModel;
    }, options);

    // Save needs to go outside of the transaction
    // otherwise some DBs will violate foreign key
    // constraints
    if (this.isDirty())
      await this.save(options);

    return result;
  },
  'destroy': async function({ field, type }, options, ...args) {
    let result = await this.getConnection(options && options.connection).transaction(async (connection) => {
      let query = await type.prepareQuery({ connection, self: this, field, options }, args);
      let model = await query.first(options);

      if (!model)
        return false;

      await connection.destroy(model.getModel(), [ model ], options);

      // Update this model to reflect the deletion
      ModelUtils.setRelationalValues(connection, this.getModel(), this, model.getModel());

      return true;
    }, options);

    // Save needs to go outside of the transaction
    // otherwise some DBs will violate foreign key
    // constraints
    if (this.isDirty())
      await this.save(options);

    return result;
  },
  'exists': async function({ field, type }, options, ...args) {
    let query = await type.prepareQuery({ connection: null, self: this, field, options }, args);
    return await query.exists(options);
  },
  'pluck': async function({ field, type }, fields, options, ...args) {
    let query = await type.prepareQuery({ connection: null, self: this, field, options }, args);
    return await query.pluck(fields, options);
  },
};

class ModelType extends RelationalTypeBase {
  isManyRelation() {
    return false;
  }

  fieldNameToOperationName(field, operation, rootMethod) {
    if (rootMethod)
      return `_${operation}${Nife.capitalize(field.fieldName)}`;
    else
      return `${operation}${Nife.capitalize(field.fieldName)}`;
  }

  initialize(connection, self) {
    return super.initialize(connection, self, TYPE_OPERATIONS);
  }
}

module.exports = {
  Model: RelationalTypeBase.wrapConstructor(ModelType),
  ModelType,
};
