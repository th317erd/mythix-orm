'use strict';

const Nife                = require('nife');
const Inflection          = require('inflection');
const RelationalTypeBase  = require('./relational-type-base');
const ModelUtils          = require('../../utils/model-utils');

const NAMED_METHOD  = false;
const ROOT_METHOD   = true;

const INJECT_TYPE_METHODS = {
  'addTo': async function({ field, type }) {

  },
  'get': async function({ field, type }, queryEngine) {

  },
  'set': async function({ field, type }) {

  },
  'update': async function({ field, type }) {

  },
  'removeFrom': async function({ field, type }) {

  },
  'destroy': async function({ field, type }) {

  },
  'has': async function({ field, type }) {

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
      return `__${operation}${fieldName}`;
    else
      return `${operation}${fieldName}`;
  }

  onModelInstantiated(modelInstance, field) {
    for (let i = 0, il = INJECT_TYPE_METHODS_KEYS.length; i < il; i++) {
      let operation       = INJECT_TYPE_METHODS_KEYS[i];
      let method          = INJECT_TYPE_METHODS[operation];
      let methodName      = this.fieldNameToOperationName(field, operation, NAMED_METHOD);
      let fullMethodName  = this.fieldNameToOperationName(field, operation, ROOT_METHOD);

      ModelUtils.injectModelMethod(modelInstance, method.bind(modelInstance, { methodName, fullMethodName, field, type: this }), methodName, fullMethodName);
    }
  }
}

module.exports = {
  Models: RelationalTypeBase.wrapConstructor(ModelsType),
  ModelsType,
};
