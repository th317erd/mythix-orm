'use strict';

const Nife                = require('nife');
const RelationalTypeBase  = require('./relational-type-base');
const ModelUtils          = require('../../utils/model-utils');

const INJECT_TYPE_METHODS = {
  'create': function(field) {

  },
  'get': function(field) {
    /*
      User.getRoles() =

      Role
        .where
        .UserRole.roleID
          .EQ(Role.where.id)
        .AND
        .UserRole.userID
          .EQ(User.where.id)
        .AND
        .User.id
          .EQ(this.id)
    */
  },
  'update': function(field) {

  },
  'destroy': function(field) {

  },
  'exists': function(field) {

  },
};

const INJECT_TYPE_METHODS_KEYS = Object.keys(INJECT_TYPE_METHODS);

class ModelType extends RelationalTypeBase {
  castToType({ value, typeInstance, connection }) {
    if (!typeInstance)
      throw new TypeError('ModelType::castToType: Type instance is required to cast.');

    if (value == null)
      return value;

    let Model = typeInstance.getTargetModel(connection);
    if (!Model)
      throw new TypeError('ModelType::castToType: Failed when attempting to fetch the required model.');

    if (value instanceof Model)
      return value;

    if (!Nife.instanceOf(value, 'object'))
      throw new TypeError('ModelType::castToType: Unable to cast provided value. Value must be a model instance, or a raw object.');

    return new Model(value);
  }

  isManyRelation() {
    return false;
  }

  onModelInstantiated(modelInstance, field) {
    let fieldName = field.fieldName;
    for (let i = 0, il = INJECT_TYPE_METHODS_KEYS.length; i < il; i++) {
      let key             = INJECT_TYPE_METHODS_KEYS[i];
      let method          = INJECT_TYPE_METHODS[key];
      let methodName      = `${key}${Nife.capitalize(fieldName)}`;
      let fullMethodName  = `__${key}${Nife.capitalize(fieldName)}`;

      ModelUtils.injectModelMethod(modelInstance, method.bind(modelInstance, field), methodName, fullMethodName);
    }
  }
}

module.exports = {
  Model: RelationalTypeBase.wrapConstructor(ModelType),
  ModelType,
};
