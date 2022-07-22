'use strict';

const Nife        = require('nife');
const Type        = require('../type');
const ModelUtils  = require('../../utils/model-utils');

class ForeignKeyType extends Type {
  static isForeignKey() {
    return true;
  }

  constructor(_fullyQualifiedName, _options) {
    if (arguments.length === 0)
      throw new TypeError('ForeignKeyType::constructor: You must specify a fully qualified field name, or provide complete options.');

    let fullyQualifiedName  = _fullyQualifiedName;
    let options             = _options || {};

    if (arguments.length === 1) {
      if (Nife.instanceOf(fullyQualifiedName, 'object'))
        options = fullyQualifiedName;
    } else {
      let def = ModelUtils.parseQualifiedName(fullyQualifiedName);
      if (Nife.isEmpty(def.modelName))
        throw new TypeError('ForeignKeyType::constructor: No model found. You must specify a model.');

      options = Object.assign(
        {},
        options,
        {
          modelName: def.modelName,
          fieldName: def.fieldNames[0],
          fullyQualifiedName,
        },
      );
    }

    super(fullyQualifiedName, options);

    this.options = options;
    this.fullyQualifiedName = fullyQualifiedName;
  }

  castToType(args) {
    let { value, typeInstance } = args;
    if (!typeInstance && value == null)
      return value;

    let targetField = typeInstance.getTargetField();
    if (!targetField)
      return value;

    return targetField.type.castToType(args);
  }

  parseOptionsAndCheckForErrors(SourceModel, sourceField, type, connection) {
    let options             = this.options;
    let fullyQualifiedName  = this.fullyQualifiedName;
    let Model               = options.Model;
    let Field               = options.Field;
    let fieldName           = options.fieldName;

    if (!Model) {
      if (options.modelName) {
        Model = connection.getModel(options.modelName);
      } else {
        let def = ModelUtils.parseQualifiedName(fullyQualifiedName);
        Model = connection.getModel(def.modelName);

        if (!fieldName)
          fieldName = def.fieldNames[0];
      }
    }

    if (!Model)
      throw new TypeError('ForeignKeyType::parseOptionsAndCheckForErrors: No model found. You must specify a model.');

    if (!Field) {
      let modelName = Model.getModelName();

      if (options.fieldName) {
        Field = connection.getField(options.fieldName, modelName);
      } else {
        let def = ModelUtils.parseQualifiedName(fullyQualifiedName);
        Field = connection.getField(def.fieldNames[0], modelName);
      }
    }

    if (!Field)
      throw new TypeError('ForeignKeyType::parseOptionsAndCheckForErrors: No field found. You must specify a field.');

    return {
      Model,
      Field,
    };
  }

  onModelInitialize(SourceModel, sourceField, type, connection) {
    super.onModelInitialize();

    let {
      Model,
      Field,
    } = this.parseOptionsAndCheckForErrors(SourceModel, sourceField, type, connection);

    Object.defineProperties(this, {
      'targetModel': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        Model,
      },
      'targetField': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        Field,
      },
    });
  }

  getOptions() {
    return this.options;
  }

  getTargetModel() {
    return this.targetModel;
  }

  getTargetModelName() {
    let targetModel = this.getTargetModel();
    return targetModel.getModelName();
  }

  getTargetField() {
    return this.targetField;
  }

  toConnectionType(...args) {
    let targetField = this.getTargetField();
    if (!targetField)
      return '';

    return targetField.type.toConnectionType(...args);
  }

  toString(...args) {
    if (args.length === 0)
      return 'ForeignKeyType {}';

    let targetField = this.getTargetField();
    if (!targetField)
      return '';

    return targetField.type.toConnectionType(...args);
  }
}

module.exports = {
  FOREIGN_KEY: Type.wrapConstructor(ForeignKeyType),
  ForeignKeyType,
};
