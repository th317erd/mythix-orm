'use strict';

const Nife        = require('nife');
const Type        = require('../type');
const ModelUtils  = require('../../utils/model-utils');

class ForeignKeyType extends Type {
  static isForeignKey() {
    return true;
  }

  static getDisplayName() {
    return 'FOREIGN_KEY';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
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
    let { value } = args;
    if (value == null)
      return value;

    let targetField = this.getTargetField(args.connection);
    if (!targetField) {
      let field         = this.getField();
      let debugFieldStr = '';

      if (field) {
        let Model = field.Model;
        if (Model)
          debugFieldStr = ` "${Model.getModelName()}:${field.fieldName}"`;
      }

      throw new TypeError(`ForeignKeyType::castToType: Target field not defined${debugFieldStr}.`);
    }

    return targetField.type.castToType(args);
  }

  isValidValue(value, options) {
    let targetField = this.getTargetField(options && options.connection);
    if (!targetField)
      throw new TypeError('ForeignKeyType::isValidValue: Target field not defined.');

    return targetField.type.isValidValue(value, options);
  }

  parseOptionsAndCheckForErrors(SourceModel, sourceField, connection) {
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
      throw new TypeError(`ForeignKeyType::parseOptionsAndCheckForErrors: No target model found for field "${SourceModel.getModelName()}:${sourceField.fieldName}". You must specify a model.`);

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
      throw new TypeError(`ForeignKeyType::parseOptionsAndCheckForErrors: No target field found for "${SourceModel.getModelName()}:${sourceField.fieldName}". You must specify a field.`);

    return {
      Model,
      Field,
    };
  }

  initialize(connection, self) {
    if (this.targetModel)
      return;

    super.initialize(connection, self);

    if (!connection)
      throw new Error('ForeignKeyType::initialize: A connection is required to use the ForeignKeyType.');

    let {
      Model,
      Field,
    } = this.parseOptionsAndCheckForErrors(this.getModel(), this.getField(), connection);

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

  getTargetModel(connection) {
    if (connection && !this.targetModel)
      this.initialize(connection);

    return this.targetModel;
  }

  getTargetModelName(connection) {
    let targetModel = this.getTargetModel(connection);
    return targetModel.getModelName();
  }

  getTargetField(connection) {
    if (connection && !this.targetModel)
      this.initialize(connection);

    return this.targetField;
  }

  getTargetFieldName(connection) {
    let targetField = this.getTargetField(connection);
    if (!targetField)
      return;

    return targetField.fieldName;
  }

  toConnectionType(connection, options) {
    let targetField = this.getTargetField(connection);
    if (!targetField)
      return '';

    return targetField.type.toConnectionType(connection, options);
  }

  toString(...args) {
    if (args.length === 0)
      return 'ForeignKeyType {}';

    let targetField = this.getTargetField(...args);
    if (!targetField)
      return '';

    return targetField.type.toConnectionType(...args);
  }
}

module.exports = {
  FOREIGN_KEY: Type.wrapConstructor(ForeignKeyType),
  ForeignKeyType,
};
