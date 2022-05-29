'use strict';

const Nife        = require('nife');
const Type        = require('../type');
const ModelUtils  = require('../../utils/model-utils');

const MAX_RECURSE_DEPTH = 14;

class RelationalTypeBase extends Type {
  static isVirtual() {
    return true;
  }

  // Model types work by specifying a "target"
  // and a "value provider".
  // These are fully qualified names, meaning
  // they also point to the model as well as the field.
  // If no model is specified, then it always defaults to
  // "this" model. If no field is specified, then it always
  // defaults to "this PK" of the model.
  // Mythix ORM will recursively walk all models and fields
  // defined until it has the full relationships between
  // all fields.
  constructor(...args) {
    super(...args);

    let [
      targetRelation,
      sourceRelation,
    ] = args;

    Object.defineProperties(this, {
      'targetRelation': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        targetRelation,
      },
      'sourceRelation': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        sourceRelation,
      },
    });
  }

  isRelational() {
    return true;
  }

  getTargetModel(connection) {
    if (!connection)
      throw new TypeError(`${this.constructor.name}::getTargetModel: Must have a valid "connection" to get the requested model.`);

    let def = this.getFullyQualifiedName(this.getTargetRelation());
    return connection.getModel(def.modelName);
  }

  getTargetRelation() {
    return this.targetRelation;
  }

  getSourceRelation() {
    return this.sourceRelation;
  }

  getFullyQualifiedName(qualifiedName) {
    let def = (Nife.isEmpty(qualifiedName)) ? {} : ModelUtils.parseQualifiedName(qualifiedName);
    if (!def.modelName)
      def.modelName = this.getModel().getModelName();

    if (!def.modelName)
      console.log('Failed getting model name: ', qualifiedName);

    if (Nife.isEmpty(def.fieldNames)) {
      let pkName = this.getModel().getPrimaryKeyFieldName();
      if (Nife.isEmpty(pkName))
        throw new Error(`${this.constructor.name}::getFullyQualifiedName: Error while attempting to parse qualified name "${qualifiedName}" on field "${this.getModel().getModelName()}.${this.getField().fieldName}": No field was specified, and no primary key was found for the model. Without a primary key on the model you MUST specify a field name.`);

      def.fieldNames = [ pkName ];
    }

    return def;
  }

  _getModelRelations(connection, _relations) {
    let relations = _relations || [];
    if (relations.length > MAX_RECURSE_DEPTH)
      throw new Error(`${this.constructor.name}::_getModelRelations: Depth limit exceeded chasing virtual fields.`);

    let fieldRelations = [
      this.getFullyQualifiedName(this.getTargetRelation()),
      this.getFullyQualifiedName(this.getSourceRelation()),
    ];

    for (let j = 0, jl = fieldRelations.length; j < jl; j++) {
      let def         = fieldRelations[j];
      let fieldNames  = def.fieldNames;
      let modelName   = def.modelName;

      for (let i = 0, il = fieldNames.length; i < il; i++) {
        let fieldName = fieldNames[i];
        let field     = connection.getField(fieldName, modelName);
        if (!field)
          throw new Error(`${this.constructor.name}::_getModelRelations: Attempted to fetch "${modelName}.${fieldName}" and failed.`);

        let fieldType = field.type;
        if (fieldType.isRelational()) {
          modelName = fieldType._getModelRelations(
            connection,
            relations,
          );
        } else if (fieldType.isVirtual()) {
          throw new Error(`${this.constructor.name}::_getModelRelations: Encountered an unexpected virtual field "${modelName}.${fieldName}".`);
        } else {
          let sourceModelName = fieldRelations[1].modelName;
          let sourceFieldName = fieldRelations[1].fieldNames[0];
          if (sourceFieldName === fieldName && sourceModelName === modelName)
            continue;

          let sourceField = connection.getField(sourceFieldName, sourceModelName);
          if (!sourceField)
            throw new Error(`${this.constructor.name}::_getModelRelations: Attempted to fetch "${sourceModelName}.${sourceFieldName}" and failed.`);

          let sourceFieldType = sourceField.type;
          if (sourceFieldType.isRelational() || sourceFieldType.isVirtual())
            continue;

          relations.push({
            relationType:     (j === 1) ? 'source' : 'target',
            fieldIndex:       i,
            sourceModelName:  sourceModelName,
            sourceFieldName:  sourceFieldName,
            targetModelName:  modelName,
            targetFieldName:  fieldName,
          });

          break;
        }
      }
    }

    return fieldRelations[0].modelName;
  }

  removeDuplicatesFromRelations(relations) {
    const removeDuplicates = (relations) => {
      let map = {};

      for (let i = 0, il = relations.length; i < il; i++) {
        let part = relations[i];
        let key = `${part.targetModelName}.${part.sourceModelName}.${part.targetFieldName}.${part.sourceFieldName}.${part.relationType}.${part.fieldIndex}`;

        map[key] = part;
      }

      return Array.from(Object.values(map));
    };

    return removeDuplicates(relations);
  }

  getJoinableRelations(connection) {
    if (!connection)
      throw new TypeError(`${this.constructor.name}::getJoinableRelations: Must have a valid "connection" to get field relations.`);

    let relations = [];

    this._getModelRelations(
      connection,
      relations,
    );

    return this.removeDuplicatesFromRelations(relations);
  }

  toString() {
    return '';
  }
}

module.exports = RelationalTypeBase;
