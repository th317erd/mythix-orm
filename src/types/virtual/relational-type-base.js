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

  getTargetRelation() {
    return this.targetRelation;
  }

  getSourceRelation() {
    return this.sourceRelation;
  }

  getFullyQualifiedName(qualifiedName) {
    let def = ModelUtils.parseQualifiedName(qualifiedName);
    if (!def.modelName)
      def.modelName = this.getModel().getModelName();

    if (Nife.isEmpty(def.fieldNames)) {
      let pkName = this.getModel().getPrimaryKeyFieldName();
      if (Nife.isEmpty(pkName))
        throw new Error(`${this.constructor.name}::getFullyQualifiedName: Error while attempting to parse qualified name "${qualifiedName}" on field "${this.getModel().getModelName()}.${this.getField().fieldName}": No field was specified, and no primary key was found for the model. Without a primary key on the model you MUST specify a field name.`);

      def.fieldNames = [ pkName ];
    } else if (def.fieldNames.length > 1) {
      console.warn(`${this.constructor.name}::getFullyQualifiedName: Warning: Found multiple qualified field names: "${qualifiedName}" = ${JSON.stringify(def.fieldNames)}. Did you use a "." when you should have use a ":"?`);
    }

    return def;
  }

  _getModelPath(connection, relationType, def, _path) {
    let path = _path || [];

    if (path.length > MAX_RECURSE_DEPTH)
      throw new Error(`${this.constructor.name}::_getModelPath: Depth limit exceeded chasing virtual fields.`);

    let field = connection.getField(def.fieldNames[0], def.modelName);
    if (!field)
      throw new Error(`${this.constructor.name}::_getModelPath: Attempted to fetch "${def.modelName}.${def.fieldNames[0]}" and failed.`);

    let fieldType = field.type;
    if (fieldType.isRelational()) {
      let targetRelation = fieldType.getTargetRelation();
      let sourceRelation = fieldType.getSourceRelation();
      let relation;

      if (relationType === 'target') {
        this._getModelPath(
          connection,
          'source',
          fieldType.getFullyQualifiedName(sourceRelation),
          path,
        );

        relation = targetRelation;
      } else {
        this._getModelPath(
          connection,
          'target',
          fieldType.getFullyQualifiedName(targetRelation),
          path,
        );

        relation = sourceRelation;
      }

      return fieldType._getModelPath(
        connection,
        relationType,
        fieldType.getFullyQualifiedName(relation),
        path,
      );
    } else if (field.type.isVirtual()) {
      throw new Error(`${this.constructor.name}::_getModelPath: Encountered an unexpected virtual field "${def.modelName}.${def.fieldNames[0]}".`);
    } else {
      path.push(def);
    }

    return path;
  }

  getTargetRelationPath(connection, _path) {
    if (!connection)
      throw new TypeError(`${this.constructor.name}::getTargetRelationPath: Must have a valid "connection" to get the requested model.`);

    return this._getModelPath(
      connection,
      'target',
      this.getFullyQualifiedName(this.targetRelation),
      _path,
    );
  }

  getSourceRelationPath(connection, _path) {
    if (!connection)
      throw new TypeError(`${this.constructor.name}::getSourceRelationPath: Must have a valid "connection" to get the requested model.`);

    return this._getModelPath(
      connection,
      'source',
      this.getFullyQualifiedName(this.sourceRelation),
      _path,
    );
  }

  getFullRelationPath(connection) {
    if (!connection)
      throw new TypeError(`${this.constructor.name}::getFullRelationPath: Must have a valid "connection" to get the requested model.`);

    let fullPath = [];

    this.getTargetRelationPath(connection, fullPath);
    this.getSourceRelationPath(connection, fullPath);

    return fullPath;
  }

  toString() {
    return '';
  }
}

module.exports = RelationalTypeBase;
