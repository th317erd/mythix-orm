'use strict';

const Nife            = require('nife');
const SqlString       = require('sqlstring');
const UUID            = require('uuid');
const ConnectionBase  = require('./connection-base');
const SQLLiterals     = require('./sql-literals');
const ModelUtils      = require('../utils/model-utils');
const ModelBase       = require('../model');

const SAVE_POINT_NAME_CHARS = [ 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P' ];

class SQLConnectionBase extends ConnectionBase {
  static Literals = SQLLiterals;

  static getSQLLiteralClassByName(_name) {
    if (!_name)
      return;

    let name = Nife.capitalize(_name.toLowerCase());

    if (name === 'Literal')
      return SQLLiterals.SQLLiteral;
    else if (name === 'Base')
      return SQLLiterals.SQLLiteralBase;

    return SQLLiterals[`${name}SQLLiteral`];
  }

  static Literal(name, ...args) {
    const LiteralClass = this.getSQLLiteralClassByName(name);
    if (!LiteralClass)
      throw new Error(`${this.constructor.name}::Literal: Unable to locate literal class for literal name "${name}".`);

    let literal = new LiteralClass(...args);
    return literal;
  }

  constructor(_options) {
    super(_options);

    Object.defineProperties(this, {
      'queryGenerator': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
    });
  }

  getQueryGenerator() {
    return this.queryGenerator;
  }

  setQueryGenerator(queryGenerator) {
    this.queryGenerator = queryGenerator;
  }

  prepareArrayValuesForSQL(_array) {
    let array = Nife.arrayFlatten(_array);

    array = array.filter((item) => {
      if (item === null)
        return true;

      if (item instanceof SQLLiterals.SQLLiteralBase)
        return true;

      if (!Nife.instanceOf(item, 'string', 'number', 'bigint', 'boolean'))
        return false;

      return true;
    });

    return Nife.uniq(array);
  }

  escapeSpecialValue(field, value) {
    if (value instanceof SQLLiterals.SQLLiteralBase)
      return value.toString(this);

    if (value === true) {
      return 'TRUE';
    } else if (value === false) {
      return 'FALSE';
    } else if (typeof value === 'bigint') {
      return value.toString();
    } else if (Array.isArray(value)) {
      let arrayValue = this.prepareArrayValuesForSQL(value);
      if (Nife.isEmpty(arrayValue))
        return '';

      return `(${arrayValue.map((item) => this.escape(field, item)).join(',')})`;
    }
  }

  escape(field, value) {
    let result = this.escapeSpecialValue(field, value);
    if (result !== undefined)
      return result;

    return SqlString.escape(value);
  }

  escapeID(value) {
    if (value instanceof SQLLiterals.SQLLiteralBase)
      return value.toString(this.connection);

    return SqlString.escapeId(value).replace(/`/g, '"');
  }

  generateSavePointName() {
    let id = UUID.v4();

    id = id.toUpperCase().replace(/\d/g, (m) => {
      let index = parseInt(m, 10);
      return SAVE_POINT_NAME_CHARS[index];
    }).replace(/-/g, '');

    return `SP${id}`;
  }

  findAllFieldsFromFieldProjectionMap(projectionFieldMap) {
    let fullFieldNames = (Array.isArray(projectionFieldMap)) ? projectionFieldMap : Array.from(projectionFieldMap.keys());

    return fullFieldNames.map((fullFieldName) => {
      let def = ModelUtils.parseQualifiedName(fullFieldName);
      if (!def.modelName || def.fieldNames.length === 0)
        return fullFieldName;

      let field = this.getField(def.fieldNames[0], def.modelName);
      if (!field)
        return fullFieldName;

      return field;
    }).filter(Boolean);
  }

  buildModelDataMapFromSelectResults(queryEngine, result) {
    if (!result)
      return {};

    let rows = result.rows;
    if (Nife.isEmpty(rows))
      return {};

    let context         = queryEngine._getRawQueryContext();
    let fields          = this.findAllFieldsFromFieldProjectionMap(result.columns);
    let rootModelName   = context.rootModelName;
    let modelData       = {};
    let alreadyVisited  = {};

    let fieldInfo = fields.map((field) => {
      let Model       = field.Model;
      let modelName   = Model.getModelName();
      let pkFieldName = Model.getPrimaryKeyFieldName();

      return {
        pkFieldName,
        field,
        Model,
        modelName,
      };
    });

    let modelInfo = fieldInfo.reduce((obj, info) => {
      obj[info.modelName] = info;
      return obj;
    }, {});

    for (let i = 0, il = rows.length; i < il; i++) {
      let row   = rows[i];
      let data  = {};

      // Collect row
      for (let j = 0, jl = fieldInfo.length; j < jl; j++) {
        let {
          field,
          modelName,
        } = fieldInfo[j];

        let dataContext = data[modelName];
        let remoteValue = row[j];

        if (!dataContext)
          dataContext = data[modelName] = {};

        dataContext[field.fieldName] = remoteValue;
      }

      // Remap row
      let modelNames = Object.keys(data).sort((a, b) => {
        if (a === rootModelName)
          return -1;

        if (b === rootModelName)
          return 1;

        if (a === b)
          return 0;

        return (a < b) ? -1 : 1;
      });

      let rootModelData;
      for (let i = 0, il = modelNames.length; i < il; i++) {
        let modelName     = modelNames[i];
        let info          = modelInfo[modelName];
        let models        = modelData[modelName];
        let model         = data[modelName];
        let pkFieldName   = info.pkFieldName;
        let index;

        if (!models)
          models = modelData[modelName] = [];

        // TODO: Use model projection to build relationships
        // This should work if it just happens at the projection
        // level, as that is what will project the fields, and
        // collect the models for these operations.

        if (pkFieldName) {
          let id = model[pkFieldName];

          if (id != null) {
            let idKey = `${modelName}:${pkFieldName}:${id}`;

            if (alreadyVisited[idKey] != null) {
              index = alreadyVisited[idKey];
            } else {
              index = alreadyVisited[idKey] = models.length;
              models.push(model);
            }
          }
        } else {
          models.push(model);
          continue;
        }

        if (i === 0) {
          rootModelData = model;
        } else {
          if (!rootModelData._) {
            Object.defineProperties(rootModelData, {
              '_': {
                writable:     true,
                enumberable:  false,
                configurable: true,
                value:        {},
              },
            });
          }

          if (!rootModelData._[modelName])
            rootModelData._[modelName] = [];

          rootModelData._[modelName].push(index);
        }
      }
    }

    return modelData;
  }

  buildModelsFromModelDataMap(queryEngine, modelDataMap) {
    if (Nife.isEmpty(modelDataMap))
      return [];

    let queryContext  = queryEngine._getRawQueryContext();
    let rootModelName = queryContext.rootModelName;
    let RootModel     = queryContext.rootModel;
    if (!rootModelName || !RootModel)
      throw new Error(`${this.constructor.name}::buildModelsFromModelDataMap: Root model not found.`);

    let rootModelData = modelDataMap[rootModelName];
    if (Nife.isEmpty(rootModelData))
      return [];

    let rootModels = rootModelData.map((data) => {
      let model = new RootModel(data);

      if (model._) {
        let relationships = data._;
        let modelNames    = Object.keys(relationships);

        for (let i = 0, il = modelNames.length; i < il; i++) {
          let modelName           = modelNames[i];
          let Model               = this.getModel(modelName);
          let pluralModelName     = Model.getPluralName();
          let relationName        = Nife.uncapitalize(pluralModelName);
          let relationshipModels  = model._[relationName];
          let modelIndexes        = relationships[modelName];
          let models              = modelDataMap[modelName];

          if (!relationshipModels)
            relationshipModels = model._[relationName] = [];

          model._[relationName] = relationshipModels.concat(modelIndexes.map((modelIndex) => {
            let modelData = models[modelIndex];
            return new Model(modelData);
          }));
        }
      }

      model.clearDirty();

      return model;
    });

    return rootModels;
  }
}

module.exports = SQLConnectionBase;
