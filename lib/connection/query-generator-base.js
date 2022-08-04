'use strict';

const Nife        = require('nife');
const QueryEngine = require('../query-engine/query-engine');
const LiteralBase = require('./literals/literal-base');
const ModelBase   = require('../model');

class QueryGeneratorBase {
  constructor(connection) {
    Object.defineProperties(this, {
      'connection': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        connection,
      },
    });
  }

  getOptionsCache(options, keyPath, initialValue) {
    return Nife.get(options, `_cache.${keyPath}`, (typeof initialValue === 'function') ? initialValue() : initialValue);
  }

  setOptionsCache(options, keyPath, value) {
    Nife.set(options, `_cache.${keyPath}`, value);
  }

  escape(...args) {
    return this.connection.escape(...args);
  }

  escapeID(...args) {
    return this.connection.escapeID(...args);
  }

  getTableNameFromQueryPart(queryPart) {
    let Model = queryPart.Model;
    return Model.getTableName();
  }

  getEscapedFieldName(_Model, field, options) {
    let isString  = Nife.instanceOf(field, 'string');
    let fieldName = (isString) ? field : field.fieldName;
    let Model     = _Model;

    if (!Model && field && !isString)
      Model = field.Model;

    if (!Model || (options && options.fieldNameOnly === true))
      return this.escapeID(fieldName);
    else
      return `"${field.Model.getModelName()}:${fieldName}"`;
  }

  getEscapedColumnName(_Model, field, options) {
    let isString    = Nife.instanceOf(field, 'string');
    let columnName  = (isString) ? field : (field.columnName || field.fieldName);
    let Model       = _Model;

    if (!Model && field && !isString)
      Model = field.Model;

    if (!Model || (options && options.columnNameOnly === true))
      return this.escapeID(columnName);
    else
      return `${this.escapeID(Model.getTableName())}.${this.escapeID(columnName)}`;
  }

  // eslint-disable-next-line no-unused-vars
  getEscapedProjectionName(Model, field, options) {
    if (options && options.noProjectionAliases)
      return this.getEscapedColumnName(Model, field, options);
    else
      return `${this.getEscapedColumnName(Model, field, options)} AS ${this.getEscapedFieldName(Model, field, options)}`;
  }

  // eslint-disable-next-line no-unused-vars
  getEscapedModelFields(Model, options) {
    let fields    = {};
    let modelName = Model.getModelName();

    Model.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      let result;

      if (options && options.asProjection)
        result = this.getEscapedProjectionName(Model, field, options);
      else if (options && options.asColumn)
        result = this.getEscapedColumnName(Model, field, options);
      else
        result = this.getEscapedFieldName(Model, field, options);

      fields[`${modelName}:${fieldName}`] = result;
    }, (options && options.fields));

    return fields;
  }

  // eslint-disable-next-line no-unused-vars
  getAllModelsUsedInQuery(queryEngine, _options) {
    let options               = _options || {};
    let queryEngineContextID  = queryEngine._getTopContextID();
    let cache                 = this.getOptionsCache(_options, `getAllModelsUsedInQuery.${queryEngineContextID}`);
    if (cache)
      return cache;

    let Models  = new Map();
    let query   = queryEngine._getRawQuery();

    for (let i = 0, il = query.length; i < il; i++) {
      let queryPart = query[i];

      if (Object.prototype.hasOwnProperty.call(queryPart, 'operator') && queryPart.operator === 'MODEL') {
        let Model = queryPart.Model;
        Models.set(Model, Model);
      } else if (Object.prototype.hasOwnProperty.call(queryPart, 'condition') && queryPart.condition === true) {
        let operatorValue = queryPart.value;
        if (!QueryEngine.isQuery(operatorValue))
          continue;

        let SubModels = this.getAllModelsUsedInQuery(operatorValue, options);
        for (let j = 0, jl = SubModels.length; j < jl; j++) {
          let Model = SubModels[j];
          Models.set(Model, Model);
        }
      }
    }

    let allModels = Array.from(Models.values());

    if (_options)
      this.setOptionsCache(_options, `getAllModelsUsedInQuery.${queryEngineContextID}`, allModels);

    return allModels;
  }

  getProjectionRequiredFields(queryEngine, options) {
    let { order } = this.getOrderLimitOffset(queryEngine, options);
    if (!order)
      return new Map();

    let orderFieldMap = new Map();
    for (let i = 0, il = order.length; i < il; i++) {
      let field = order[i];
      if (!field)
        continue;

      if (field instanceof LiteralBase) {
        let result          = field.toString(this.connection);
        let projectionField = this.parseFieldProjection(result, true);
        if (projectionField === result) {
          // not able to parse projection
          continue;
        }

        field = projectionField;
      }

      let Field         = field.Field || field;
      let Model         = Field.Model;
      let modelName     = Model.getModelName();
      let fullFieldName = `${modelName}:${Field.fieldName}`;

      orderFieldMap.set(fullFieldName, {
        projectedName:  this.getEscapedProjectionName(Model, Field, options),
        Model:          Model,
        modelName:      modelName,
        Field:          Field,
        fieldName:      Field.fieldName,
        direction:      '+',
        fullFieldName,
      });
    }

    return orderFieldMap;
  }

  getProjectionFromQueryEngine(queryEngine, options) {
    let queryEngineContextID  = queryEngine._getTopContextID();
    let cache                 = this.getOptionsCache(options, `getProjectionFromQueryEngine.${queryEngineContextID}`);
    if (cache)
      return cache;

    const shouldResetProjection = (fields) => {
      if (fields.length === 0)
        return false;

      if (fields.indexOf('+') >= 0)
        return false;

      for (let i = 0, il = fields.length; i < il; i++) {
        let field = fields[i];

        if (field instanceof LiteralBase)
          return true;

        if (ModelBase.isModelClass(field))
          return true;

        if (!Nife.instanceOf(field, 'string'))
          true;

        if (field === '-')
          return true;

        if (field === '*')
          continue;

        if (!(/^\s*[+-]/).test(field))
          return true;
      }

      return false;
    };

    const collectProjectionValuesFromQuery = (RootModel, query) => {
      let projections = [ RootModel ];

      for (let i = 0, il = query.length; i < il; i++) {
        let queryPart = query[i];

        if (!Object.prototype.hasOwnProperty.call(queryPart, 'control'))
          continue;

        if (queryPart.control !== true)
          continue;

        if (queryPart.operator !== 'PROJECT')
          continue;

        let fields = queryPart.value;
        if (shouldResetProjection(fields)) {
          fields = Nife.arrayFlatten(fields);

          let addIndex        = fields.indexOf('+');
          let newProjections  = fields.slice();

          if (addIndex >= 0)
            newProjections[addIndex] = Nife.arrayFlatten(projections);

          projections = newProjections;
        } else {
          projections = projections.concat(fields);
        }
      }

      return Nife.uniq(Nife.arrayFlatten(projections));
    };

    const modelFieldsToProjection = (fieldSet, Model, removeFromProjection) => {
      let modelName = Model.getModelName();

      Model.iterateFields(({ field, fieldName }) => {
        if (field.type.isVirtual())
          return;

        let fullFieldName = `${modelName}:${fieldName}`;

        if (removeFromProjection) {
          fieldSet.delete(fullFieldName);
          return;
        }

        fieldSet.set(fullFieldName, {
          projectedName:  this.getEscapedProjectionName(Model, field, options),
          Model:          Model,
          Field:          field,
          direction:      '+',
          fullFieldName,
          fieldName,
          modelName,
        });
      });
    };

    const addRequiredFieldsToProjection = (fieldSet) => {
      let requiredProjectionFields = this.getProjectionRequiredFields(queryEngine, options);

      for (let [ key, value ] of requiredProjectionFields) {
        if (fieldSet.has(key))
          continue;

        fieldSet.set(key, value);
      }

      return fieldSet;
    };

    let RootModel = queryEngine._getRawQueryContext().rootModel;
    if (!RootModel)
      throw new Error('QueryGeneratorBase::getProjectionFromQueryEngine: No root model found for query. Root model is required to generate a projection.');

    let projections     = collectProjectionValuesFromQuery(RootModel, queryEngine._getRawQuery());
    let projectedFields = new Map();
    let allModels       = this.getAllModelsUsedInQuery(queryEngine, options);
    let isAdding        = true;

    // If projection is empty, then return
    // the projection of the root model
    if (Nife.isEmpty(projections)) {
      modelFieldsToProjection(projectedFields, RootModel);
      return Array.from(addRequiredFieldsToProjection(projectedFields).values());
    }

    for (let i = 0, il = projections.length; i < il; i++) {
      let projectionValue = projections[i];
      if (!projectionValue)
        continue;

      if (projectionValue === '+') {
        isAdding = true;
        continue;
      }

      if (projectionValue === '-') {
        isAdding = false;
        continue;
      }

      if (projectionValue === '*') {
        for (let i = 0, il = allModels.length; i < il; i++) {
          let Model = allModels[i];
          modelFieldsToProjection(projectedFields, Model);
        }

        continue;
      }

      if (projectionValue instanceof LiteralBase) {
        let key = projectionValue.toString(this.connection);
        if (isAdding)
          projectedFields.set(key, projectionValue);
        else
          projectedFields.delete(key);

        continue;
      }

      if (ModelBase.isModelClass(projectionValue)) {
        if (allModels.indexOf(projectionValue) >= 0)
          modelFieldsToProjection(projectedFields, projectionValue, !isAdding);

        continue;
      }

      let { fieldName, direction, hasDirection } = this.getFieldDirectionSpecifier(projectionValue);
      if (!fieldName)
        continue;

      if (!hasDirection) {
        direction = (isAdding) ? '+' : '-';
        hasDirection = true;
      }

      let def = this.connection.parseQualifiedName(fieldName);
      if (!def.modelName) {
        if (Nife.isNotEmpty(def.fieldNames) && allModels.length > 1)
          throw new Error(`QueryGeneratorBase::getProjectionFromQueryEngine: "${def.fieldNames[0]}" ambiguous. You must use a fully qualified field name for an ORDER clause. Example: "+Model:id".`);

        def.modelName = RootModel.getModelName();
      }

      let ProjectionModel = this.connection.getModel(def.modelName);
      if (!ProjectionModel) {
        if (hasDirection && direction === '-') {
          projectedFields.delete(projectionValue);
          continue;
        }

        // Proceed blindly... as the user may be
        // querying something we are unaware of
        projectedFields.set(projectionValue, projectionValue);
        continue;
      }

      if (allModels.indexOf(ProjectionModel) < 0)
        continue;

      if (!def.fieldNames.length) {
        // If there are no field names, but we have a model name
        // then let the projection generator generate all model
        // fields

        if (ProjectionModel) {
          modelFieldsToProjection(projectedFields, ProjectionModel, (hasDirection && direction === '-'));
          continue;
        }
      }

      let field     = this.connection.getField(def.fieldNames[0], def.modelName);
      let modelName = (field) ? field.Model.getModelName() : ProjectionModel.getModelName();

      if (!field) {
        let projectionFieldName = def.fieldNames[0];
        let fullFieldName       = `${modelName}:${projectionFieldName}`;

        if (hasDirection && direction === '-') {
          projectedFields.delete(fullFieldName);
          continue;
        }

        // Proceed blindly... as the user may be
        // querying something we are unaware of
        projectedFields.set(fullFieldName, {
          projectedName:  this.getEscapedProjectionName(ProjectionModel, fieldName, options),
          Model:          ProjectionModel,
          Field:          null,
          fieldName:      projectionFieldName,
          direction:      '+',
          modelName,
          fullFieldName,
        });

        continue;
      }

      let projectionFieldName = field.fieldName;
      let fullFieldName       = `${modelName}:${projectionFieldName}`;

      if (hasDirection && direction === '-') {
        projectedFields.delete(fullFieldName);
        continue;
      }

      projectedFields.set(fullFieldName, {
        projectedName:  this.getEscapedProjectionName(field.Model, field, options),
        Model:          field.Model,
        Field:          field,
        fieldName:      field.fieldName,
        fullFieldName,
        modelName,
        direction,
      });
    }

    let result = Array.from(addRequiredFieldsToProjection(projectedFields).values());

    if (options)
      this.setOptionsCache(options, `getProjectionFromQueryEngine.${queryEngineContextID}`, result);

    return result;
  }

  sortFieldProjectionMap(fieldProjectionMap) {
    let keys = Array.from(fieldProjectionMap.keys()).sort((a, b) => {
      var x = fieldProjectionMap.get(a);
      var y = fieldProjectionMap.get(b);

      if ((typeof x === 'string' && typeof y === 'string') || (x instanceof LiteralBase && y instanceof LiteralBase)) {
        if (a === b)
          return 0;

        return (a < b) ? -1 : 1;
      }

      if (typeof x === 'string')
        return -1;

      if (typeof y === 'string')
        return 1;

      return 0;
    });

    let newMap = new Map();

    for (let i = 0, il = keys.length; i < il; i++) {
      let key = keys[i];
      newMap.set(key, fieldProjectionMap.get(key));
    }

    return newMap;
  }

  getProjectedFields(queryEngine, _options, asMap) {
    let queryProjection     = this.getProjectionFromQueryEngine(queryEngine, _options);
    let allProjectionFields = new Map();

    for (let i = 0, il = queryProjection.length; i < il; i++) {
      let projectionField = queryProjection[i];
      if (!projectionField)
        continue;

      if (projectionField instanceof LiteralBase) {
        let result = projectionField.toString(this.connection);
        let fullFieldName = this.parseFieldProjection(result);
        if (!fullFieldName)
          fullFieldName = result;

        if (fullFieldName && result)
          allProjectionFields.set(fullFieldName, result);

        if (result && !this.isFieldProjection(result))
          allProjectionFields.set(result, result);

        continue;
      }

      if (projectionField.direction === '-')
        continue;

      if (Nife.instanceOf(projectionField, 'string')) {
        allProjectionFields.set(projectionField, projectionField);
        continue;
      }

      let projectedName = projectionField.projectedName;
      allProjectionFields.set(projectionField.fullFieldName, projectedName);
    }

    allProjectionFields = this.sortFieldProjectionMap(allProjectionFields);

    if (asMap === true)
      return allProjectionFields;
    else
      return Array.from(allProjectionFields.values());
  }

  // eslint-disable-next-line no-unused-vars
  getJoinTableInfoFromQueryContexts(leftQueryContext, rightSideQueryEngine, joinType, options) {
    const findFirstField = (query) => {
      for (let i = 0, il = query.length; i < il; i++) {
        let queryPart = query[i];
        if (!Object.prototype.hasOwnProperty.call(queryPart, 'operator'))
          continue;

        if (queryPart.operator === 'FIELD' && Nife.isNotEmpty(queryPart.fieldName))
          return i;
      }

      return -1;
    };

    let rootModel     = leftQueryContext.rootModel;
    let rootModelName = rootModel.getModelName();
    let leftSideModel = leftQueryContext.Model;
    if (!leftSideModel)
      throw new Error(`${this.constructor.name}::getJoinTableInfoFromQueryEngine: Invalid operation: No model found for left-side of join statement.`);

    let leftSideField = this.connection.getField(leftQueryContext.fieldName, leftQueryContext.modelName);
    if (!leftSideField)
      throw new Error(`${this.constructor.name}::getJoinTableInfoFromQueryEngine: Invalid operation: No left-side field found to match on for table join statement.`);

    let rightQueryContext = rightSideQueryEngine._getRawQuery();
    let isNot             = leftQueryContext.not;
    let operator          = (isNot) ? leftQueryContext.inverseOperator : leftQueryContext.operator;
    let firstFieldIndex   = findFirstField(rightQueryContext);

    if (firstFieldIndex < 0)
      throw new Error(`${this.constructor.name}::getJoinTableInfoFromQueryEngine: Invalid operation: No right-side field found to match on for table join statement.`);

    let nextContext = rightQueryContext[firstFieldIndex + 1];
    if (nextContext && nextContext.condition === true)
      throw new Error(`${this.constructor.name}::getJoinTableInfoFromQueryEngine: Invalid operation: Expected a field to join on, but instead received a query.`);

    rightQueryContext = rightQueryContext[firstFieldIndex];

    let rightSideModel = rightQueryContext.Model;
    if (!rightSideModel)
      throw new Error(`${this.constructor.name}::getJoinTableInfoFromQueryEngine: Invalid operation: No model found for right-side of join statement.`);

    let rightSideField  = this.connection.getField(rightQueryContext.fieldName, rightQueryContext.modelName);
    if (!rightSideField)
      throw new Error(`${this.constructor.name}::getJoinTableInfoFromQueryEngine: Invalid operation: No right-side field found to match on for table join statement.`);

    let leftSideModelName   = leftSideModel.getModelName();
    let rightSideModelName  = rightSideModel.getModelName();
    let modelOrderSwapped   = (rootModelName === rightSideModelName);
    let joinModel           = (modelOrderSwapped) ? leftSideModel : rightSideModel;
    let joinModelName       = (modelOrderSwapped) ? leftSideModelName : rightSideModelName;

    return {
      operator,
      joinType,
      modelOrderSwapped,
      rootModelName,

      joinModel,
      joinModelName,

      leftSideModel,
      leftSideModelName,
      leftQueryContext,
      leftSideField,

      rightSideModel,
      rightSideModelName,
      rightQueryContext,
      rightSideField,
    };
  }

  getFieldDirectionSpecifier(order) {
    if (!order)
      return order;

    if (order instanceof LiteralBase)
      return order;

    // Is this a field?
    if (order && order.Model && order.fieldName) {
      return {
        hasDirection: false,
        direction:    '+',
        fieldName:    order.fieldName,
      };
    }

    let sign;

    let fieldName = ('' + order).replace(/^[+-]+/, (m) => {
      sign = m.charAt(0);
      return '';
    });

    return {
      hasDirection: !!sign,
      direction:    (sign === '-') ? '-' : '+',
      fieldName,
    };
  }

  // eslint-disable-next-line no-unused-vars
  getOrderLimitOffset(queryEngine, options) {
    let queryEngineContextID  = queryEngine._getTopContextID();
    let cache                 = this.getOptionsCache(options, `getOrderLimitOffset.${queryEngineContextID}`);
    if (cache)
      return cache;

    let query     = queryEngine._getRawQuery();
    let rootModel = queryEngine._getRawQueryContext().rootModel;
    let limit;
    let offset;
    let order;

    for (let i = 0, il = query.length; i < il; i++) {
      let queryPart = query[i];

      if (!Object.prototype.hasOwnProperty.call(queryPart, 'control'))
        continue;

      if (queryPart.control !== true)
        continue;

      let queryOperator = queryPart.operator;

      if (queryOperator === 'LIMIT')
        limit = queryPart.value;
      else if (queryOperator === 'OFFSET')
        offset = queryPart.value;
      else if (queryOperator === 'ORDER')
        order = queryPart.value;
    }

    if (Nife.isNotEmpty(order) && !(order instanceof LiteralBase)) {
      let allModels = this.getAllModelsUsedInQuery(queryEngine, options);

      order = order.map((_fieldName) => {
        if (_fieldName instanceof LiteralBase)
          return _fieldName;

        let { fieldName, direction } = this.getFieldDirectionSpecifier(_fieldName);
        if (!fieldName)
          return;

        let def = this.connection.parseQualifiedName(fieldName);
        if (!def.modelName) {
          if (allModels.length > 1)
            throw new Error(`QueryGeneratorBase::getOrderLimitOffset: "${fieldName}" ambiguous. You must use a fully qualified field name for an ORDER clause. Example: "+Model:id".`);

          def.modelName = allModels[0].getModelName();
        }

        if (!def.fieldNames.length)
          throw new Error(`QueryGeneratorBase::getOrderLimitOffset: No field names found for "${fieldName}".`);

        let field = this.connection.getField(def.fieldNames[0], def.modelName);
        if (!field)
          throw new Error(`QueryGeneratorBase::getOrderLimitOffset: Unable to locate field "${def.modelName}"."${def.fieldNames[0]}".`);

        return {
          Model: field.Model,
          Field: field,
          direction,
        };
      });
    } else if (Nife.isEmpty(order)) {
      if (options && options.selectStatement === true)
        order = this.connection.getDefaultOrder(rootModel, options);
    }

    let orderLimitOffset = { limit, order, offset };

    if (options)
      this.setOptionsCache(options, `getOrderLimitOffset.${queryEngineContextID}`, orderLimitOffset);

    return orderLimitOffset;
  }

  queryHasConditions(query) {
    for (let i = 0, il = query.length; i < il; i++) {
      let queryPart = query[i];
      if (!Object.prototype.hasOwnProperty.call(queryPart, 'condition'))
        continue;

      if (queryPart.condition === true)
        return true;
    }

    return false;
  }

  getQuerySliceFromQueryPart(queryPart) {
    let queryRoot = queryPart.queryRoot;
    let index     = queryRoot.indexOf(queryPart);

    return queryRoot.slice(index);
  }

  _averageLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let field = literal.definitionToField(this.connection, literal.definition);
    let escapedFieldName;

    if (field instanceof LiteralBase)
      escapedFieldName = field.toString(this.connection);
    else
      escapedFieldName = this.getEscapedColumnName(field.Model, field, literal.options);

    return `AVG(${escapedFieldName})`;
  }

  _countLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let field = (literal.definition) ? literal.definitionToField(this.connection, literal.definition) : null;
    let escapedFieldName;

    if (field) {
      if (field instanceof LiteralBase)
        escapedFieldName = field.toString(this.connection);
      else
        escapedFieldName = this.getEscapedColumnName(field.Model, field, literal.options);
    } else {
      escapedFieldName = '*';
    }

    return `COUNT(${escapedFieldName})`;
  }

  _distinctLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let field = literal.definitionToField(this.connection, literal.definition);
    if (field instanceof LiteralBase)
      return `DISTINCT ${field.toString(this.connection)}`;

    return `DISTINCT ${this.getEscapedProjectionName(field.Model, field, literal.options)}`;
  }

  _maxLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let field = literal.definitionToField(this.connection, literal.definition);
    let escapedFieldName;

    if (field instanceof LiteralBase)
      escapedFieldName = field.toString(this.connection);
    else
      escapedFieldName = this.getEscapedColumnName(field.Model, field, literal.options);

    return `MAX(${escapedFieldName})`;
  }

  _minLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let field = literal.definitionToField(this.connection, literal.definition);
    let escapedFieldName;

    if (field instanceof LiteralBase)
      escapedFieldName = field.toString(this.connection);
    else
      escapedFieldName = this.getEscapedColumnName(field.Model, field, literal.options);

    return `MIN(${escapedFieldName})`;
  }

  _sumLiteralToString(literal) {
    if (!literal || !(literal instanceof LiteralBase))
      return;

    let field = literal.definitionToField(this.connection, literal.definition);
    let escapedFieldName;

    if (field instanceof LiteralBase)
      escapedFieldName = field.toString(this.connection);
    else
      escapedFieldName = this.getEscapedColumnName(field.Model, field, literal.options);

    return `SUM(${escapedFieldName})`;
  }
}

module.exports = QueryGeneratorBase;