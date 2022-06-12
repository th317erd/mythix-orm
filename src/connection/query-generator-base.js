'use strict';

const Nife            = require('nife');
const { FLAG_LITERAL } = require('../helpers/default-helpers');
const QueryEngine     = require('../query-engine/query-engine');
const SQLLiteralBase  = require('./sql-literals/sql-literal-base.js');
const ModelUtils      = require('../utils/model-utils');
const ModelBase       = require('../model');

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

  getOptionsCache(options, keyPath, defaultValue) {
    return Nife.get(options, `_cache.${keyPath}`, (typeof defaultValue === 'function') ? defaultValue() : defaultValue);
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

  prepareArrayValuesForSQL(...args) {
    return this.connection.prepareArrayValuesForSQL(...args);
  }

  getTableNameFromQueryPart(queryPart) {
    let Model = queryPart.Model;
    return Model.getTableName();
  }

  getEscapedFieldName(field, options) {
    if (options && options.fieldNameOnly === true)
      return this.escapeID(field.fieldName);
    else
      return `"${field.Model.getModelName()}:${field.fieldName}"`;
  }

  getEscapedColumnName(field, options) {
    if (options && options.columnNameOnly === true)
      return this.escapeID(field.columnName);
    else
      return `${this.escapeID(field.Model.getTableName())}.${this.escapeID(field.columnName)}`;
  }

  // eslint-disable-next-line no-unused-vars
  getEscapedProjectionName(field, options) {
    return `${this.getEscapedColumnName(field, options)} AS ${this.getEscapedFieldName(field, options)}`;
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
        result = this.getEscapedProjectionName(field, options);
      else if (options && options.asColumn)
        result = this.getEscapedColumnName(field, options);
      else
        result = this.getEscapedFieldName(field, options);

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
      return {};

    return order.reduce((obj, order) => {
      if (!order)
        return obj;

      if (order instanceof SQLLiteralBase)
        return obj;

      obj[`${order.Model.getModelName()}:${order.Field.fieldName}`] = this.getEscapedProjectionName(order.Field, options);

      return obj;
    }, {});
  }

  getProjectionFromQueryEngine(queryEngine, options) {
    let queryEngineContextID  = queryEngine._getTopContextID();
    let cache                 = this.getOptionsCache(options, `getProjectionFromQueryEngine.${queryEngineContextID}`);
    if (cache)
      return cache;

    let query       = queryEngine._getRawQuery();
    let projections = [];

    for (let i = 0, il = query.length; i < il; i++) {
      let queryPart = query[i];

      if (!Object.prototype.hasOwnProperty.call(queryPart, 'control'))
        continue;

      if (queryPart.control !== true)
        continue;

      if (queryPart.operator === 'PROJECT')
        projections = projections.concat(queryPart.value);
    }

    projections = Nife.uniq(Nife.arrayFlatten(projections));

    let areAllSubtractions = true;

    if (Nife.isNotEmpty(projections)) {
      let allModels = this.getAllModelsUsedInQuery(queryEngine, options);

      projections = projections.map((_fieldName) => {
        if (!_fieldName)
          return;

        if (_fieldName instanceof SQLLiteralBase)
          return _fieldName;

        if (_fieldName.prototype instanceof ModelBase)
          return _fieldName;

        if (_fieldName === '-*') {
          areAllSubtractions = false;
          return;
        }

        if (_fieldName === '*') {
          areAllSubtractions = false;
          return _fieldName;
        }

        let { fieldName, direction } = this.getFieldDirectionSpecifier(_fieldName);
        if (!fieldName)
          return;

        if (direction !== '-')
          areAllSubtractions = false;

        let def = ModelUtils.parseQualifiedName(fieldName);
        if (!def.modelName) {
          if (allModels.length > 1)
            throw new Error(`QueryGeneratorBase::getProjectionFromQueryEngine: "${fieldName}" ambiguous. You must use a fully qualified field name for a PROJECT clause. Example: "-Model:id".`);

          def.modelName = allModels[0].getModelName();
        }

        if (!def.fieldNames.length)
          throw new Error(`QueryGeneratorBase::getProjectionFromQueryEngine: No field names found for "${fieldName}".`);

        let field = this.connection.getField(def.fieldNames[0], def.modelName);
        if (!field)
          throw new Error(`QueryGeneratorBase::getProjectionFromQueryEngine: Unable to locate field "${def.modelName}"."${def.fieldNames[0]}".`);

        if (allModels.indexOf(field.Model) < 0)
          return;

        let modelName = field.Model.getModelName();

        return {
          fullFieldName:  `${modelName}:${field.fieldName}`,
          projectedName:  this.getEscapedProjectionName(field, options),
          Model:          field.Model,
          Field:          field,
          fieldName:      field.fieldName,
          modelName,
          direction,
        };
      }).filter(Boolean);
    }

    if (areAllSubtractions)
      projections.push('*');

    if (options)
      this.setOptionsCache(options, `getProjectionFromQueryEngine.${queryEngineContextID}`, projections);

    return projections;
  }

  sortFieldProjectionMap(fieldProjectionMap) {
    let keys    = Array.from(fieldProjectionMap.keys()).sort();
    let newMap  = new Map();

    for (let i = 0, il = keys.length; i < il; i++) {
      let key = keys[i];
      newMap.set(key, fieldProjectionMap.get(key));
    }

    return newMap;
  }

  parseFieldProjection = (str) => {
    let modelName;
    let fieldName;

    str.replace(/(AS\s+)?"(\w+):([\w.]+)"/i, (m, as, _modelName, _fieldName) => {
      modelName = _modelName;
      fieldName = _fieldName;
    });

    if (!modelName || !fieldName) {
      // Reverse search model and field name
      // based on table and column name
      str.replace(/"([^"]+)"."([^"]+)"/i, (m, _tableName, _columnName) => {
        this.connection.findModelField(({ field, stop }) => {
          if (field.columnName !== _columnName)
            return;

          let tableName = field.Model.getTableName();
          if (tableName !== _tableName)
            return;

          modelName = field.Model.getModelName();
          fieldName = field.fieldName;

          stop();
        });
      });
    }

    if (!modelName || !fieldName)
      return str;

    return `${modelName}:${fieldName}`;
  };

  parseFieldProjectionToFieldMap(selectStatement) {
    let firstPart           = selectStatement.replace(/[\r\n]/g, ' ').split(/\s+FROM\s+"/i)[0].replace(/^SELECT\s+/i, '').trim();
    let fieldParts          = firstPart.split(',');
    let projectionFieldMap  = new Map();

    for (let i = 0, il = fieldParts.length; i < il; i++) {
      let fieldPart     = fieldParts[i];
      let fullFieldName = this.parseFieldProjection(fieldPart);

      // getEscapedProjectionName
      if (fullFieldName.indexOf(':') >= 0) {
        let def = ModelUtils.parseQualifiedName(fullFieldName);
        if (!def.modelName || def.fieldNames.length === 0) {
          projectionFieldMap.set(fullFieldName, fullFieldName);
          continue;
        }

        let field = this.connection.getField(def.fieldNames[0], def.modelName);
        if (!field) {
          projectionFieldMap.set(fullFieldName, fullFieldName);
          continue;
        }

        projectionFieldMap.set(fullFieldName, this.getEscapedProjectionName(field));
      } else {
        projectionFieldMap.set(fullFieldName, fullFieldName);
      }
    }

    return this.sortFieldProjectionMap(projectionFieldMap);
  }

  getProjectedFields(queryEngine, _options, asMap) {
    let Models                    = this.getAllModelsUsedInQuery(queryEngine, _options);
    let queryProjection           = this.getProjectionFromQueryEngine(queryEngine, _options);
    let requiredProjectionFields  = this.getProjectionRequiredFields(queryEngine, _options);
    let modelNamesComputed        = {};
    let allProjectionFields       = new Map();
    let options                   = Object.assign({}, _options || {}, { asProjection: true });

    if (queryProjection.indexOf('*') >= 0) {
      let removeFieldsFromProjection = queryProjection.map((projectionField) => {
        if (!projectionField)
          return;

        if (projectionField === '*')
          return;

        if (projectionField instanceof SQLLiteralBase) {
          let result        = projectionField.toString(this.connection);
          let fullFieldName = this.parseFieldProjection(result);
          allProjectionFields.set(fullFieldName, result);

          return;
        }

        if (projectionField.prototype instanceof ModelBase)
          return;

        if (projectionField.direction !== '-')
          return;

        return projectionField.fullFieldName;
      }).filter(Boolean);

      if (Nife.isEmpty(removeFieldsFromProjection))
        removeFieldsFromProjection = null;

      for (let i = 0, il = Models.length; i < il; i++) {
        let Model     = Models[i];
        let modelName = Model.getModelName();

        if (modelNamesComputed[modelName])
          continue;

        modelNamesComputed[modelName] = true;

        let modelFields = this.getEscapedModelFields(Model, options);
        if (!modelFields)
          continue;

        let modelFieldKeys = Object.keys(modelFields);
        for (let j = 0, jl = modelFieldKeys.length; j < jl; j++) {
          let modelFieldKey = modelFieldKeys[j];
          let fullFieldName = modelFields[modelFieldKey];

          if (removeFieldsFromProjection && removeFieldsFromProjection.indexOf(modelFieldKey) >= 0) {
            if (requiredProjectionFields && requiredProjectionFields[fullFieldName])
              continue;
            else
              continue;
          }

          allProjectionFields.set(modelFieldKey, fullFieldName);
        }
      }
    } else {
      for (let i = 0, il = queryProjection.length; i < il; i++) {
        let projectionField = queryProjection[i];
        if (!projectionField)
          continue;

        if (projectionField instanceof SQLLiteralBase) {
          let result = projectionField.toString(this.connection);
          let fullFieldName = this.parseFieldProjection(result);
          if (!fullFieldName)
            fullFieldName = result;

          allProjectionFields.set(fullFieldName, result);

          continue;
        }

        if (projectionField.prototype instanceof ModelBase)
          continue;

        if (projectionField.direction === '-')
          continue;

        let projectedName = projectionField.projectedName;
        allProjectionFields.set(projectionField.fullFieldName, projectedName);
      }
    }

    allProjectionFields = this.sortFieldProjectionMap(allProjectionFields);

    if (asMap === true)
      return allProjectionFields;
    else
      return Array.from(allProjectionFields.values());
  }

  generateSelectQueryFieldProjection(queryEngine, options, asMap) {
    let projectedFields = this.getProjectedFields(queryEngine, options, asMap);

    if (asMap === true)
      return projectedFields;
    else
      return Array.from(projectedFields.values()).join(',');
  }

  // eslint-disable-next-line no-unused-vars
  generateSelectQueryOperatorFromQueryEngineOperator(operator, value, valueIsReference, options) {
    if (operator instanceof SQLLiteralBase)
      return operator.toString(this.connection);

    switch (operator) {
      case 'EQ':
        if (!valueIsReference) {
          if (value === null || value === true || value === false)
            return 'IS';
          else if (Array.isArray(value))
            return 'IN';
        }

        return '=';
      case 'NEQ':
        if (!valueIsReference) {
          if (value === null || value === true || value === false)
            return 'IS NOT';
          else if (Array.isArray(value))
            return 'NOT IN';
        }

        return '!=';
      case 'GT':
        return '>';
      case 'GTE':
        return '>=';
      case 'LT':
        return '<';
      case 'LTE':
        return '<=';
      default:
        throw new Error(`${this.constructor.name}::generateSelectQueryOperatorFromQueryEngineOperator: Unknown operator "${operator}".`);
    }
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

  generateSelectQueryCondition(queryPart, field, operator, _value, options) {
    let value = _value;

    // If the value is an array, then handle the
    // special "IN" case for an array
    if (Array.isArray(value)) {
      // Flatten array, filter down to
      // only unique items, and remove
      // anything that we can't match on
      // (such as "undefined", and objects)
      value = this.prepareArrayValuesForSQL(value);

      // Filter out NULL, TRUE, and FALSE,
      // as these will need to be compared
      // with "IS" or "IS NOT" operators
      let specialValues = value.filter((item) => (item === null || item === false || item === true));

      // See what remains (if anything)
      let arrayValues = value.filter((item) => (item !== null && item !== false && item !== true));

      // If we have special values, then build a
      // condition enclosed in parenthesis
      if (specialValues.length > 0) {
        let subParts = specialValues.map((specialValue) => {
          return this.generateSelectQueryCondition(queryPart, field, operator, specialValue, options);
        });

        if (arrayValues.length > 0)
          subParts.push(this.generateSelectQueryCondition(queryPart, field, operator, arrayValues, options));

        return `(${subParts.join(' OR ')})`;
      }

      // If no values left in array, then
      // skip condition altogether
      if (Nife.isEmpty(arrayValues))
        return '';

      // Otherwise, fall-through
      value = arrayValues;
    }

    let escapedTableName  = this.escapeID(this.getTableNameFromQueryPart(queryPart));
    let escapedColumnName = this.escapeID(field.columnName);
    let sqlOperator       = this.generateSelectQueryOperatorFromQueryEngineOperator(operator, value, false, options);

    if (QueryEngine.isQuery(value)) {
      if (!this.queryHasConditions(value._getRawQuery()))
        return '';

      return `${escapedTableName}.${escapedColumnName} ${sqlOperator} (${this.generateSelectStatement(value, options)})`;
    }

    return `${escapedTableName}.${escapedColumnName} ${sqlOperator} ${this.escape(field, value)}`;
  }

  generateSelectJoinOnTableQueryCondition(leftQueryPart, rightQueryPart, leftField, rightField, operator, options) {
    let leftSideEscapedTableName    = this.escapeID(this.getTableNameFromQueryPart(leftQueryPart));
    let leftSideEscapedColumnName   = this.escapeID(leftField.columnName);
    let rightSideEscapedTableName   = this.escapeID(this.getTableNameFromQueryPart(rightQueryPart));
    let rightSideEscapedColumnName  = this.escapeID(rightField.columnName);
    let sqlOperator                 = this.generateSelectQueryOperatorFromQueryEngineOperator(operator, undefined, true, options);

    return `${leftSideEscapedTableName}.${leftSideEscapedColumnName} ${sqlOperator} ${rightSideEscapedTableName}.${rightSideEscapedColumnName}`;
  }

  // eslint-disable-next-line no-unused-vars
  generateSelectQueryFromTable(Model, joinType, options) {
    if (!Model)
      throw new Error(`${this.constructor.name}::generateSelectQueryFromTable: No valid model provided.`);

    let escapedTableName = this.escapeID(Model.getTableName());
    return (joinType) ? `${joinType} ${escapedTableName}` : `FROM ${escapedTableName}`;
  }

  generateSelectJoinOnTableQueryConditions(leftQueryContext, queryEngine, joinType, options) {
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

    let leftSideModel = leftQueryContext.Model;
    if (!leftSideModel)
      throw new Error(`${this.constructor.name}::generateSelectJoinOnTableQueryConditions: Invalid operation: No model found for left-side of join statement.`);

    let leftSideField = this.connection.getField(leftQueryContext.fieldName, leftQueryContext.modelName);
    if (!leftSideField)
      throw new Error(`${this.constructor.name}::generateSelectJoinOnTableQueryConditions: Invalid operation: No left-side field found to match on for table join statement.`);

    let rightQueryContext = queryEngine._getRawQuery();
    let isNot             = leftQueryContext.not;
    let operator          = (isNot) ? leftQueryContext.inverseOperator : leftQueryContext.operator;
    let firstFieldIndex   = findFirstField(rightQueryContext);

    if (firstFieldIndex < 0)
      throw new Error(`${this.constructor.name}::generateSelectJoinOnTableQueryConditions: Invalid operation: No right-side field found to match on for table join statement.`);

    let nextContext = rightQueryContext[firstFieldIndex + 1];
    if (nextContext && nextContext.condition === true)
      throw new Error(`${this.constructor.name}::generateSelectJoinOnTableQueryConditions: Invalid operation: Expected a field to join on, but instead received a query.`);

    let rightFieldContext = rightQueryContext[firstFieldIndex];
    let rightSideModel    = rightFieldContext.Model;
    if (!rightSideModel)
      throw new Error(`${this.constructor.name}::generateSelectJoinOnTableQueryConditions: Invalid operation: No model found for right-side of join statement.`);

    let rightSideField  = this.connection.getField(rightFieldContext.fieldName, rightFieldContext.modelName);
    if (!rightSideField)
      throw new Error(`${this.constructor.name}::generateSelectJoinOnTableQueryConditions: Invalid operation: No right-side field found to match on for table join statement.`);

    let sqlParts = [
      this.generateSelectQueryFromTable(rightSideModel, joinType, options),
      'ON',
      this.generateSelectJoinOnTableQueryCondition(leftQueryContext, rightFieldContext, leftSideField, rightSideField, operator, options),
    ];

    return sqlParts.join(' ');
  }

  // TODO: Needs to take a join type object
  // eslint-disable-next-line no-unused-vars
  generateSQLJoinTypeFromQueryEngineJoinType(joinType, options) {
    return joinType;
  }

  generateSelectQueryJoinTables(queryEngine, options) {
    let sqlParts  = [];
    let query     = queryEngine._getRawQuery();

    for (let i = 0, il = query.length; i < il; i++) {
      let queryPart = query[i];
      if (!(Object.prototype.hasOwnProperty.call(queryPart, 'condition') && queryPart.condition === true))
        continue;

      let operatorValue = queryPart.value;
      if (!QueryEngine.isQuery(operatorValue))
        continue;

      // TODO: Need query engine to be able to specify join type
      let joinType = this.generateSQLJoinTypeFromQueryEngineJoinType('LEFT INNER JOIN', options);
      let joinOnCondition = this.generateSelectJoinOnTableQueryConditions(queryPart, operatorValue, joinType, options);

      sqlParts.push(joinOnCondition);
    }

    return sqlParts.join(' ');
  }

  generateSelectWhereConditions(queryEngine, options) {
    let query     = queryEngine._getRawQuery();
    let sqlParts  = [];

    for (let i = 0, il = query.length; i < il; i++) {
      let queryPart     = query[i];
      let queryOperator = queryPart.operator;
      let queryValue    = queryPart.value;
      let result        = undefined;

      if (Object.prototype.hasOwnProperty.call(queryPart, 'condition')) {
        if (queryPart.condition !== true)
          continue;

        result = this.generateSelectQueryCondition(queryPart, queryPart.Field, queryOperator, queryValue, options);
      } else if (Object.prototype.hasOwnProperty.call(queryPart, 'logical')) {
        if (queryOperator === 'NOT')
          continue;

        // We shouldn't be adding any logical operator
        // until we have a left-hand value
        if (sqlParts.length > 0) {
          if (queryOperator === 'OR')
            sqlParts.push('OR');
          else if (queryOperator === 'AND')
            sqlParts.push('AND');
        }

        // If we have a value for the logical operator
        // then that means we have a sub-grouping
        if (Object.prototype.hasOwnProperty.call(queryPart, 'value') && QueryEngine.isQuery(queryValue)) {
          result = this.generateSelectWhereConditions(queryValue, options);
          if (result)
            result = `(${result})`;
        }
      }

      if (result)
        sqlParts.push(result);
    }

    return sqlParts.join(' ');
  }

  getFieldDirectionSpecifier(order) {
    if (!order)
      return order;

    if (order instanceof SQLLiteralBase)
      return order;

    let sign;

    let fieldName = ('' + order).replace(/^[+-]+/, (m) => {
      sign = m.charAt(0);
      return '';
    });

    return {
      direction: (sign === '-') ? '-' : '+',
      fieldName,
    };
  }

  // eslint-disable-next-line no-unused-vars
  getOrderLimitOffset(queryEngine, options) {
    let queryEngineContextID  = queryEngine._getTopContextID();
    let cache                 = this.getOptionsCache(options, `getOrderLimitOffset.${queryEngineContextID}`);
    if (cache)
      return cache;

    let query = queryEngine._getRawQuery();
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

    if (Nife.isNotEmpty(order) && !(order instanceof SQLLiteralBase)) {
      let allModels = this.getAllModelsUsedInQuery(queryEngine, options);

      order = order.map((_fieldName) => {
        if (_fieldName instanceof SQLLiteralBase)
          return _fieldName;

        let { fieldName, direction } = this.getFieldDirectionSpecifier(_fieldName);
        if (!fieldName)
          return;

        let def = ModelUtils.parseQualifiedName(fieldName);
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
    }

    let orderLimitOffset = { limit, order, offset };

    if (options)
      this.setOptionsCache(options, `getOrderLimitOffset.${queryEngineContextID}`, orderLimitOffset);

    return orderLimitOffset;
  }

  // eslint-disable-next-line no-unused-vars
  generateOrderClause(_orders, options) {
    if (_orders instanceof SQLLiteralBase)
      return _orders.toString(this.connection);

    let orders = Nife.toArray(_orders).filter(Boolean);
    if (Nife.isEmpty(orders))
      return '';

    let orderByParts = [];
    for (let i = 0, il = orders.length; i < il; i++) {
      let order = orders[i];

      if (order instanceof SQLLiteralBase) {
        orderByParts.push(order.toString(this.connection));
        continue;
      }

      let escapedTableName  = this.escapeID(order.Model.getTableName());
      let escapedColumnName = this.escapeID(order.Field.columnName);

      orderByParts.push(`${escapedTableName}.${escapedColumnName} ${(order.direction === '-') ? 'DESC' : 'ASC'}`);
    }

    return `ORDER BY ${orderByParts.join(',')}`;
  }

  // eslint-disable-next-line no-unused-vars
  generateLimitClause(limit, options) {
    if (limit instanceof SQLLiteralBase)
      return limit.toString(this.connection);

    return `LIMIT ${limit}`;
  }

  // eslint-disable-next-line no-unused-vars
  generateOffsetClause(offset, options) {
    if (offset instanceof SQLLiteralBase)
      return offset.toString(this.connection);

    return `OFFSET ${offset}`;
  }

  generateSelectOrderLimitOffset(queryEngine, options) {
    let {
      order,
      limit,
      offset,
    } = this.getOrderLimitOffset(queryEngine, options);
    let sqlParts = [];

    if (Nife.isNotEmpty(order)) {
      let result = this.generateOrderClause(order, options);
      if (result)
        sqlParts.push(result);
    }

    if (!Object.is(limit, Infinity) && Nife.isNotEmpty(limit)) {
      let result = this.generateLimitClause(limit, options);
      if (result)
        sqlParts.push(result);
    }

    if (Nife.isNotEmpty(offset)) {
      let result = this.generateOffsetClause(offset, options);
      if (result)
        sqlParts.push(result);
    }

    return sqlParts.join(' ');
  }

  generateWhereAndOrderLimitOffset(queryEngine, _options) {
    let options   = _options || {};
    let sqlParts  = [];

    let where = this.generateSelectWhereConditions(queryEngine, options);
    if (where)
      sqlParts.push(`WHERE ${where}`);

    let orderLimitOffset = this.generateSelectOrderLimitOffset(queryEngine, options);
    if (orderLimitOffset)
      sqlParts.push(orderLimitOffset);

    return sqlParts.join(' ');
  }

  generateSelectStatement(queryEngine, _options) {
    let rootModel = queryEngine._getRawQueryContext().rootModel;
    if (!rootModel)
      throw new Error(`${this.constructor.name}::generateSelectQueryJoinTables: No root model found.`);

    let options   = _options || {};
    let sqlParts  = [ 'SELECT' ];
    let projectionFields;

    if (options.returnFieldProjection === true || options.asMap === true) {
      projectionFields = this.generateSelectQueryFieldProjection(queryEngine, options, true);
      sqlParts.push(Array.from(projectionFields.values()).join(','));
    } else {
      sqlParts.push(this.generateSelectQueryFieldProjection(queryEngine, options));
    }

    sqlParts.push(this.generateSelectQueryFromTable(rootModel, undefined, options));
    sqlParts.push(this.generateSelectQueryJoinTables(queryEngine, options));
    sqlParts.push(this.generateWhereAndOrderLimitOffset(queryEngine, options));

    let sql = sqlParts.filter(Boolean).join(' ');

    if (options.returnFieldProjection === true)
      return { sql, projectionFields };
    else
      return sql;
  }

  getFieldDefaultValue(field, fieldName, _options) {
    if (field.defaultValue === undefined)
      return;

    let options           = _options || {};
    let defaultValue      = field.defaultValue;
    let useDefaultKeyword = (Object.prototype.hasOwnProperty.call(options, 'useDefaultKeyword')) ? options.useDefaultKeyword : true;
    let escapeValue       = (Object.prototype.hasOwnProperty.call(options, 'escape')) ? options.escape : true;

    if (typeof defaultValue === 'function') {
      if (options.remoteOnly !== true) {
        defaultValue = defaultValue({ field, fieldName, connection: this.connection });
        defaultValue = (escapeValue) ? this.escape(field, defaultValue) : defaultValue;
      } else if ((field.defaultValue.mythixFlags || 0) & FLAG_LITERAL) {
        defaultValue = defaultValue({ field, fieldName, connection: this.connection });
      } else {
        return;
      }
    } else {
      defaultValue = (escapeValue) ? this.escape(field, defaultValue) : defaultValue;
    }

    if (defaultValue instanceof SQLLiteralBase) {
      useDefaultKeyword = false;

      if (escapeValue)
        defaultValue = defaultValue.toString(this.connection);
    }

    if (useDefaultKeyword)
      return `DEFAULT ${defaultValue}`;
    else
      return `${defaultValue}`;
  }

  // eslint-disable-next-line no-unused-vars
  generateCreateTableStatement(Model, _options) {
    let options = _options || {};
    let fieldParts = [];

    Model.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      let columnName      = field.field || fieldName;
      let constraintParts = [];

      if (field.primaryKey) {
        if (field.primaryKey instanceof SQLLiteralBase)
          constraintParts.push(field.primaryKey.toString(this.connection));
        else
          constraintParts.push('PRIMARY KEY');
      } else {
        if (field.unique) {
          if (field.unique instanceof SQLLiteralBase)
            constraintParts.push(field.unique.toString(this.connection));
          else
            constraintParts.push('UNIQUE');
        }

        if (field.allowNull === false)
          constraintParts.push('NOT NULL');
      }

      let defaultValue = this.getFieldDefaultValue(field, fieldName, { remoteOnly: true });
      if (defaultValue !== undefined)
        constraintParts.push(defaultValue);

      constraintParts = constraintParts.join(' ');
      if (Nife.isNotEmpty(constraintParts))
        constraintParts = ` ${constraintParts}`;

      fieldParts.push(`  ${this.escapeID(columnName)} ${field.type.toString(this)}${constraintParts}`);
    });

    let ifNotExists = 'IF NOT EXISTS ';
    if (options.ifNotExists === false)
      ifNotExists = '';

    return `CREATE TABLE ${ifNotExists}${this.escapeID(Model.getTableName())} (${fieldParts.join(',\n')}\n);`;
  }

  generateInsertFieldValuesFromModel(model, _options) {
    if (!model)
      return '';

    let options   = _options || {};
    let sqlParts  = [];
    let hasParts  = false;

    model.iterateFields(({ field, fieldName }) => {
      let fieldValue = model[fieldName];
      if (fieldValue === undefined)
        fieldValue = this.getFieldDefaultValue(field, fieldName, { useDefaultKeyword: false, escape: false });

      if (fieldValue instanceof SQLLiteralBase)
        fieldValue = fieldValue.toString(this.connection);

      if (fieldValue === undefined) {
        sqlParts.push('');
        return;
      }

      hasParts = true;
      sqlParts.push(this.escape(field, fieldValue));
    }, options.dirtyFields);

    if (!hasParts)
      return '';
    else
      return sqlParts.join(',');
  }

  generateInsertValuesFromModels(Model, _models, _options) {
    let options                 = _options || {};
    let preparedModels          = this.connection.prepareAllModelsForOperation(Model, _models, options);
    let { models, dirtyFields } = preparedModels;
    if (Nife.isEmpty(models))
      return '';

    let sqlParts    = [];
    let subOptions  = Object.assign(Object.create(options), { dirtyFields });

    for (let i = 0, il = models.length; i < il; i++) {
      let model = models[i];
      if (!(model instanceof ModelBase))
        model = new Model(model);

      let rowValues = this.generateInsertFieldValuesFromModel(model, subOptions);
      sqlParts.push(`(${rowValues})`);
    }

    return (options.newlines === false) ? sqlParts.join(',') : sqlParts.join(',\n');
  }

  generateInsertStatement(Model, _models, _options) {
    let options                 = _options || {};
    let preparedModels          = this.connection.prepareAllModelsForOperation(Model, _models, options);
    let { models, dirtyFields } = preparedModels;
    if (Nife.isEmpty(models))
      return '';

    let subOptions  = Object.assign(Object.create(options), {
      asColumn:       true,
      columnNameOnly: true,
      fields:         dirtyFields,
      dirtyFields,
    });

    let values = this.generateInsertValuesFromModels(Model, preparedModels, subOptions);
    if (!values)
      return '';

    let escapedTableName  = this.escapeID(Model.getTableName());
    let escapedFieldNames = Array.from(Object.values(this.getEscapedModelFields(Model, subOptions)));

    return `INSERT INTO ${escapedTableName} (${escapedFieldNames}) VALUES ${values}`;
  }

  generateUpdateStatement(Model, _model, _options, _queryEngine) {
    if (!_model)
      return '';

    let queryEngine = _queryEngine;
    let options     = _options;

    if (QueryEngine.isQuery(options)) {
      queryEngine = options;
      options = {};
    } else if (!options) {
      options = {};
    }

    let model = _model;
    if (!(model instanceof ModelBase))
      model = new Model(model);

    let modelChanges    = model.changes;
    let dirtyFieldNames = Object.keys(modelChanges);
    let dirtyFields     = model.getFields(dirtyFieldNames);
    if (Nife.isEmpty(dirtyFields))
      return '';

    let escapedTableName  = this.escapeID(Model.getTableName());
    let sqlParts          = [ 'UPDATE ', escapedTableName, ' SET ' ];
    let setParts          = [];
    let tabs              = '';

    if (options.newlines !== false) {
      sqlParts.push('\n');
      tabs = '  ';
    }

    for (let i = 0, il = dirtyFields.length; i < il; i++) {
      let dirtyField        = dirtyFields[i];
      let fieldValue        = modelChanges[dirtyField.fieldName].current;
      let escapedColumnName = this.escapeID(dirtyField.columnName);
      let escapedValue      = this.escape(dirtyField, fieldValue);
      if (!escapedValue)
        continue;

      setParts.push(`${tabs}${escapedColumnName} = ${escapedValue}`);
    }

    if (setParts.length === 0)
      return '';

    sqlParts.push((options.newlines === false) ? setParts.join(',') : setParts.join(',\n'));

    if (queryEngine) {
      let where = this.generateWhereAndOrderLimitOffset(queryEngine, options);
      if (where) {
        if (options.newlines !== false)
          sqlParts.push('\n');
        else
          sqlParts.push(' ');

        sqlParts.push(where);
      }
    }

    return sqlParts.join('');
  }

  generateDeleteStatement(Model, _options, _queryEngine) {
    let queryEngine = _queryEngine;
    let options     = _options;
    let where;

    if (QueryEngine.isQuery(options)) {
      queryEngine = options;
      options = {};
    } else if (!options) {
      options = {};
    }

    if (queryEngine)
      where = this.generateWhereAndOrderLimitOffset(queryEngine, options);

    let escapedTableName = this.escapeID(Model.getTableName());
    return `DELETE FROM ${escapedTableName}${(where) ? ` ${where}` : ''}`;
  }
}

module.exports = QueryGeneratorBase;
