'use strict';

const Nife            = require('nife');
const { FLAG_REMOTE } = require('../helpers/default-helpers');
const QueryEngine     = require('../query-engine/query-engine');

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

  // eslint-disable-next-line no-unused-vars
  getEscapedModelFields(Model, options) {
    let fields            = {};
    let modelName         = Model.getModelName();
    let tableName         = Model.getTableName();
    let escapedTableName  = this.escapeID(tableName);
    let escapedModelName  = this.escapeID(modelName);

    Model.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      fields[`${modelName}:${fieldName}`] = `${escapedTableName}.${this.escapeID(field.columnName)} AS ${escapedModelName}.${this.escapeID(field.fieldName)}`;
    });

    return fields;
  }

  // eslint-disable-next-line no-unused-vars
  getAllModelsUsedInQuery(queryEngine, options) {
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

    return Array.from(Models.values());
  }

  getProjectedFields(queryEngine, options) {
    let Models              = this.getAllModelsUsedInQuery(queryEngine);
    let modelNamesComputed  = {};
    let fields              = new Map();

    for (let i = 0, il = Models.length; i < il; i++) {
      let Model     = Models[i];
      let modelName = Model.getModelName();

      if (modelNamesComputed[modelName])
        continue;

      modelNamesComputed[modelName] = true;

      let modelFields = this.getEscapedModelFields(Model, options);
      if (!modelFields)
        continue;

      modelFields = Array.from(Object.values(modelFields));
      for (let j = 0, jl = modelFields.length; j < jl; j++) {
        let field = modelFields[j];
        fields.set(field, field);
      }
    }

    return Array.from(fields.values()).sort();
  }

  generateSelectQueryFieldProjection(queryEngine, options) {
    let projectedFields = this.getProjectedFields(queryEngine, options);
    return Array.from(Object.values(projectedFields)).join(',');
  }

  // eslint-disable-next-line no-unused-vars
  generateSelectQueryFromTable(modelName, joinType, options) {
    let Model = this.connection.getModel(modelName);
    if (!Model)
      throw new Error(`${this.constructor.name}::generateSelectQueryFromTable: Attempted to fetch model named "${modelName}" but no model found.`);

    let escapedTableName = this.escapeID(Model.getTableName());
    return (joinType) ? `${joinType} ${escapedTableName}` : `FROM ${escapedTableName}`;
  }

  // eslint-disable-next-line no-unused-vars
  generateSelectQueryOperatorFromQueryEngineOperator(operator, value, valueIsReference, options) {
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

      return `${escapedTableName}.${escapedColumnName} ${sqlOperator} (${this.generateSelectQuery(value, options)})`;
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
      joinType,
      `${this.escapeID(rightSideModel.getTableName())}`,
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

  generateSelectQuery(queryEngine, _options) {
    let options   = _options || {};
    let sqlParts  = [ 'SELECT' ];

    sqlParts.push(this.generateSelectQueryFieldProjection(queryEngine, options));
    sqlParts.push(this.generateSelectQueryJoinTables(queryEngine, options));

    let where = this.generateSelectWhereConditions(queryEngine, options);
    if (where)
      sqlParts.push(`WHERE ${where}`);

    return sqlParts.filter(Boolean).join(' ');
  }

  getFieldDefaultValue(field, fieldName) {
    if (field.defaultValue === undefined)
      return;

    let defaultValue      = field.defaultValue;
    let useDefaultKeyword = true;

    if (typeof defaultValue === 'function') {
      defaultValue = defaultValue({ field, fieldName, connection: this.connection });
      if (!((field.defaultValue.mythixFlags || 0) & FLAG_REMOTE)) {
        defaultValue = this.escape(field, defaultValue);
      } else if (typeof defaultValue === 'string' && defaultValue.charAt(0) === '!') {
        useDefaultKeyword = false;
        defaultValue = defaultValue.substring(1);
      }
    } else {
      defaultValue = this.escape(field, defaultValue);
    }

    if (useDefaultKeyword)
      return `DEFAULT ${defaultValue}`;
    else
      return `${defaultValue}`;
  }

  generatorCreateTableStatement(Model/*, options */) {
    let fieldParts = [];

    Model.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      let columnName      = field.field || fieldName;
      let constraintParts = [];

      if (field.primaryKey) {
        constraintParts.push('PRIMARY KEY');
      } else {
        if (field.unique === true)
          constraintParts.push('UNIQUE');

        if (field.allowNull === false)
          constraintParts.push('NOT NULL');
      }

      let defaultValue = this.getFieldDefaultValue(field, fieldName);
      if (defaultValue !== undefined)
        constraintParts.push(defaultValue);

      constraintParts = constraintParts.join(' ');
      if (Nife.isNotEmpty(constraintParts))
        constraintParts = ` ${constraintParts}`;

      fieldParts.push(`  ${this.escapeID(columnName)} ${field.type.toString(this)}${constraintParts}`);
    });

    return `CREATE TABLE IF NOT EXISTS ${this.escapeID(Model.getTableName())} (${fieldParts.join(',\n')}\n);`;
  }
}

module.exports = QueryGeneratorBase;
