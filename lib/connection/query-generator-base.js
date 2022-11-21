'use strict';

const Nife        = require('nife');
const Literals    = require('./literals');
const LiteralBase = Literals.LiteralBase;

/// The base query generator class.
///
/// Alias: QueryGenerator
class QueryGeneratorBase {
  constructor(connection) {
    Object.defineProperties(this, {
      'connection': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        connection,
      },
    });
  }

  stackAssign(obj, ..._args) {
    let newObj = Object.create(obj || {});
    let args = _args.filter(Boolean);

    if (args.length > 0)
      Object.assign(newObj, ...args);

    return newObj;
  }

  escape(...args) {
    return this.connection.escape(...args);
  }

  escapeID(...args) {
    return this.connection.escapeID(...args);
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

    if (options && options.columnNamePrefix)
      columnName = `${options.columnNamePrefix}${columnName}`;

    if (!Model || (options && options.columnNameOnly === true))
      return this.escapeID(columnName);
    else
      return `${this.getEscapedTableName(Model)}.${this.escapeID(columnName)}`;
  }

  getEscapedTableName(_modelOrField, options) {
    let Model     = (_modelOrField.Model) ? _modelOrField.Model : _modelOrField;
    let tableName = Model.getTableName(this.connection);

    if (options && options.tableNamePrefix)
      tableName = `${options.tableNamePrefix}${tableName}`;

    return this.escapeID(tableName);
  }

  // eslint-disable-next-line no-unused-vars
  getEscapedProjectionName(Model, field, options) {
    if (options && options.noProjectionAliases)
      return this.getEscapedColumnName(Model, field, options);
    else
      return `${this.getEscapedColumnName(Model, field, options)} AS ${(options && options.as) ? this.escapeID(options.as) : this.getEscapedFieldName(Model, field, options)}`;
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

  isFieldIdentifier(str) {
    return (/^"[^"]+"."[^"]+"|"\w+:[\w.]+"/i).test(str);
  }

  getQueryEngineOrder(queryEngine, _options) {
    let options = _options || {};
    let context = queryEngine.getOperationContext();
    let order   = context.order;

    return (order && order.size) ? order : this.connection.getDefaultOrder(context.rootModel, options);
  }

  getProjectedFields(queryEngine, _options, asMap) {
    let options             = this.stackAssign(_options || {}, { isProjection: true });
    let context             = queryEngine.getOperationContext();
    let queryProjection     = new Map(context.projection);
    let order               = this.getQueryEngineOrder(queryEngine, options);
    let allProjectionFields = new Map();

    if (order && order.size) {
      let contextOrderSupport = this.connection.isOrderSupportedInContext(options);
      if (contextOrderSupport) {
        for (let [ fullyQualifiedFieldName, orderScope ] of order) {
          if (!queryProjection.has(fullyQualifiedFieldName))
            queryProjection.set(fullyQualifiedFieldName, orderScope);
        }
      }
    }

    for (let [ fullyQualifiedName, projectedScope ] of queryProjection) {
      let { value } = projectedScope;

      if (Nife.instanceOf(value, 'string')) {
        // Raw string is treated as a literal
        allProjectionFields.set(fullyQualifiedName, value);
        continue;
      } else if (LiteralBase.isLiteral(value)) {
        let result = value.toString(this.connection, options);
        allProjectionFields.set(result || fullyQualifiedName, result || fullyQualifiedName);

        continue;
      }

      let escapedFieldName = this.getEscapedProjectionName(value.Model, value, options);
      allProjectionFields.set(`${value.Model.getModelName()}:${value.fieldName}`, escapedFieldName);
    }

    if (asMap === true)
      return allProjectionFields;
    else
      return Array.from(allProjectionFields.values());
  }

  // eslint-disable-next-line no-unused-vars
  getJoinTableInfoFromQueryContexts(leftQueryContext, rightQueryContext, joinType, options) {
    let rootModel         = leftQueryContext.rootModel;
    let rootModelName     = rootModel.getModelName();
    let leftSideModel     = leftQueryContext.Model;
    let leftSideModelName = leftQueryContext.modelName;
    if (!leftSideModel)
      throw new Error(`${this.constructor.name}::getJoinTableInfoFromQueryEngine: Invalid operation: No model found for left-side of join statement.`);

    let leftSideField = leftQueryContext.Field;
    if (!leftSideField)
      throw new Error(`${this.constructor.name}::getJoinTableInfoFromQueryEngine: Invalid operation: No left-side field found to match on for table join statement.`);

    let isNot               = leftQueryContext.not;
    let operator            = (isNot) ? leftQueryContext.inverseOperator : leftQueryContext.operator;
    let rightSideModel      = rightQueryContext.Model;
    let rightSideModelName  = rightQueryContext.modelName;
    if (!rightSideModel)
      throw new Error(`${this.constructor.name}::getJoinTableInfoFromQueryEngine: Invalid operation: No model found for right-side of join statement.`);

    let rightSideField = rightQueryContext.Field;
    if (!rightSideField)
      throw new Error(`${this.constructor.name}::getJoinTableInfoFromQueryEngine: Invalid operation: No right-side field found to match on for table join statement.`);

    let swapJoinRelation    = (rightSideModelName === rootModelName);
    let joinModel           = (swapJoinRelation) ? leftSideModel : rightSideModel;
    let joinModelName       = (swapJoinRelation) ? leftSideModelName : rightSideModelName;

    return {
      operator,
      joinType,
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

    if (LiteralBase.isLiteral(order))
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
    let context = queryEngine.getOperationContext();

    return {
      limit:  context.limit,
      offset: context.offset,
      order:  this.getQueryEngineOrder(queryEngine, options),
    };
  }

  getQuerySliceFromQueryPart(queryPart) {
    let operationStack = queryPart.operationStack;
    let index     = operationStack.indexOf(queryPart);

    return operationStack.slice(index);
  }

  _getLiteralAlias(literal, options) {
    let as = (literal.options && literal.options.as) || (options && options.as);
    if (Nife.isEmpty(as))
      return '';

    return ` AS ${this.escapeID(as)}`;
  }

  _averageLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let field = literal.getField(this.connection);
    let escapedFieldName;

    if (LiteralBase.isLiteral(field))
      escapedFieldName = field.toString(this.connection, options);
    else
      escapedFieldName = this.getEscapedColumnName(field.Model, field, this.stackAssign(options, literal.options));

    return `AVG(${escapedFieldName})${this._getLiteralAlias(literal, options)}`;
  }

  _countLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let field = literal.getField(this.connection);
    let escapedFieldName;

    if (field) {
      if (LiteralBase.isLiteral(field))
        escapedFieldName = field.toString(this.connection, options);
      else
        escapedFieldName = this.getEscapedColumnName(field.Model, field, this.stackAssign(options, literal.options));
    } else {
      escapedFieldName = '*';
    }

    return `COUNT(${escapedFieldName})${this._getLiteralAlias(literal, options)}`;
  }

  _distinctLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let field = literal.getField(this.connection);

    if (LiteralBase.isLiteral(field))
      return `DISTINCT ${field.toString(this.connection, options)}${this._getLiteralAlias(literal, options)}`;

    return `DISTINCT ${this.getEscapedProjectionName(field.Model, field, this.stackAssign(options, literal.options))}${this._getLiteralAlias(literal, options)}`;
  }

  _fieldLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let field = literal.getField(this.connection);
    if (LiteralBase.isLiteral(field))
      return field.toString(this.connection, options);

    return this.getEscapedProjectionName(field.Model, field, this.stackAssign(options, { noProjectionAliases: (options && !options.isProjection) }, literal.options));
  }

  _maxLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let field = literal.getField(this.connection);
    let escapedFieldName;

    if (LiteralBase.isLiteral(field))
      escapedFieldName = field.toString(this.connection, options);
    else
      escapedFieldName = this.getEscapedColumnName(field.Model, field, this.stackAssign(options, literal.options));

    return `MAX(${escapedFieldName})${this._getLiteralAlias(literal, options)}`;
  }

  _minLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let field = literal.getField(this.connection);
    let escapedFieldName;

    if (LiteralBase.isLiteral(field))
      escapedFieldName = field.toString(this.connection, options);
    else
      escapedFieldName = this.getEscapedColumnName(field.Model, field, this.stackAssign(options, literal.options));

    return `MIN(${escapedFieldName})${this._getLiteralAlias(literal, options)}`;
  }

  _sumLiteralToString(literal, options) {
    if (!literal || !LiteralBase.isLiteral(literal))
      return;

    let field = literal.getField(this.connection);
    let escapedFieldName;

    if (LiteralBase.isLiteral(field))
      escapedFieldName = field.toString(this.connection, options);
    else
      escapedFieldName = this.getEscapedColumnName(field.Model, field, this.stackAssign(options, literal.options));

    return `SUM(${escapedFieldName})${this._getLiteralAlias(literal, options)}`;
  }

  // eslint-disable-next-line no-unused-vars
  toConnectionString(queryEngine, options) {
    return '<not supported by connection>';
  }
}

module.exports = QueryGeneratorBase;
