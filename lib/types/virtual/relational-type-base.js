'use strict';

const Type            = require('../type');
const ModelUtils      = require('../../utils/model-utils');
const { QueryEngine } = require('../../query-engine');

const NAMED_METHOD  = false;
const ROOT_METHOD   = true;

class RelationalTypeBase extends Type {
  static isVirtual() {
    return true;
  }

  static isRelational() {
    return true;
  }

  // Model types work by specifying a "target"
  // and a "value provider" (source).
  // These are fully qualified names, meaning
  // they also point to the model as well as the field.
  // If no model is specified, then it always defaults to
  // "this" model. If no field is specified, then it always
  // defaults to "this PK" of the model.
  // Mythix ORM will recursively walk all models and fields
  // defined until it has the full relationships between
  // all fields.
  constructor(queryFactory, _options) {
    let options = _options || {};

    super(queryFactory, options);

    Object.defineProperties(this, {
      'queryFactory': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        queryFactory,
      },
      'options': {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        options,
      },
    });
  }

  getOptions() {
    return this.options;
  }

  initialize(connection, self, methods) {
    if (typeof this.queryFactory !== 'function') {
      let debugScope = '';

      let Model = this.getModel();
      let Field = this.getField();

      if (Model || Field) {
        let modelName = (Model) ? Model.getModelName() : '<UnknownModel>';
        let fieldName = (Field) ? Field.fieldName : '<UnknownField>';

        debugScope = `"${modelName}:${fieldName}" `;
      }

      throw new TypeError(`${this.constructor.name}::initialize: ${debugScope}Bad relation arguments. Expected a method that returns a query.`);
    }

    super.initialize(connection, self);

    if (!connection)
      throw new Error(`${this.constructor.name}::initialize: A connection is required to use the ModelType.`);

    let field           = this.getField();
    let context         = {};
    let binding         = Object.create(context);
    let operationNames  = Object.keys(methods);

    for (let i = 0, il = operationNames.length; i < il; i++) {
      let operation       = operationNames[i];
      let method          = methods[operation];
      let methodName      = this.fieldNameToOperationName(field, operation, NAMED_METHOD);
      let fullMethodName  = this.fieldNameToOperationName(field, operation, ROOT_METHOD);

      context[operation] = function(...args) {
        return this[methodName].call(...args);
      };

      context[`_${operation}`] = function(...args) {
        return this[fullMethodName].apply(this, args);
      };

      Object.assign(binding, { methodName, fullMethodName, field, type: this });

      ModelUtils.injectModelMethod(self, method.bind(self, binding), methodName, fullMethodName);
    }
  }

  async walkQueryRelations(connection, callback, context, ...args) {
    const prepareSourceAndTarget = (leftContext, rightContext) => {
      return {
        target: {
          Model:      leftContext.Model,
          modelName:  leftContext.modelName,
          field:      leftContext.Field,
          fieldName:  leftContext.fieldName,
        },
        source: {
          Model:      rightContext.Model,
          modelName:  rightContext.modelName,
          field:      rightContext.Field,
          fieldName:  rightContext.fieldName,
        },
      };
    };

    let query       = await this.prepareQuery(Object.assign({ connection }, context), args);
    let queryParts  = query._getRawQuery();

    for (let i = 0, il = queryParts.length; i < il; i++) {
      let leftQueryContext = queryParts[i];

      if (!(Object.prototype.hasOwnProperty.call(leftQueryContext, 'condition') && leftQueryContext.condition))
        continue;

      let conditionValue    = leftQueryContext.value;
      let rightQueryContext = null;

      if (QueryEngine.isQuery(conditionValue)) {
        rightQueryContext = conditionValue._getRawQueryContext();
        if (rightQueryContext.condition)
          continue;
      } else {
        continue;
      }

      callback(prepareSourceAndTarget(leftQueryContext, rightQueryContext));

      if (leftQueryContext.Field.type.isForeignKey()) {
        let fkField         = leftQueryContext.Field.type;
        let targetModel     = fkField.getTargetModel(connection);
        let targetModelName = fkField.getTargetModelName(connection);
        let targetField     = fkField.getTargetField(connection);
        let targetFieldName = fkField.getTargetFieldName(connection);

        if (targetModelName === rightQueryContext.modelName && targetFieldName === rightQueryContext.fieldName)
          continue;

        callback(prepareSourceAndTarget(
          {
            Model:      targetModel,
            modelName:  targetModelName,
            field:      targetField,
            fieldName:  targetFieldName,
          },
          rightQueryContext,
        ));
      }

      if (rightQueryContext.Field.type.isForeignKey()) {
        let fkField         = rightQueryContext.Field.type;
        let targetModel     = fkField.getTargetModel(connection);
        let targetModelName = fkField.getTargetModelName(connection);
        let targetField     = fkField.getTargetField(connection);
        let targetFieldName = fkField.getTargetFieldName(connection);

        if (targetModelName === leftQueryContext.modelName && targetFieldName === leftQueryContext.fieldName)
          continue;

        callback(prepareSourceAndTarget(
          leftQueryContext,
          {
            Model:      targetModel,
            modelName:  targetModelName,
            field:      targetField,
            fieldName:  targetFieldName,
          },
        ));
      }
    }

    return query;
  }

  async getTargetModel(connection, context, ...args) {
    return (await this.prepareQuery(Object.assign({ connection }, context), args))._getRawQueryContext().rootModel;
  }

  async prepareQuery({ connection: _connection, self, field, options: _options, userQuery }, args) {
    let options       = _options || {};
    let queryFactory  = this.queryFactory;
    let connection    = _connection || self.getConnection(options.connection);
    let rootQuery     = await queryFactory.call(
      self,
      Object.assign(
        {
          query:    userQuery || undefined,
          type:     this,
          self,
          connection,
          field,
        },
        connection.getModels() || {},
      ),
      ...(args || []),
    );

    if (!QueryEngine.isQuery(rootQuery)) {
      // eslint-disable-next-line no-magic-numbers
      throw new Error(`${this.constructor.name}::prepareQuery: Query factory is required to return a query. Got "${('' + rootQuery).slice(0, 1000)}" instead.`);
    }

    return rootQuery;
  }

  toConnectionType() {
    throw new Error(`${this.constructor.name}::toConnectionType: Can not convert relational types to DB types.`);
  }

  toString() {
    return `${this.constructor.name} {}`;
  }
}

module.exports = RelationalTypeBase;
