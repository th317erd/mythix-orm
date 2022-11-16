'use strict';

const Nife            = require('nife');
const Type            = require('../type');
const Utils           = require('../../utils');
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

  constructor(_targetModelName, queryFactory, _options) {
    let options = _options || {};

    super(_targetModelName, queryFactory, options);

    let targetModelName = _targetModelName;
    if (!targetModelName || !(Nife.instanceOf(targetModelName, 'string') || targetModelName._isMythixModel))
      throw new TypeError(`${this.constructor.name}::constructor: First argument is required to be a model or a model name.`);

    if (targetModelName._isMythixModel)
      targetModelName = targetModelName.getModelName();

    Object.defineProperties(this, {
      'targetModelName': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        targetModelName,
      },
      'queryFactory': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        queryFactory,
      },
      'options': {
        writable:     true,
        enumerable:   false,
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
        return this[methodName].apply(this, args);
      };

      context[`_${operation}`] = function(...args) {
        return this[fullMethodName].apply(this, args);
      };

      Object.assign(binding, { methodName, fullMethodName, field, type: this });

      Utils.injectModelMethod(self, method.bind(self, binding), methodName, fullMethodName);
    }
  }

  async walkQueryRelations(connection, callback, context, ...args) {
    const isAlreadyVisited = (leftContext, rightContext) => {
      // Model can't be related to itself
      if (leftContext.modelName === rightContext.modelName)
        return true;

      let firstKey  = `${leftContext.modelName}:${leftContext.fieldName}<->${rightContext.modelName}:${rightContext.fieldName}`;
      let secondKey = `${rightContext.modelName}:${rightContext.fieldName}<->${leftContext.modelName}:${leftContext.fieldName}`;
      if (alreadyVisited.has(firstKey))
        return true;

      if (alreadyVisited.has(secondKey))
        return true;

      alreadyVisited.add(firstKey);
      alreadyVisited.add(secondKey);

      return false;
    };

    const prepareSourceAndTarget = (leftContext, rightContext) => {
      const voidFieldForNonRelational = (info) => {
        let { target, source } = info;

        if (target.field.type.isRelational() && !source.field.type.isRelational()) {
          source.field = null;
          source.fieldName = null;
        }

        if (!target.field.type.isRelational() && source.field.type.isRelational()) {
          target.field = null;
          target.fieldName = null;
        }

        return info;
      };

      let target = {
        Model:     leftContext.Model,
        modelName: leftContext.modelName,
        field:     leftContext.Field,
        fieldName: leftContext.fieldName,
      };

      let source = {
        Model:     rightContext.Model,
        modelName: rightContext.modelName,
        field:     rightContext.Field,
        fieldName: rightContext.fieldName,
      };

      // The primary model is always the target
      // so swap the order if it is currently
      // marked as the source
      let swapRelation = (source.modelName === primaryModelName);

      return voidFieldForNonRelational({
        PrimaryModel,
        TargetModel,
        TargetField,
        target: (swapRelation) ? source : target,
        source: (swapRelation) ? target : source,
      });
    };

    let PrimaryModel      = (context.self && context.self.getModel());
    let primaryModelName  = PrimaryModel.getModelName();
    let query             = await this.prepareQuery(Object.assign({ connection }, context), args);
    let queryParts        = query.getOperationStack();
    let queryContext      = (queryParts[queryParts.length - 1] || {});
    let TargetModel       = queryContext.rootModel;
    let TargetField       = queryContext.rootField;
    let alreadyVisited    = new Set();

    for (let i = 0, il = queryParts.length; i < il; i++) {
      let leftQueryContext = queryParts[i];

      if (!(Object.prototype.hasOwnProperty.call(leftQueryContext, 'condition') && leftQueryContext.condition))
        continue;

      let conditionValue    = leftQueryContext.value;
      let rightQueryContext = null;

      if (QueryEngine.isQuery(conditionValue)) {
        rightQueryContext = conditionValue.getOperationContext();
        if (rightQueryContext.condition)
          continue;
      } else {
        continue;
      }

      if (isAlreadyVisited(leftQueryContext, rightQueryContext))
        continue;

      callback(prepareSourceAndTarget(leftQueryContext, rightQueryContext));

      if (leftQueryContext.Field.type.isForeignKey()) {
        let fkField         = leftQueryContext.Field.type;
        let targetModel     = fkField.getTargetModel(connection);
        let targetModelName = fkField.getTargetModelName(connection);
        let targetField     = fkField.getTargetField(connection);
        let targetFieldName = fkField.getTargetFieldName(connection);
        let fkContext       = {
          Model:     targetModel,
          modelName: targetModelName,
          field:     targetField,
          fieldName: targetFieldName,
        };

        if (!isAlreadyVisited(fkContext, rightQueryContext))
          callback(prepareSourceAndTarget(fkContext, rightQueryContext));
      }

      if (rightQueryContext.Field.type.isForeignKey()) {
        let fkField         = rightQueryContext.Field.type;
        let targetModel     = fkField.getTargetModel(connection);
        let targetModelName = fkField.getTargetModelName(connection);
        let targetField     = fkField.getTargetField(connection);
        let targetFieldName = fkField.getTargetFieldName(connection);
        let fkContext       = {
          Model:     targetModel,
          modelName: targetModelName,
          field:     targetField,
          fieldName: targetFieldName,
        };

        if (!isAlreadyVisited(leftQueryContext, fkContext))
          callback(prepareSourceAndTarget(leftQueryContext, fkContext));
      }
    }

    if (TargetField.type.isRelational() && TargetField.type.isManyRelation())
      return query;

    if (context.field.type.isRelational() && context.field.type.isManyRelation())
      return query;

    // Here the source and target are swapped
    // because the target will be the primary model
    // and the source (target model) is where we will
    // be copying field values from
    let targetModelName = TargetModel.getModelName();
    let source      = {
      Model:     TargetModel,
      modelName: targetModelName,
      Field:     TargetField,
      fieldName: TargetField.fieldName,
    };
    let target    = {
      Model:     PrimaryModel,
      modelName: PrimaryModel.getModelName(),
      Field:     context.field,
      fieldName: context.field.fieldName,
    };

    if (!isAlreadyVisited(source, target))
      callback(prepareSourceAndTarget(source, target));

    return query;
  }

  getTargetModel(_connection) {
    let connection = _connection || this.getModel().getConnection();
    return connection.getModel(this.targetModelName);
  }

  async prepareQuery({ connection: _connection, self, field, options: _options }, args) {
    let options       = _options || {};
    let queryFactory  = this.queryFactory;
    let connection    = _connection || self.getConnection(options.connection);
    let TargetModel   = this.getTargetModel(connection);
    let rootQuery     = await queryFactory.call(
      self,
      {
        type: this,
        self,
        connection,
        field,
      },
      connection.getModels() || {},
      ...(args || []),
    );

    if (!QueryEngine.isQuery(rootQuery)) {
      // eslint-disable-next-line no-magic-numbers
      throw new Error(`${this.constructor.name}::prepareQuery: Query factory is required to return a query. Got "${('' + rootQuery).slice(0, 1000)}" instead.`);
    }

    return rootQuery.AND[TargetModel.getModelName()];
  }

  toConnectionType() {
    throw new Error(`${this.constructor.name}::toConnectionType: Can not convert relational types to DB types.`);
  }

  toString() {
    return `${this.constructor.name} {}`;
  }
}

module.exports = RelationalTypeBase;
