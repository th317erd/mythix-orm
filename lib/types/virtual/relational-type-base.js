'use strict';

const Nife            = require('nife');
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

  async getTargetModel(connection, { self, originField }) {
    return await this.prepareQuery(connection, self, originField).rootModel;
  }

  async prepareQuery(_connection, self, field, ...args) {
    let queryFactory      = this.queryFactory;
    let connection        = _connection || self.getConnection();
    let rootQuery         = await queryFactory.call(
      self,
      Object.assign(
        {
          self,
          connection,
          field,
          thisType: this,
          args,
        },
        connection.getModels() || {},
      ),
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
