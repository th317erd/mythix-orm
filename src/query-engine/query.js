'use strict';

const TAILCALL = {};

const CONTEXT_FLAG_ROOT   = 0x01;
const CONTEXT_FLAG_MODEL  = 0x02;
const CONTEXT_FLAG_FIELD  = 0x04;
const CONTEXT_FLAG_QUERY  = 0x08;

function createProxy(contextFetcher, handler, _context) {
  const fetchContext = () => {
    return Object.assign({}, contextFetcher(context), context);
  };

  const wrapProxyHandler = (handlerFunc) => {
    return function(...args) {
      let thisContext = fetchContext();
      return handlerFunc.apply(thisContext, args);
    };
  };

  let context       = (_context) ? _context : {};
  let proxyHandlers = {};
  let keys          = Object.keys(handler);

  for (let i = 0, il = keys.length; i < il; i++) {
    let key = keys[i];
    proxyHandlers[key] = wrapProxyHandler(handler[key]);
  }

  return new Proxy(context, proxyHandlers);
}

function queryOperator(operatorName, callback, _options) {
  let options = _options || {};

  let func = function(...args) {
    let context = this.getQueryContext();

    let result = callback.apply(this, [ context ].concat(args));
    if (result !== undefined)
      return result;

    return createQueryProxy(this.fetchModelContext);
  };

  Object.defineProperties(func, {
    'mythixOperatorName': {
      writable:     true,
      enumberable:  false,
      configurable: true,
      value:        operatorName,
    },
    'mythixTailCall': {
      writable:     true,
      enumberable:  false,
      configurable: true,
      value:        (options.tailCall !== false),
    },
    'mythixAllowCall': {
      writable:     true,
      enumberable:  false,
      configurable: true,
      value:        (options.allowCall !== false),
    },
    'mythixContextFlags': {
      writable:     true,
      enumberable:  false,
      configurable: true,
      value:        options.contextFlags || 0,
    },
  });

  return func;
}

const OPERATORS = {
  EQ: queryOperator('EQ', function(context, value) {
    context.push(Object.assign({}, this, { operator: 'EQ', value, index: context.length }));
  }, { tailCall: false, contextFlags: CONTEXT_FLAG_FIELD }),
  AND: queryOperator('AND', function(context) {
    context.push(Object.assign({}, this, { operator: 'AND', index: context.length }));
  }, { contextFlags: CONTEXT_FLAG_QUERY }),
  OR: queryOperator('OR', function(context) {
    context.push(Object.assign({}, this, { operator: 'OR', index: context.length }));
  }, { contextFlags: CONTEXT_FLAG_QUERY }),
  NOT: queryOperator('NOT', function(context) {
    context.push(Object.assign({}, this, { operator: 'NOT', index: context.length }));
  }, { allowCall: false, contextFlags: CONTEXT_FLAG_MODEL }),
  getRawQueryContext: queryOperator('getRawQueryContext', function(context) {
    return context;
  }, { tailCall: false, contextFlags: CONTEXT_FLAG_QUERY }),
};

function getContextOperator(operatorName, contextFlags) {
  let operatorNames = Object.keys(OPERATORS);
  for (let i = 0, il = operatorNames.length; i < il; i++) {
    let thisOperatorName = operatorNames[i];
    if (thisOperatorName !== operatorName)
      continue;

    let operator = OPERATORS[operatorName];
    if (operator.mythixContextFlags & contextFlags)
      return operator;
  }
}

function getOperatorName(operator) {
  if (typeof operator !== 'function')
    return '<NONE>';

  if (operator.mythixOperatorName)
    return operator.mythixOperatorName;

  return operator.name || '<NONE>';
}

function tailCallOperator(operator) {
  if (operator.mythixTailCall === false)
    throw new Error(`QueryEngine: ${getOperatorName(operator)} must be invoked`);

  let thisContext = Object.create(this);
  thisContext.isTailCall = true;

  return operator.call(thisContext);
}

function createQueryOperatorProxy(operatorName, operator, contextFetcher) {
  const fetchQueryContext = () => {
    return Object.assign({}, contextFetcher(), {
      operatorName,
      operator,
      queryProxy,
      fetchQueryContext,
    });
  };

  let queryProxy = createProxy(
    fetchQueryContext,
    {
      get: function(target, propName, receiver) {
        if (typeof target === 'function')
          tailCallOperator.call(this, target);

        return this.modelContextGetHandler.call(this, target, propName, receiver);
      },
      apply: function(target, thisArg, args) {
        if (target.mythixAllowCall === false)
          throw new Error(`QueryEngine: ${operatorName} can not be invoked`);

        return target.apply(this, args);
      },
    },
    operator,
  );

  return queryProxy;
}

function createQueryProxy(contextFetcher) {
  const fetchQueryContext = () => {
    return Object.assign({}, contextFetcher(), {
      queryProxy,
      fetchQueryContext,
      queryContextGetHandler,
    });
  };

  const queryContextGetHandler = function(target, propName, receiver) {
    let operator = getContextOperator.call(this, propName, CONTEXT_FLAG_QUERY);
    if (!operator)
      throw new Error(`QueryEngine: Unknown operator "${propName}"`);

    return createQueryOperatorProxy(propName, operator, fetchQueryContext);
  };

  let queryProxy = createProxy(
    fetchQueryContext,
    {
      get: queryContextGetHandler,
    },
  );

  return queryProxy;
}

function createOperatorProxy(operatorName, operator, contextFetcher) {
  const fetchOperatorContext = () => {
    return Object.assign({}, contextFetcher(), { operatorName, operator, operatorProxy, fetchOperatorContext, previousValue: operator });
  };

  let operatorProxy = createProxy(
    fetchOperatorContext,
    {
      get: function(target, propName, receiver) {
        if (target === 'function')
          tailCallOperator.call(this, target);
      },
      apply: function(target, thisArg, args) {
        if (target.mythixAllowCall === false)
          throw new Error(`QueryEngine: ${operatorName} can not be invoked`);

        return target.apply(this, args);
      },
    },
    operator,
  );

  return operatorProxy;
}

function createFieldProxy(fieldName, field, contextFetcher) {
  const fetchFieldContext = () => {
    return Object.assign({}, contextFetcher(), {
      fieldName,
      field,
      fieldProxy,
      fetchFieldContext,
      previousValue: field,
      fieldContextGetHandler,
    });
  };

  const fieldContextGetHandler = function(target, propName, receiver) {
    let operator = getContextOperator.call(this, propName, CONTEXT_FLAG_FIELD);
    if (!operator)
      throw new Error(`QueryEngine: Unknown operator "${propName}"`);

    return createOperatorProxy(propName, operator, fetchFieldContext);
  };

  let fieldProxy = createProxy(
    fetchFieldContext,
    {
      get: fieldContextGetHandler,
    },
  );

  return fieldProxy;
}

function createModelProxy(modelName, Model, contextFetcher) {
  const fetchModelContext = () => {
    return Object.assign({}, contextFetcher(), {
      modelName,
      Model,
      modelProxy,
      fetchModelContext,
      previousValue: Model,
      modelContextGetHandler,
    });
  };

  const modelContextGetHandler = function(target, propName, receiver) {
    // First, check if this is an operator
    let operator = getContextOperator.call(this, propName, CONTEXT_FLAG_MODEL);
    if (typeof operator === 'function')
      return createQueryOperatorProxy(propName, operator, fetchModelContext);

    // Next, check if this is a field
    let field = this.Model.getField(propName);
    if (!field)
      throw new Error(`QueryEngine: Field named "${modelName}.${propName}" not found`);

    return createFieldProxy(propName, field, fetchModelContext);
  };

  let modelProxy = createProxy(
    fetchModelContext,
    {
      get:    modelContextGetHandler,
      apply:  function(target, thisArg, args) {
        return target.apply(this, args);
      },
    },
    function(fieldName) {
      let field = this.Model.getField(fieldName);
      if (!field)
        throw new Error(`QueryEngine: Field named "${modelName}.${fieldName}" not found`);

      return createFieldProxy(fieldName, field, fetchModelContext);
    },
  );

  return modelProxy;
}

function query(...models) {
  const rootContextGetHandler = function(target, propName, receiver) {
    if ((/^[A-Z]/).test(propName))
      return this.getModel(propName);

    console.log('Attempting to call: ', target, propName, receiver);
  };

  let rootProxy = createProxy(
    () => {
      const fetchRootContext = () => {
        return {
          rootProxy,
          models,
          getModel,
          getQueryContext,
          fetchRootContext,
        };
      };

      const getModel = (modelName) => {
        let Model = models.find((model) => model.name === modelName);
        if (!Model)
          throw new Error(`QueryEngine: Model named "${modelName}" not found`);

        return createModelProxy(modelName, Model, fetchRootContext);
      };

      const getQueryContext = () => {
        return queryContext;
      };

      const queryContext = [];

      return fetchRootContext();
    },
    {
      get: rootContextGetHandler,
    },
  );

  return rootProxy;
}

module.exports = query;
