'use strict';

const Nife = require('nife');

// The code below will take a "query object"
// and convert it into Mythix ORM query.
//
// "query objects" are objects with a simple
// structure and convention to build complex queries.
//
// Fields inside these objects can have operators,
// which are postfixed to the field name. For example,
// you could create a filter to find a user by name
// with the following query object:
// { "firstName=": "John", "lastName!=": "Bob" }
// which would find all users with the first name
// of "John", and any last name except "Bob".
//
// AND and OR conditions are also supported. These
// work based of the structure of the object itself.
// If an array is used, then OR is in effect.
// If an object is used, then AND is in effect.
// For example, the following query object:
// [ { firstName: "John", lastName: "Brown" }, { firstName: "Mary", lastName: "Smith" } ]
// would result in the following query:
// WHERE ((firstName = 'John' AND lastName = 'Brown') OR (firstName = 'Mary' AND lastName = 'Smith')),
// finding either user John Brown, or Mary Smith.
//
// IN and NOT IN operators are handled automatically
// when the operator is either "=" or "!=", and the
// provided value is an array. For example:
// { firstName: [ 'John', 'Bob', 'Mary' ] }
// would result in the following query:
// WHERE firstName IN ('John', 'Bob', 'Mary')

const FILTER_OPERATORS = {
  '=':  (Model, fieldName, query, value) => {
    return query.AND[fieldName].EQ(value);
  },
  '!=': (Model, fieldName, query, value) => {
    return query.AND[fieldName].NEQ(value);
  },
  '>':  (Model, fieldName, query, value) => {
    return query.AND[fieldName].GT(value);
  },
  '>=': (Model, fieldName, query, value) => {
    return query.AND[fieldName].GTE(value);
  },
  '<':  (Model, fieldName, query, value) => {
    return query.AND[fieldName].LT(value);
  },
  '<=': (Model, fieldName, query, value) => {
    return query.AND[fieldName].LTE(value);
  },
  // Between
  '><': (Model, fieldName, query, value) => {
    return query.AND(Model.where[fieldName].GTE(value[0]).AND[fieldName].LTE(value[1]));
  },
  // Not between
  '<>': (Model, fieldName, query, value) => {
    return query.AND(Model.where[fieldName].LT(value[0]).OR[fieldName].GT(value[1]));
  },
  // Like (TODO: Mythix ORM needs to support this)
  '*':  (Model, fieldName, query, value) => {
    return query.EQ(value);
  },
  // NOT Like (TODO: Mythix ORM needs to support this)
  '!*': (Model, fieldName, query, value) => {
    return query.EQ(value);
  },
};

function parseFilterFieldAndOperator(fieldName) {
  let operator = '=';
  let field;

  ('' + fieldName).replace(/^\s*(\w+)\s*(.*)$/, (m, _field, _operator) => {
    field = _field.trim();

    if (Nife.isNotEmpty(_operator))
      operator = _operator.trim();
  });

  if (Nife.isEmpty(field))
    throw new Error('generateQueryFromFilter: "field" is blank');

  if (!Object.prototype.hasOwnProperty.call(FILTER_OPERATORS, operator))
    throw new Error(`generateQueryFromFilter: Unknown operator "${operator}"`);

  return { field, operator };
}

function generateQueryFromFilter(connection, Model, _filter, _depth) {
  const getOperator = (name) => {
    let func = FILTER_OPERATORS[name];
    if (!func)
      throw new Error(`generateQueryFromFilter: No such operator "${name}"`);

    return func;
  };

  let filter  = _filter;
  let query   = Model.where(connection);
  let depth   = _depth || 0;

  // [ { id: 1 }, { name: 'derp' }, [ { 'dob>=': '2000-01-01', 'role': [ 'person', 'dog' ] } ] ]...
  //   SQL = `id = 1 OR name = 'derp' OR (dob >= '2000-01-01' AND role IN [ 'person', 'dog' ])`

  // If filter provided is a mythix model,
  // then convert the model attributes to
  // an object so we can iterate the attributes
  if (!Nife.instanceOf(filter, 'array', 'object', 'set', 'map')) {
    if (Model.isModel(filter)) {
      let pkFieldName     = filter.getPrimaryKeyFieldName();
      let primaryKeyValue = filter[pkFieldName];

      if (pkFieldName && primaryKeyValue != null)
        filter = { [pkFieldName]: primaryKeyValue };
      else
        filter = filter.toJSON();
    } else {
      return;
    }
  }

  Nife.iterate(filter, ({ key, value: _value, type }) => {
    let isArrayType = (type === 'Array' || type === 'Set');
    let fieldName   = (isArrayType) ? null : key;
    let value       = _value;

    if (isArrayType && value == null)
      return;

    if (value === undefined)
      return;

    if (isArrayType && (Nife.instanceOf(value, 'array', 'object', 'set', 'map') || Model.isModel(value))) {
      query = query.OR(generateQueryFromFilter(connection, Model, value, depth + 1));
    } else {
      let {
        field,
        operator,
      } = parseFilterFieldAndOperator(fieldName);

      if (!field)
        return;

      let modelField = Model.getField(field);
      if (!modelField || modelField.type.isVirtual())
        return;

      let queryOperator = getOperator(operator);

      if (operator !== '<>' && operator !== '><' && Nife.instanceOf(value, 'array', 'set')) {
        if (Nife.instanceOf(value, 'set'))
          value = Array.from(value.values());

        value = value.filter((v) => Nife.isNotEmpty(v));

        if (Nife.isEmpty(value))
          return;

        if (operator === '=' || operator === '!=')
          queryOperator = getOperator(operator);
        else
          throw new Error(`Invalid array value for operator "${operator}"`);
      }

      if (value === null) {
        if (operator === '=' || operator === '!=')
          queryOperator = getOperator(operator);
        else
          throw new Error(`Invalid "NULL" value for operator "${operator}"`);
      }

      query = queryOperator(Model, field, query, value);
    }
  });

  return query;
}

module.exports = {
  parseFilterFieldAndOperator,
  generateQueryFromFilter,
};
