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
  '=': (Model, fieldName, query, value) => {
    return query.AND[fieldName].EQ(value);
  },
  '!=': (Model, fieldName, query, value) => {
    return query.AND[fieldName].NEQ(value);
  },
  '>': (Model, fieldName, query, value) => {
    return query.AND[fieldName].GT(value);
  },
  '>=': (Model, fieldName, query, value) => {
    return query.AND[fieldName].GTE(value);
  },
  '<': (Model, fieldName, query, value) => {
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
  // Like
  '*': (Model, fieldName, query, value) => {
    return query.AND[fieldName].LIKE(value);
  },
  // NOT Like
  '!*': (Model, fieldName, query, value) => {
    return query.AND[fieldName].NOT_LIKE(value);
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

function margeFields(RootModel, connection, currentFields, _incomingFields) {
  const RESET = 0;
  const ADD   = 1;
  const SUB   = 2;

  const getInitialMode = (incomingFields) => {
    for (let i = 0, il = incomingFields.length; i < il; i++) {
      let incomingField = incomingFields[i];
      if (!incomingField)
        continue;

      if (typeof incomingField.isLiteral === 'function' && incomingField.isLiteral(incomingField))
        return RESET;

      if (typeof incomingField.isModelClass === 'function' && incomingField.isModelClass(incomingField))
        return RESET;

      if (typeof incomingField.isField === 'function' && incomingField.isField(incomingField))
        return RESET;

      if (!Nife.instanceOf(incomingField, 'string'))
        RESET;

      let firstChar = incomingField.charAt(0);
      if (firstChar === '+')
        return ADD;

      if (firstChar === '-')
        return SUB;

      return RESET;
    }
  };

  const addOrRemove = (mode, key, item) => {
    if (mode === ADD)
      fields.set(key, item);
    else
      fields.delete(key);
  };

  const addOrRemoveAllModelFields = (mode, Model) => {
    let modelName = Model.getModelName();

    Model.iterateFields(({ field, fieldName }) => {
      if (field.type.isVirtual())
        return;

      let fullFieldName = `${modelName}:${fieldName}`;
      addOrRemove(mode, fullFieldName, field);
    });
  };

  let incomingFields = _incomingFields.filter(Boolean);
  if (Nife.isEmpty(incomingFields))
    return currentFields;

  let rootModelName = RootModel.getModelName();
  let mode          = getInitialMode(incomingFields);
  let fields        = (mode === RESET) ? new Map() : new Map(currentFields);

  if (mode === RESET)
    mode = ADD;

  for (let i = 0, il = incomingFields.length; i < il; i++) {
    let incomingField = incomingFields[i];
    if (!incomingField)
      continue;

    if (typeof incomingField.isLiteral === 'function' && incomingField.isLiteral(incomingField)) {
      let key = incomingField.toString(this.connection);
      addOrRemove(mode, key, incomingField);
      continue;
    }

    if (typeof incomingField.isModelClass === 'function' && incomingField.isModelClass(incomingField)) {
      addOrRemoveAllModelFields(mode, incomingField);
      continue;
    }

    if (typeof incomingField.isField === 'function' && incomingField.isField(incomingField)) {
      let fullFieldName = `${incomingField.Model.getModelName()}:${incomingField.fieldName}`;
      addOrRemove(mode, fullFieldName, incomingField);
      continue;
    }

    if (!Nife.instanceOf(incomingField, 'string'))
      continue;

    if (!connection)
      throw new Error('QueryUtils::margeFields: "connection" is required, but not found.');

    let firstChar = incomingField.charAt(0);
    if (firstChar === '+') {
      mode = ADD;
      incomingField = incomingField.substring(1);
    }

    if (firstChar === '-') {
      mode = SUB;
      incomingField = incomingField.substring(1);
    }

    if (!incomingField)
      continue;

    let def = connection.parseQualifiedName(incomingField);
    if (!def.modelName)
      def.modelName = rootModelName;

    let Model = connection.getModel(def.modelName);
    if (!Model)
      throw new Error(`QueryUtils::margeFields: Model "${def.modelName}" not found.`);

    if (Nife.isEmpty(def.fieldNames)) {
      addOrRemoveAllModelFields(mode, Model);
      continue;
    }

    let modelName = Model.getModelName();
    let fieldName = def.fieldNames[0];
    let field     = connection.getField(fieldName, modelName);
    if (!field)
      throw new Error(`QueryUtils::margeFields: Field "${fieldName}" not found.`);

    let fullFieldName = `${modelName}:${fieldName}`;
    addOrRemove(mode, fullFieldName, field);
  }

  return fields;
}

module.exports = {
  parseFilterFieldAndOperator,
  generateQueryFromFilter,
  margeFields,
};
