///! import `var { Utils: { QueryUtils } } = require('mythix-orm');`
///!
///! QueryUtils provide utility functions
///! for creating and interacting with queries
///! ([QueryEngine](https://github.com/th317erd/mythix-orm/wiki/QueryEngine)).
///!
///! DocScope: QueryUtils

'use strict';

const Nife = require('nife');

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

/// Take the provided `fieldName`, which might include
/// an operator as a postfix, and return the operator
/// and `fieldName` found. If no operator postfix is
/// present, then the default `=` operator is returned.
///
/// Refer to <see>QueryUtils.generateQueryFromFilter</see> for
/// a better understanding of what this does and why it is needed.
///
/// Arguments:
///   fieldName: string
///     The field name to parse, with an optional operator postfix added.
///
/// Return: { field: string; operator: string; }
///   Return the parsed `field`, and the parsed `operator`. If no
///   operator postfix is on the field, then the default is the `=`
///   operator.
///
/// See: QueryUtils.generateQueryFromFilter
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

/// Take a "query object" and convert it into Mythix ORM query.
///
/// "query objects" are objects with a simple
/// structure and convention to build complex queries.
///
/// Fields inside these objects can have operators,
/// which are postfixed to the field name. For example,
/// you could create a filter to find a user by name
/// with the following query object:
/// `{ "firstName=": "John", "lastName!=": "Bob" }`
/// which would find all users with the first name
/// of "John", and any last name except "Bob".
///
/// `AND` and `OR` conditions are also supported. These
/// work based of the structure of the object itself.
/// If an array is used, then `OR` is in effect.
/// If an object is used, then `AND` is in effect.
/// For example, the following query object:
/// `[ { firstName: "John", lastName: "Brown" }, { firstName: "Mary", lastName: "Smith" } ]`
/// would result in the following SQL query:
/// `WHERE ((firstName = 'John' AND lastName = 'Brown') OR (firstName = 'Mary' AND lastName = 'Smith'))`,
/// finding either user John Brown, or Mary Smith.
///
/// `IN` and `NOT IN` operators are handled automatically
/// when the operator is either `=` or `!=`, and the
/// provided value is an array. For example:
/// `{ firstName: [ 'John', 'Bob', 'Mary' ] }`
/// would result in the following SQL query:
/// `WHERE firstName IN ('John', 'Bob', 'Mary')`.
///
/// Operators that can be postfixed to field names
/// in the provided `filter` object are as follows:
/// | Operator | Description |
/// | `=` | Equality operator. If an `Array` of values is provided, then this will turn into a `IN` operation in the underlying database. |
/// | `!=` | Inverse (not) equality operator. If an `Array` of values is provided, then this will turn into a `NOT IN` operation in the underlying database. |
/// | `>` | Greater than operator. |
/// | `>=` | Greater than or equal to operator. |
/// | `<` | Less than operator. |
/// | `<=` | Less than or equal to operator. |
/// | `><` | Between operator. This operator requires that the provided value be an array with exactly two elements: `[ min, max ]`. |
/// | `<>` | Inverse (not) between operator. This operator requires that the provided value be an array with exactly two elements: `[ min, max ]`. |
/// | `*` | A `LIKE` wildcard matching operator. The provided value should use `%` for "zero or more" matches, and `_` for "any single character" match. |
/// | `!*` | A `NOT LIKE` wildcard matching operator. The provided value should use `%` for "zero or more" matches, and `_` for "any single character" match. |
///
/// Note:
///   This is a simple interface to take an "object" and turn it into
///   a <see>QueryEngine</see>. It doesn't allow multiple models
///   to be defined at once (table-joins), nor other complex operations.
///   If you need more complex operations on your query, you will need
///   to manually create your query... though this method can be used
///   as a starting point.
///
/// Arguments:
///   connection: <see>Connection</see>
///     The connection used to create the <see>QueryEngine</see>.
///   Model: class <see>Model</see>
///     The model the query is being generated for. The specified
///     fields provided via the `filter` argument should all be from
///     this model.
///   filter: object | Array
///     An object or an array of objects to build a query from. Any
///     object will have all its properties `AND`ed together... whereas
///     any array will have its sub-objects `OR`ed together. i.e.
///     `[ { prop1 AND prop2 AND prop3 } OR { prop1 AND prop2 AND prop3 } ]`.
///
/// Return: <see>QueryEngine</see>
///   The new query for the `Model` provided, generated from the
///   provided `filter` argument.
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

/// Merge fields for a `PROJECT`, `ORDER`,
/// or `GROUP_BY` <see>QueryEngine</see> operation.
///
/// See <see>ModelScope.mergeFields</see> for a more detailed description
/// of what this method does and how it is used.
///
/// Arguments:
///   queryEngine: <see>QueryEngine</see>
///     The <see>QueryEngine</see> instance that the `PROJECT`,
///     `ORDER`, or `GROUP_BY` operation is being applied to.
///   currentFields: Map<string, object>
///     A map of the current fields that have been applied to the
///     given operation.
///   incomingFields: Array<string | Literal | Model | Field>
///     A list of all the incoming fields that are supplied to the
///     `PROJECT`, `ORDER`, or `GROUP_BY` operation that is being carried
///     out. This will either merge will `currentFields`, or replace
///     the `currentFields`, depending on the content of this argument.
///   extraData?: object
///     If supplied, then merge these extra properties into each field being
///     added to the list of fields. This is used for example by the `ORDER`
///     operation to define the `direction` property for each field added.
///   options?: object
///     Options for the operation. These are only used when stringifying
///     literals that are being added to the field list. See <see>LiteralBase.toString</see>
///     for more information.
///
/// See: ModelScope.mergeFields
function mergeFields(queryEngine, currentFields, _incomingFields, extraData, _options) {
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
      fields.set(key, { ...(extraData || {}), value: item });
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

  let context = queryEngine.getOperationContext();
  let incomingFields = _incomingFields.filter(Boolean);
  if (Nife.isEmpty(incomingFields))
    return new Map();

  let options       = _options || {};
  let connection    = queryEngine.getConnection();
  let RootModel     = context.rootModel;
  let rootModelName = RootModel.getModelName();
  let mode          = getInitialMode(incomingFields);
  let fields        = (mode === RESET) ? new Map() : new Map(currentFields);
  let allQueryModels;

  if (mode === RESET)
    mode = ADD;

  for (let i = 0, il = incomingFields.length; i < il; i++) {
    let incomingField = incomingFields[i];
    if (!incomingField)
      continue;

    if (typeof incomingField.isLiteral === 'function' && incomingField.isLiteral(incomingField)) {
      if (!connection)
        throw new Error('QueryUtils::mergeFields: "connection" is required, but not found.');

      let result = incomingField.toString(connection, options);
      addOrRemove(mode, result, result);
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
      throw new Error('QueryUtils::mergeFields: "connection" is required, but not found.');

    if (!incomingField)
      continue;

    let firstChar = incomingField.charAt(0);
    let currentMode = mode;

    if (firstChar === '+') {
      if (incomingField.length === 1) {
        mode = ADD;
        continue;
      }

      currentMode = ADD;
      incomingField = incomingField.substring(1);
    }

    if (firstChar === '-') {
      if (incomingField.length === 1) {
        mode = SUB;
        continue;
      }

      currentMode = SUB;
      incomingField = incomingField.substring(1);
    }

    if (incomingField.charAt(0) === '@') {
      incomingField = incomingField.substring(1);
      addOrRemove(currentMode, incomingField, incomingField);
      continue;
    }

    if (incomingField === '*') {
      if (!allQueryModels)
        allQueryModels = queryEngine.getAllModelsUsedInQuery();

      for (let i = 0, il = allQueryModels.length; i < il; i++) {
        let Model = allQueryModels[i];
        addOrRemoveAllModelFields(currentMode, Model);
      }

      continue;
    }

    let def = connection.parseQualifiedName(incomingField);
    if (!def.modelName)
      def.modelName = rootModelName;

    let Model = connection.getModel(def.modelName);
    if (!Model)
      throw new Error(`QueryUtils::mergeFields: Model "${def.modelName}" not found.`);

    if (Nife.isEmpty(def.fieldNames)) {
      addOrRemoveAllModelFields(currentMode, Model);
      continue;
    }

    let modelName = Model.getModelName();
    let fieldName = def.fieldNames[0];
    let field     = connection.getField(fieldName, modelName);
    if (!field)
      throw new Error(`QueryUtils::mergeFields: Field "${fieldName}" not found.`);

    let fullFieldName = `${modelName}:${fieldName}`;
    addOrRemove(currentMode, fullFieldName, field);
  }

  return fields;
}

module.exports = {
  parseFilterFieldAndOperator,
  generateQueryFromFilter,
  mergeFields,
};
