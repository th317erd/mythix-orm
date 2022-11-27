'use strict';

const Nife            = require('nife');
const ProxyClass      = require('../proxy-class');
const QueryEngineBase = require('./query-engine-base');
const QueryUtils      = require('../utils/query-utils');
const {
  LiteralBase,
  DistinctLiteral,
} = require('../connection/literals');

function applyOrderClause(extraData, ...args) {
  let entities = Nife.arrayFlatten(args);

  entities = Nife.toArray(entities).map((value) => {
    if (value == null)
      return;

    // Pass literals directly through
    if (LiteralBase.isLiteral(value))
      return value;

    // Is the projection a field?
    if (value.Model && value.fieldName)
      return `${value.Model.getModelName()}:${value.fieldName}`;

    if (!Nife.instanceOf(value, 'string'))
      throw new Error('QueryEngine::ModelScope::ORDER: Invalid value provided. All values provided must be strings, fields, or literals. If you want to change the sort order of a given column, add "+" (ASC) or "-" (DESC) to be beginning of the field name. Example: .ORDER("+createdAt"), or .ORDER([ "-name", "+createdAt" ]).');

    return value;
  }).filter(Boolean);

  let context = this.getOperationContext();
  let order = this.margeFields(
    context.order,
    entities,
    extraData,
    { isOrderBy: true },
  );

  this._pushOperationOntoStack({
    control:   true,
    operator:  'ORDER',
    queryProp: 'ORDER',
    value:     entities,
    order,
  });

  return this._fetchScope('model');
}

function wrapOrderClause(func) {
  const applyQueryOrder = (_order, replace) => {
    let query = this;
    let order = Nife.arrayFlatten(Nife.toArray(_order).filter(Boolean));

    let asc = order.filter((thisOrder) => {
      if (!Nife.instanceOf(thisOrder, 'string'))
        return true;

      if (thisOrder.charAt(0) === '-')
        return false;

      return true;
    });

    let desc = order.filter((thisOrder) => {
      if (!Nife.instanceOf(thisOrder, 'string'))
        return false;

      if (thisOrder.charAt(0) !== '-')
        return false;

      return true;
    });

    if (Nife.isNotEmpty(asc)) {
      asc = asc.map((thisOrder) => {
        if (!Nife.instanceOf(thisOrder, 'string'))
          return thisOrder;

        return thisOrder.replace(/^\+/, '');
      });

      // eslint-disable-next-line new-cap
      query = (replace) ? ASC(asc) : ASC('+', asc);
    }

    if (Nife.isNotEmpty(desc)) {
      desc = desc.map((thisOrder) => {
        if (!Nife.instanceOf(thisOrder, 'string'))
          return thisOrder;

        return thisOrder.replace(/^-/, '');
      });

      // eslint-disable-next-line new-cap
      query = (replace) ? DESC(desc) : DESC('+', desc);
    }

    return query;
  };

  const DESC = (...args) => {
    return applyOrderClause.call(this, { direction: '-' }, ...args);
  };

  const ASC = (...args) => {
    return applyOrderClause.call(this, { direction: '+' }, ...args);
  };

  const ADD = (...args) => {
    return applyQueryOrder(args, false);
  };

  const REPLACE = (...args) => {
    return applyQueryOrder(args, true);
  };

  func.DESC = DESC;
  func.ASC = ASC;
  func.ADD = ADD;
  func.REPLACE = REPLACE;

  return func;
}

/// `ModelScope` is the "model level" of a query.
/// It manages things like `EXISTS`, `PROJECT`,
/// `ORDER`, `INNER_JOIN`, etc...
///
/// Being a Proxy, it will "listen" for key access,
/// and lookup fields if there is a key access where
/// the key name isn't found on the class instance itself.
/// In this case, it will check the previous model specified
/// on the top of the internal "operation stack" to see if it
/// owns a field with the name of the missing key.
/// If there is a matching field on the top-most model of the stack,
/// then it will use the field found to push a <see>FieldScope</see>
/// onto the "operation stack", and then return that to the user.
/// Take the following example:
/// ```javascript
/// let queryRoot = new QueryEngine({ connection });
/// let userIDFieldScope = queryRoot.User.id;
/// ```
/// When we attempt to access the key `'id'` on
/// the `User` "model scope", we will find that no such key
/// exists on the `ModelScope` class. Now that no such property is found
/// on the `ModelScope` class, the <see>ProxyClass</see> will call the method
/// `MISSING` on the `ModelScope`, and this `MISSING` method
/// will check if the current model (`User`) has a field named `'id'`.
/// The <see>ModelScope._getField</see> method finds this field, and
/// returns it. The `ModelScope` then takes this field instance,
/// and uses it to create and return a <see>FieldScope</see>
/// using <see>QueryEngineBase._newFieldScope</see>. Now
/// that we have a <see>FieldScope</see>, we can continue
/// chaining, now using "field level" operators to act on
/// the field we just looked up.
///
/// See: QueryEngineBase
///
/// See: QueryEngine
///
/// See: FieldScope
///
/// Note:
///   `ModelScope` is a sub-part of the `QueryEngine`, and so is generally referred to
///   simply as the `QueryEngine` as a whole. This is also the case for <see>QueryEngineBase</see>,
///   and <see>FieldScope</see>, which also make up the `QueryEngine` as sub-parts,
///   and so are also often referred to simply as "the query engine".
class ModelScope extends QueryEngineBase {
  /// Get the specified field (by name) from the
  /// top-most model on the internal "operation stack".
  ///
  /// This method is called when a key can not be found
  /// on the `ModelScope` instance. The <see>ProxyClass</see>
  /// will call the `MISSING` method when the key is not
  /// found, and that method in turn calls this `_getField`
  /// method to see if the key specified was actually a field
  /// name on the most recent model in the "operation stack".
  ///
  /// Arguments:
  ///   fieldName: string
  ///     The field name of the field to fetch from the top-most model on the stack.
  ///
  /// Return: <see>Field</see>
  ///   The field found, or `undefined` if no such field is found.
  _getField(fieldName) {
    let Model = this.currentContext.Model;
    return Model.getField(fieldName);
  }

  /// Specify a <see>FieldScope</see> directly.
  ///
  /// This can be useful if you have a name collision, or if
  /// you just want to go the direct route to specify a <see>FieldScope</see>.
  ///
  /// This will find the field specified on the most recent (top-most)
  /// model on the internal "operation stack". If the field is found, then
  /// it will be used to create a new <see>FieldScope</see>, which will
  /// then be pushed onto the "operation stack", and returned to the user.
  ///
  /// Example:
  ///   // The two queries below are equivalent. The latter
  ///   // can be a good way to request a field if the field
  ///   // name being specified happens to be a name collision
  ///   // (i.e. has the same name as a QueryEngine or ModelScope
  ///   // property... such as "ORDER" or "OFFSET" for example).
  ///   let query1 = User.where.id.EQ('test');
  ///   let query2 = User.where.Field('id').EQ('test');
  ///
  /// Note:
  ///   An exception will be thrown in the specified field can not be found.
  ///
  /// Note:
  ///   This is one of the rare places in Mythix ORM where a fully qualified field
  ///   name **SHOULD NOT** be used. The reason should be clear: The model in the
  ///   operation should already be known.
  ///
  /// Arguments:
  ///   fieldName: string
  ///     A field name (NOT fully qualified), as a string. It must be a field
  ///     that exists on the model from the top-most <see>ModelScope</see> on
  ///     the internal "operation stack".
  ///
  /// Return: <see>FieldScope</see>
  ///   A new <see>FieldScope</see>, targeted to the field specified by `fieldName`.
  Field(fieldName) {
    let field = this._getField(fieldName);
    if (!field)
      throw new Error(`QueryEngine::ModelScope::Field: Requested field "${fieldName}" not found.`);

    return this._newFieldScope(field);
  }

  [ProxyClass.MISSING](target, prop) {
    if (prop === 'where' || prop === '$')
      return this._fetchScope('model');

    let field = this._getField(prop);
    if (field)
      return this._newFieldScope(field);

    let lowerScope = this._fetchScope('queryEngine');
    return lowerScope[prop];
  }

  /// Merge fields together, replacing, adding, or subtracting from
  /// the field set.
  ///
  /// This method is used by <see>ModelScope.ORDER</see>, <see>ModelScope.GROUP_BY</see>,
  /// and <see>ModelScope.PROJECT</see>. It works be replacing, adding, or subtracting
  /// from the current set of fields defined by any of these operations.
  ///
  /// If the first field encountered in the provided `incomingFields` isn't prefixed with
  /// a `+` or `-`, and if a `+` or `-` operation doesn't come before the encountered field, then
  /// the operation is considered a "replace" operation. For example, in `query.ORDER('User:id')`
  /// we are "replacing" the order clause with only a single field: `'User:id'`. If however,
  /// we did `query.ORDER('+User:id')` then the `'User:id'` field would be *added* to any current
  /// order clause in the query. We could also prefix the entire operation with a single `'+'`
  /// string, and then all following fields in the list would be in "add" mode. For example, the
  /// following two operations are equivalent: `query.ORDER('+', 'User:firstName', 'User:lastName')`, and
  /// `query.ORDER('+User:firstName', '+User:lastName')`. The subtraction operator `'-'` works the
  /// same, but in reverse, specifying that we want to remove fields instead of add them:
  /// `query.ORDER('-', 'User:firstName', 'User:lastName')`, and `query.ORDER('-User:firstName', '-User:lastName')`.
  /// It is also possible to mix the two together: `query.ORDER('+', 'User:firstName', '-', 'User:lastName')`, or
  /// `query.ORDER('+User:firstName', '-User:lastName')`.
  ///
  /// There is one special operator, `'*'`. This operator will add *all* fields from all models that
  /// are currently in use on the query. For example, if we did: `Role.where.userID(User.where.id).PROJECT('*')`,
  /// this would add *all* `Role` and `User` fields to the projection. Replace, add, and subtract rules still
  /// apply with a wildcard. So in our example above we are "replacing" the projection, since no `+` or `-` was
  /// encountered before the operation. If you instead wanted to *add* all model fields to the projection,
  /// you should instead do: `Role.where.userID(User.where.id).PROJECT('+*')` or `Role.where.userID(User.where.id).PROJECT('+', '*')`.
  /// Generally, it probably wouldn't matter much, since you are replacing "every field" anyhow... but it could
  /// matter if for example you had previously projected a literal... in which case it would be replaced
  /// with all model fields. Subtraction rules also apply to wildcard selectors.
  ///
  /// The only truly important thing to remember here is that if **no** operation is specified (add or subtract),
  /// then the default operation is "replace", so the field set will be replaced entirely for the operation
  /// in question.
  ///
  /// There is one exception to this behavior, for `ORDER.ADD` and `ORDER.REPLACE` **only**. These two operations
  /// allow the field sort direction itself to be defined with `+` and `-`... so a
  /// `query.ORDER.ADD('+User:firstName', '-User:lastName')` is not requesting that we add the `'User:firstName'`
  /// field, and subtract the `'User:lastName'` field... but instead is specifying that we want
  /// `'User:firstName'` in `ASC` sort order, whereas we want `'User:lastName'` in `DESC` sort order.
  /// `ORDER.ADD` and `ORDER.REPLACE` are the only exceptions to the normal logic defined here. There are
  /// two methods so that a user can add to the field set, or replace the field set. A `SUB` (subtract)
  /// operation is not needed, because that can be done with `ORDER` anyway, i.e.
  /// `query.ORDER.ADD('+User:firstName', '+User:lastName').ORDER('-Role')`, which would "add" the
  /// `'User:firstName'` and `'User:lastName'` fields, simultaneously specifying their short direction,
  /// and then `ORDER('-Role')` is subtracting every field from the `Role` model.
  ///
  /// Note:
  ///   A solo `+` or `-` operation is a no-op: `query.ORDER('+')` and `query.ORDER('-')`
  ///   will do nothing at all, and will not modify the operation in the slightest.
  ///
  /// Note:
  ///   This method supports a short-cut for literals. You can specify a string prefixed
  ///   with an `@` symbol to specify a literal. For example, `query.PROJECT('@COUNT("users"."firstName") AS "count"')`.
  ///   Doing so will create a new <see>Literal</see> instance with the value provided. This
  ///   is not the best way to provide a literal however, because the <see>Literal</see> class
  ///   defines a "raw" literal, whereas the typed literal classes (such as <see>CountLiteral</see>)
  ///   actually store the field they are operating on, and can report that field back to the engine.
  ///
  /// Arguments:
  ///   currentFields: Map<string, { value: Field | Literal | string; direction?: '+' | '-'; ... }>
  ///     A map of the current fields for the operation. This is the value from the operation
  ///     context itself. For example, the `ORDER` method provides this argument via:
  ///     `this.getOperationContext().order`, providing any previous "order" operation
  ///     as the current fields.
  ///   incomingFields: Array<<see>Model</see> | <see>Field</see> | string | literal | '+' | '-' | '*'>
  ///     Incoming fields to replace, add, or subtract from the operation in question.
  ///     If no fields at all are provided, then this will reset/nullify the operation. For example,
  ///     an `ORDER()` would clear any `ORDER BY` clause entirely. If a model is provided, either
  ///     as a name, i.e. `'User'`, or as the raw model, i.e. `User`, then *all the fields* from the
  ///     specified model will be added or subtracted. A field can be specified as a fully qualified
  ///     field name, a raw <see>Field</see> instance, or just the field name itself, i.e. `'firstName'`.
  ///     If no model is specified (i.e. a non-fully-qualified field name), then the engine will attempt
  ///     to fetch the field specified from the root model of the query. Literals can be used for all
  ///     supported operations, `ORDER`, `GROUP_BY`, and `PROJECT`, and they also follow the replace, add
  ///     and subtract rules of the engine.
  ///   extraData?: object
  ///     Extra data to apply to the operation. For example, the `ORDER` operation applies a `direction` property
  ///     to each field in the map. The field map is a `Map` instance, where each key is the fully qualified field
  ///     name, or expanded literal value. Each value on the map is an object, containing at least a `value` property
  ///     that is the field or literal specified. This object can also contain any ancillary operation info, such
  ///     as in the case of the `ORDER` operation, which will also add a `direction` property to each field in the
  ///     map to specify the field's (or literal's) sort order.
  ///   options?: object
  ///     An options object to pass off to `Literal.toString` when expanding literals for use as `Map` keys.
  ///
  /// Return: Map<string, { value: Field | Literal | string; direction?: '+' | '-'; ... }>
  ///   Return the new field set. A `Map` will always be returned, but it is possible
  ///   for the `Map` to be empty.
  margeFields(currentFields, incomingFields, extraData, options) {
    return QueryUtils.margeFields(this, currentFields, incomingFields, extraData, options);
  }

  /// Invert the logic of the following operator.
  ///
  /// This method does not need to be called.
  ///
  /// Unlike `AND` and `OR` operators, this is not a permanent toggle.
  /// As soon as a following operator is encountered, the `NOT` operation
  /// will be "turned off". `NOT` will invert any operation, so for example
  /// if you did a `User.where.id.NOT.EQ('test')` then this is the same as
  /// `User.where.id.NEQ('test')`--selecting everything NOT EQUAL TO `'test'`.
  /// `NOT` can also be used in combination with `EXISTS`, `ANY`, `ALL`, `IN`,
  /// etc... inverting any operator following `NOT`.
  ///
  /// Note:
  ///   `NOT` is only ever used once, and then it is toggled back off. For example,
  ///   if we did: `User.where.id.NOT.EQ('test').AND.lastName.EQ('Smith')`, then
  ///   we would have a query where `id != 'test' AND lastName = 'Smith'`. As you
  ///   can see, the `NOT` doesn't apply to the second `lastName` operator, as it
  ///   was turned off as soon as the first `EQ` operator was encountered on the `id`
  ///   field.
  ///
  /// SyntaxType: FunctionDeclaration
  NOT = ProxyClass.autoCall(function() {
    this._pushOperationOntoStack({ logical: true, operator: 'NOT', queryProp: 'NOT', not: !this.currentContext.not });
    return this._fetchScope('model');
  });

  /// Logical `AND`, for ANDing operations together.
  ///
  /// This method does not need to be called, but it can
  /// be called if desired.
  ///
  /// This is a "toggle", so as soon as it is used,
  /// it will continue to be "active" for all following
  /// operations. In Mythix ORM you don't need to specify
  /// `AND` or `OR` unless you actually need them. By default
  /// `AND` is enabled for all queries. So for example you
  /// can do `User.where.id.EQ('test').firstName.EQ('John').lastName.EQ('Smith')`,
  /// which is exactly the same as `User.where.id.EQ('test').AND.firstName.EQ('John').AND.lastName.EQ('Smith')`.
  ///
  /// `AND` can also be called to group conditions. For example, you could create
  /// the following query: `User.where.id.EQ('test').AND(User.where.lastName.EQ('John').OR.lastName.EQ('Brown'))`
  /// to create the following SQL query: `WHERE id = 'test' AND (lastName = 'John' OR lastName = 'Brown')`.
  ///
  /// SyntaxType: FunctionDeclaration
  ///
  /// Arguments:
  ///   query?: <see>QueryEngine</see>
  ///     An optional query, which if provided, will create a "condition group" as a result.
  AND = ProxyClass.autoCall(function(value) {
    this._pushOperationOntoStack({ logical: true, operator: 'AND', queryProp: 'AND', and: true, or: false, not: false, value });
    return this._fetchScope('model');
  });

  /// Logical `OR`, for ORing operations together.
  ///
  /// This method does not need to be called, but it can
  /// be called if desired.
  ///
  /// This is a "toggle", so as soon as it is used,
  /// it will continue to be "active" for all following
  /// operations. In Mythix ORM you don't need to specify
  /// `AND` or `OR` unless you actually need them. By default
  /// `AND` is enabled for all queries. For example you
  /// can do `User.where.id.EQ('test').OR.firstName.EQ('John').lastName.EQ('Smith')`,
  /// which is exactly the same as `User.where.id.EQ('test').OR.firstName.EQ('John').OR.lastName.EQ('Smith')`.
  ///
  /// `OR` can also be called to group conditions. For example, you could create
  /// the following query: `User.where.id.EQ('test').OR(User.where.firstName.EQ('John').OR.lastName.EQ('Brown'))`
  /// to create the following SQL query: `WHERE id = 'test' OR (firstName = 'John' AND lastName = 'Brown')`.
  ///
  /// SyntaxType: FunctionDeclaration
  ///
  /// Arguments:
  ///   query?: <see>QueryEngine</see>
  ///     An optional query, which if provided, will create a "condition group" as a result.
  OR = ProxyClass.autoCall(function(value) {
    this._pushOperationOntoStack({ logical: true, operator: 'OR', queryProp: 'OR', and: false, or: true, not: false, value });
    return this._fetchScope('model');
  });

  /// Apply a `LIMIT` clause to the query.
  ///
  /// Any valid positive integer is acceptable, as well as `Infinity`.
  /// If `Infinity` is used, then the `LIMIT` will either turn into its
  /// max possible value (in the billions... depending on the underlying database),
  /// or it will be stripped from the query entirely.
  /// `NaN`, or anything that isn't a valid positive integer will throw an error.
  ///
  /// Note:
  ///   Positive floating point numbers are rounded with `Math.round`.
  ///
  /// Arguments:
  ///   limit: number
  ///     The limit to apply to the query.
  LIMIT(_value) {
    let value = _value;
    if (typeof value !== 'number' || isNaN(value) || value < 0)
      throw new Error('QueryEngine::ModelScope::LIMIT: Value provided must be a valid positive number, or Infinity.');

    value = Math.round(value);
    this._pushOperationOntoStack({ control: true, operator: 'LIMIT', queryProp: 'LIMIT', value, limit: value });

    return this._fetchScope('model');
  }

  /// Apply an `OFFSET` clause to the query.
  ///
  /// Any valid positive integer is acceptable. `Infinity`, `NaN`,
  /// or anything that isn't a valid positive integer will throw an error.
  ///
  /// Note:
  ///   Positive floating point numbers are rounded with `Math.round`.
  ///
  /// Arguments:
  ///   offset: number
  ///     The offset to apply to the query.
  OFFSET(_value) {
    let value = _value;
    if (typeof value !== 'number' || !isFinite(value) || value < 0)
      throw new Error('QueryEngine::ModelScope::OFFSET: Value provided must be a valid positive number.');

    value = Math.round(value);
    this._pushOperationOntoStack({ control: true, operator: 'OFFSET', queryProp: 'OFFSET', value, offset: value });

    return this._fetchScope('model');
  }

  /// Apply an `ORDER BY` clause to the query, to sort
  /// the results on the fields specified, in either
  /// `ASC` or `DESC` order.
  ///
  /// There are five variants to this method, `ORDER.ASC`,
  /// `ORDER.DESC`, `ORDER.ADD`, `ORDER.REPLACE`, and `ORDER` (which is an alias for `ORDER.ASC`), .
  /// 1) `ORDER` - Alias for `ORDER.ASC`.
  /// 2) `ORDER.ASC` - Follow the rules of <see>ModelScope.margeFields</see>. Each field/literal added is in `ASC` order.
  /// 3) `ORDER.DESC` - Follow the rules of <see>ModelScope.margeFields</see>. Each field/literal added is in `DESC` order.
  /// 4) `ORDER.ADD` - **DO NOT** follow the rules of <see>ModelScope.margeFields</see>, and instead **add** all fields specified, with their sort order being specified instead by the `+` or `-` prefixes on each field.
  /// 5) `ORDER.REPLACE` - **DO NOT** follow the rules of <see>ModelScope.margeFields</see>, and instead **replace** the operation fields to the fields specified, with their sort order being specified instead by the `+` or `-` prefixes on each field.
  ///
  /// See <see>ModelScope.margeFields</see> to better understand how this method works.
  ///
  /// SyntaxType: FunctionDeclaration
  ///
  /// Note:
  ///   This method will flatten all provided arguments into a one dimensional array,
  ///   so you can provide arrays or deeply nested arrays for the fields specified.
  ///
  /// Arguments:
  ///   ...args: Array<<see>Field</see> | <see>LiteralBase</see> | string | '+' | '-' | '*'>
  ///     New "fields" to supply to `ORDER`, replacing, adding, or subtracting the
  ///     specified fields from the `ORDER` operation.
  ORDER = wrapOrderClause.call(this, (...args) => {
    return applyOrderClause.call(this, { direction: '+' }, ...args);
  });

  /// Apply a `GROUP BY` clause to the query.
  ///
  /// See <see>ModelScope.margeFields</see> to better understand how this method works.
  ///
  /// Note:
  ///   This method will flatten all provided arguments into a one dimensional array,
  ///   so you can provide arrays or deeply nested arrays for the fields specified.
  ///
  /// Arguments:
  ///   ...args: Array<<see>Field</see> | <see>LiteralBase</see> | string | '+' | '-' | '*'>
  ///     New "fields" to supply to `GROUP BY`, replacing, adding, or subtracting the
  ///     specified fields from the `GROUP_BY` operation.
  GROUP_BY(...args) {
    let entities = Nife.arrayFlatten(args);

    entities = Nife.toArray(entities).map((value) => {
      if (value == null)
        return;

      // Pass literals directly through
      if (LiteralBase.isLiteral(value))
        return value;

      // Is the projection a field?
      if (value.Model && value.fieldName)
        return `${value.Model.getModelName()}:${value.fieldName}`;

      if (!Nife.instanceOf(value, 'string'))
        throw new Error('QueryEngine::ModelScope::GROUP_BY: Invalid value provided. All values provided must be strings, fields, or literals. If you want to change the sort order of a given column, add "+" (ASC) or "-" (DESC) to be beginning of the field name. Example: .ORDER("+createdAt"), or .ORDER([ "-name", "+createdAt" ]).');

      return value;
    }).filter(Boolean);

    let context = this.getOperationContext();
    let groupBy = this.margeFields(
      context.groupBy,
      entities,
      {},
      { isGroupBy: true },
    );

    this._pushOperationOntoStack({
      control:   true,
      operator:  'GROUP_BY',
      queryProp: 'GROUP_BY',
      value:     entities,
      groupBy,
    });

    return this._fetchScope('model');
  }

  /// Apply a `HAVING` clause to the query.
  ///
  /// This will only be applied in the underlying database query
  /// if it is also paired with a <see>ModelScope.GROUP_BY</see> operation,
  /// otherwise it will be ignored.
  ///
  /// Example:
  ///   let adultCountByAge = await User.where
  ///     .GROUP_BY('User:age')
  ///     .HAVING(User.where.age.GTE(18))
  ///     .PROJECT('User:age', new CountLiteral('User:age', { as: 'count' }))
  ///     .all();
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The query to use to apply conditions to the `HAVING` clause.
  HAVING(query) {
    this._pushOperationOntoStack({ control: true, operator: 'HAVING', queryProp: 'HAVING', value: query, having: query });
    return this._fetchScope('model');
  }

  /// Check if any rows match the query provided.
  ///
  /// This is on the <see>ModelScope</see> itself because
  /// it is never paired with a field, and is an operator that
  /// stands all on its own.
  ///
  /// It can be used to check the existence of any value in the
  /// database. For example, you might want to query on users,
  /// but only if those users have an "admin" role:
  /// `await User.where.email.EQ('test@example.com').EXISTS(Role.where.name.EQ("admin").userID.EQ(new FieldLiteral('User:id')))`
  ///
  /// Note:
  ///   You can execute a `NOT EXISTS` operation simply by prefixing `EXISTS` with a `.NOT`
  ///   operation, for example `query.NOT.EXISTS(...)`.
  ///
  /// Arguments:
  ///   query: <see>QueryEngine</see>
  ///     The sub-query to execute to check for existence. Use a <see>FieldLiteral</see>
  ///     to pair it with the primary query.
  EXISTS(_query) {
    let query         = _query;
    let queryContext  = (QueryEngineBase.isQuery(query)) ? query.getOperationContext() : null;
    if (!queryContext || !queryContext.hasCondition)
      throw new Error('QueryEngine::ModelScope::EXISTS: Provided value must be a query with conditions.');

    if (!queryContext.projection) {
      let Model   = queryContext.Model;
      let pkField = Model.getPrimaryKeyField();

      if (pkField)
        query = query.clone().PROJECT(pkField);
      else
        throw new Error('QueryEngine::ModelScope::EXISTS: Provided query must have only a single field projected.');
    }

    this._pushOperationOntoStack({
      condition:       true,
      operator:        'EXISTS',
      inverseOperator: 'NOT EXISTS',
      queryProp:       'EXISTS',
      value:           query,
      having:          query,
      hasCondition:    true,
    });

    return this._fetchScope('model');
  }

  /// Replace, add to, or subtract from the projection of the query.
  ///
  /// See <see>ModelScope.margeFields</see> to better understand how this method works.
  ///
  /// Note:
  ///   This method will flatten all provided arguments into a one dimensional array,
  ///   so you can provide arrays or deeply nested arrays for the fields specified.
  ///
  /// Note:
  ///   Mythix ORM collects and returns models (or partial models) based on the projection.
  ///   By default only the "root model" of a query will be projected and converted into
  ///   model instances. If you want to fetch more than just the root model while querying,
  ///   make sure to project the models (or fields from other models) that you want to
  ///   collect into model instances during load.
  ///
  /// Arguments:
  ///   ...args: Array<<see>Field</see> | <see>LiteralBase</see> | string | '+' | '-' | '*'>
  ///     New "fields" to supply to the projection, replacing, adding, or subtracting the
  ///     specified fields from the `PROJECT` operation.
  PROJECT(...args) {
    let entities = Nife.arrayFlatten(args);

    entities = Nife.toArray(entities).map((value) => {
      if (value == null)
        return;

      // Pass literals directly through
      if (LiteralBase.isLiteral(value))
        return value;

      // Is the projection a model?
      if (value._isMythixModel)
        return value;

      // Is the projection a field?
      if (value.Model && value.fieldName)
        return value;

      if (!Nife.instanceOf(value, 'string')) {
        console.log(entities);
        throw new TypeError(`QueryEngine::ModelScope::PROJECT: Invalid value provided [${value.toString()}]. All values provided must be strings, fields, models, or literals.`);
      }

      return value;
    }).filter(Boolean);

    let context = this.getOperationContext();
    let projection = this.margeFields(
      context.projection,
      entities,
      {},
      { isProjection: true },
    );

    this._pushOperationOntoStack({
      control:   true,
      operator:  'PROJECT',
      queryProp: 'PROJECT',
      value:     entities,
      projection,
    });

    return this._fetchScope('model');
  }

  /// Apply a DISTINCT clause to the query.
  ///
  /// This method does not need to be called, but it can
  /// be called if desired.
  ///
  /// A field is required for this operation to keep the interface
  /// consistent across all database drivers... however, the field
  /// specified may not actually be used, depending on database support,
  /// or other operations being carried out in the query.
  ///
  /// If the underlying database supports it (i.e. PostgreSQL), then
  /// this will turn into a `DISTINCT ON(field)` operation. If the
  /// database (or operation being carried out), doesn't support `DISTINCT ON`,
  /// then it will fallback to just a `DISTINCT` across the entire projection.
  /// Any `DISTINCT` operation applied to the query will always be the very
  /// first part of the projection, regardless of whatever else is projected.
  ///
  /// This method is *optionally* callable. If not called, then the primary key
  /// of the model specified will be used (if available). If no primary key exists
  /// on the specified model, then it will fallback to a raw `DISTINCT` clause
  /// prefixing the projection. For example: `User.where.DISTINCT.lastName.EQ('Smith')`
  /// would be distinct on the primary key of `User` (which would be `id` in our example).
  /// If instead we call the operator, then we can supply our own field or literal:
  /// `User.where.DISTINCT('User:firstName').lastName.EQ('Smith')`.
  ///
  /// To turn off any previous `DISTINCT` applied, pass the argument `false` to
  /// `DISTINCT`: `User.where.DISTINCT.lastName.EQ('Smith').DISTINCT(false)`.
  /// In this example the resulting query would have no `DISTINCT` clause at all,
  /// since it was disabled when `DISTINCT(false)` was called.
  ///
  /// `DISTINCT` changes the nature of the query, and might change how it is carried out
  /// by the underlying database driver. For example, a distinct combined with a `count`
  /// call would modify the `count` query: `await User.where.DISTINCT('User:id').count()` would
  /// actually turn into the following SQL: `SELECT COUNT(DISTINCT "users"."id")`. This
  /// is just one example however... just know that the underlying database driver might
  /// alter the query, or take a completely different path to query if a `DISTINCT` operation
  /// is in-play.
  ///
  /// Note:
  ///   The support for a `DISTINCT` clause changes wildly across databases. Some might support
  ///   `ON` for a specific column, some may not. Some databases might force a certain `ORDER`
  ///   when using `DISTINCT`, some may not... `DISTINCT` may be supported in sub-queries, or
  ///   it may not... or might require the query be written differently. Mythix ORM does its best
  ///   to make a "standard" out of this very non-standard situation (does any SQL actually follow a standard?),
  ///   but just be aware that `DISTINCT` might bite you if you are changing database drivers to a
  ///   database that behaves differently.
  ///
  /// SyntaxType: FunctionDeclaration
  ///
  /// Arguments:
  ///   field: <see>Field</see> | <see>LiteralBase</see> | string | false = Model.getPrimaryKeyField()
  ///     The field or literal to be `DISTINCT ON` (if the database supports it). If `false`
  ///     is specified, then any previous `DISTINCT` operation is cleared.
  DISTINCT = ProxyClass.autoCall(function(_fullyQualifiedName) {
    let fullyQualifiedName  = _fullyQualifiedName;
    let currentQuery        = this;
    let distinctValue       = fullyQualifiedName;
    let context             = this.getOperationContext();

    if (arguments.length === 0) {
      let rootModel = context.rootModel;
      if (rootModel) {
        let pkFieldName = rootModel.getPrimaryKeyFieldName();
        if (pkFieldName)
          distinctValue = new DistinctLiteral(`${rootModel.getModelName()}:${pkFieldName}`);
      }

      if (!distinctValue)
        distinctValue = new DistinctLiteral();
    } else if (Nife.instanceOf(fullyQualifiedName, 'string')) {
      if (fullyQualifiedName.indexOf(':') < 0)
        fullyQualifiedName = `${context.rootModel.getModelName()}:${fullyQualifiedName}`;

      distinctValue = new DistinctLiteral(fullyQualifiedName);
    } else if (typeof fullyQualifiedName.isField === 'function' && fullyQualifiedName.isField(fullyQualifiedName)) {
      distinctValue = new DistinctLiteral(`${fullyQualifiedName.Model.getModelName()}:${fullyQualifiedName.fieldName}`);
    } else if (LiteralBase.isLiteral(fullyQualifiedName)) {
      distinctValue = new DistinctLiteral(fullyQualifiedName);
    } else if (!fullyQualifiedName) {
      distinctValue = null;
    } else {
      throw new TypeError(`QueryEngine::ModelScope::DISTINCT: Invalid value provided [${(fullyQualifiedName) ? fullyQualifiedName.toString() : fullyQualifiedName}]. All values provided must be strings, fields, or literals.`);
    }

    currentQuery._pushOperationOntoStack({ sqlFunc: true, operator: 'DISTINCT', queryProp: 'DISTINCT', value: distinctValue, distinct: distinctValue });
    return this._fetchScope('model');
  });

  /// Specify an `INNER JOIN` operation for joining tables.
  ///
  /// This method does not need to be called, but it can
  /// be called if desired.
  ///
  /// All Mythix ORM "join" operations should come immediately
  /// before a table join. For example: `User.where.INNER_JOIN.id.EQ(Role.where.userID)`.
  /// They are "toggles", and so will remain "on" once used. In short, if
  /// you specify an `INNER_JOIN` at the very beginning of your query, then
  /// **ALL** table joins in the query will be `INNER JOIN`, unless you specify
  /// other join types.
  ///
  /// Note:
  ///   `INNER_JOIN` is the default table join type in Mythix ORM if no other table
  ///   join type is specified in the query.
  ///
  /// SyntaxType: FunctionDeclaration
  INNER_JOIN = ProxyClass.autoCall(function() {
    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'INNER_JOIN', value: 'inner', joinType: 'inner', joinOuter: false });
    return this._fetchScope('model');
  });

  /// Specify a `LEFT JOIN` operation for joining tables.
  ///
  /// This method does not need to be called, but it can
  /// be called if desired.
  ///
  /// All Mythix ORM "join" operations should come immediately
  /// before a table join. For example: `User.where.LEFT_JOIN.id.EQ(Role.where.userID)`.
  /// They are "toggles", and so will remain "on" once used. In short, if
  /// you specify a `LEFT_JOIN` at the very beginning of your query, then
  /// **ALL** table joins in the query will be `LEFT JOIN`, unless you specify
  /// other join types, i.e. `User.where.LEFT_JOIN.id.EQ(Role.where.userID).AND.INNER_JOIN.id.EQ(Organization.where.userID)`.
  ///
  /// SyntaxType: FunctionDeclaration
  ///
  /// Arguments:
  ///   outerJoin: boolean = false
  ///     If `true`, then this will result in a `LEFT OUTER JOIN` if the database supports it.
  LEFT_JOIN = ProxyClass.autoCall(function(outerJoin) {
    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'LEFT_JOIN', value: 'left', joinType: 'left', joinOuter: !!outerJoin });
    return this._fetchScope('model');
  });

  /// Specify a `RIGHT JOIN` operation for joining tables.
  ///
  /// This method does not need to be called, but it can
  /// be called if desired.
  ///
  /// All Mythix ORM "join" operations should come immediately
  /// before a table join. For example: `User.where.RIGHT_JOIN.id.EQ(Role.where.userID)`.
  /// They are "toggles", and so will remain "on" once used. In short, if
  /// you specify a `RIGHT_JOIN` at the very beginning of your query, then
  /// **ALL** table joins in the query will be `RIGHT JOIN`, unless you specify
  /// other join types, i.e. `User.where.RIGHT_JOIN.id.EQ(Role.where.userID).AND.INNER_JOIN.id.EQ(Organization.where.userID)`.
  ///
  /// SyntaxType: FunctionDeclaration
  ///
  /// Arguments:
  ///   outerJoin: boolean = false
  ///     If `true`, then this will result in a `RIGHT OUTER JOIN` if the database supports it.
  RIGHT_JOIN = ProxyClass.autoCall(function(outerJoin) {
    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'RIGHT_JOIN', value: 'right', joinType: 'right', joinOuter: !!outerJoin });
    return this._fetchScope('model');
  });

  /// Specify a `FULL JOIN` operation for joining tables.
  ///
  /// This method does not need to be called, but it can
  /// be called if desired.
  ///
  /// All Mythix ORM "join" operations should come immediately
  /// before a table join. For example: `User.where.FULL_JOIN.id.EQ(Role.where.userID)`.
  /// They are "toggles", and so will remain "on" once used. In short, if
  /// you specify a `FULL_JOIN` at the very beginning of your query, then
  /// **ALL** table joins in the query will be `FULL JOIN`, unless you specify
  /// other join types, i.e. `User.where.FULL_JOIN.id.EQ(Role.where.userID).AND.INNER_JOIN.id.EQ(Organization.where.userID)`.
  ///
  /// SyntaxType: FunctionDeclaration
  ///
  /// Arguments:
  ///   outerJoin: boolean = false
  ///     If `true`, then this will result in a `FULL OUTER JOIN` if the database supports it.
  FULL_JOIN = ProxyClass.autoCall(function(outerJoin) {
    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'FULL_JOIN', value: 'full', joinType: 'full', joinOuter: !!outerJoin });
    return this._fetchScope('model');
  });

  /// Specify a `CROSS JOIN` operation for joining tables.
  ///
  /// This method does not need to be called.
  ///
  /// All Mythix ORM "join" operations should come immediately
  /// before a table join. For example: `User.where.CROSS_JOIN.id.EQ(Role.where.userID)`.
  /// They are "toggles", and so will remain "on" once used. In short, if
  /// you specify a `CROSS_JOIN` at the very beginning of your query, then
  /// **ALL** table joins in the query will be `CROSS JOIN`, unless you specify
  /// other join types, i.e. `User.where.CROSS_JOIN.id.EQ(Role.where.userID).AND.INNER_JOIN.id.EQ(Organization.where.userID)`.
  ///
  /// SyntaxType: FunctionDeclaration
  CROSS_JOIN = ProxyClass.autoCall(function() {
    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'CROSS_JOIN', value: 'cross', joinType: 'cross', joinOuter: false });
    return this._fetchScope('model');
  });

  /// Specify a custom join operation for the underlying database.
  /// It is recommended that you use a literal, though that isn't required.
  ///
  /// All Mythix ORM "join" operations should come immediately
  /// before a table join. For example: `User.where.JOIN('RIGHT INNER JOIN').id.EQ(Role.where.userID)`.
  /// They are "toggles", and so will remain "on" once used.
  ///
  /// Note:
  ///   This method should be avoided if at all possible, since it will
  ///   likely be database specific, making your code not as portable to
  ///   another database driver.
  ///
  /// Note:
  ///   Is there a standard join type that Mythix ORM missed, that should be
  ///   supported across most or all databases? If so, let us know by opening
  ///   an Issue or PR on our GitHub page. Thank you!
  ///
  /// Arguments:
  ///   type: <see>Literal</see> | string
  ///     The join type to use in the underlying database (a literal value).
  JOIN(type) {
    if (!(Nife.instanceOf(type, 'string') || LiteralBase.isLiteral(type)))
      throw new Error('QueryEngine::ModelScope::JOIN: Invalid value provided. Value must be a valid string or Literal specifying JOIN type.');

    this._pushOperationOntoStack({ join: true, operator: 'JOIN', queryProp: 'JOIN', value: type, joinType: type, joinOuter: false });
    return this._fetchScope('model');
  }
}

module.exports = ModelScope;
