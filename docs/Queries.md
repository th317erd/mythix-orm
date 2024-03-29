Mythix ORM uses an advanced (but simple) custom query engine that is unique. The query engine uses and requires JavaScript [Proxy](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy) support. Because of this, Mythix ORM won't run on Node versions less than `6.0`. If Mythix ORM is being used in the browser, then it will require browser support for [Proxy](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Proxy).

## Getting started

Before we start discussing how the query engine works, let's first jump into some examples so we can see it in action.

Example 1: Query a user by ID

```javascript
let user = await User.where.id.EQ(userID).first();
```

Example 2: Query a user's roles by joining tables

```javascript
let roles = await Role.where.userID.EQ(User.where.id).all();
```

Example 3: Query a user's roles with a sub-query

```javascript
let roles = await Role.where.userID.EQ(User.where.firstName.EQ('Bob').lastName.EQ('Brown').PROJECT('id')).all();
```

## Terminology

In this document we will refer to some common terms. A brief overview of terminology here may assist the reader:

  1. "root model" (sometimes also referred to as the "target model") refers to the first model used in a query. For example, for the following query "`User.where`" the root model is "`User`".
  2. "query engine" the query engine class and related family classes (<see>ModelScope</see>, <see>FieldScope</see>, <see>QueryEngine</see>, and <see>QueryEngineBase</see>)
  3. "operation" is any single call to a query engine method that results in a new "operation frame" being pushed onto the internal operation stack. `Model`, `Field`, `DISTINCT`, `EQ`, `PROJECTION`, `LEFT_JOIN`, etc... are all "operations" that push a "frame" onto the "operation stack".
  4. "operation stack" is the term used to refer to the internal operation stack that the query engine
  adds onto with each operation carried out. Each "frame" on this stack is referred to as an "operation frame",
  or more loosely, an "operation context". "operation context"--while not incorrect while referring to an "operation frame"--has a little more to it, and actually spans the "frame" and all lower frames simultaneously.
  5. "operation context" is a "frame" on the operation stack, plus all frames below the given frame. The "operation stack" is built by pushing "frames" onto the stack. There is a catch though. All "frames" on the stack have their
  `prototype` set to the previous frame on the stack. This is where the term "operation context" comes in. Accessing
  the "operation context" essentially means accessing a frame (usually the top-most frame), but since each frame "inherits" from the previous frame in the stack, the "operation context" has access to all properties across all frames beneath it, making for an entire "context".
  6. "generator" or "query generator" refers to the underlying connection generator interface. This "query generator" takes a query engine and turns it into a query for the underlying database engine.
  7. "fully qualified field name" is a field that defines both the model and the field. For example: `User:id`, or `User:firstName`. This special pattern can be used nearly everywhere a field can be specified, and is sometimes required. It isn't always required to use fully qualified syntax. Short-hand syntax is just the field name, for example: `firstName`. If there is only one model in the query then Mythix ORM generally won't complain, and will be able to find the field name just fine. However, if there is more than one model in the query, Mythix ORM will require that you use fully qualified syntax for fields.

## How the Query Engine works

The Mythix ORM Query Engine works by returning a Proxy class instance when a query engine is first constructed.

"What the heck is a 'Proxy class'?" I hear you already asking...

In JavaScript, any non-primitive instance of an object can be returned from a class constructor. Whatever instance is returned from a class constructor will be the instantiated instance returned by the `new` operator.

Take for example the following:

```javascript
class TestClass {
  constructor() {
    return { hello: 'world' };
  }
}

console.log(new TestClass());
// output: { "hello": "world" }
```

As you can see, instead of an instance of `TestClass`, what we get is actually the instance of the object we created in the constructor: `{ "hello": "world" }`.

With this in mind, we can create "Proxy classes". This works by wrapping `this` in a proxy:

```javascript
class QueryEngine {
  constructor() {
    return new Proxy(this, { ...handlers });
  }
}
```

So now, when we invoke `new QueryEngine()`, we get back an instance of `QueryEngine`, but wrapped in a `Proxy`. What we *do* with this proxy is where the magic happens.

Since we return a proxy instead of just the `QueryEngine` class instance, we can intercept all attribute get and sets. This is why the following works:

```javascript
await User.where.id.EQ(userID).first();
```

What is happening here is the following:

  1. `User.where` is a `getter` attribute that returns a new `QueryEngine` proxy class.
  2. When we attempt to access the `id` property of the query engine proxy, the proxy looks inside the query engine and sees that no such property exists. So it then looks up to see if the `User` model has an `id` field. Since the `User` model *does* have an `id` field, it pushes that operation (a field operation) onto the query engine's internal stack.
  3. The `id` property fetch itself returns the query engine again... or rather, a modified scope of the query engine, called a `FieldScope`.
  4. This `FieldScope`--that is itself a query engine proxy--knows that certain operations can be done on fields, including `.EQ`. So when we ask for the property `.EQ`, the `FieldScope` query engine proxy returns a "conditional method", that we call to provide a value. In our case, we call it like `.EQ(userID)`. When called, the method pushes a conditional "equals" operation onto the query engine stack, along with the value `userID`.
  5. Finally, the query engine itself defines common useful methods for interacting with a query, such as `first`, `last`, `all`, `pluck`, `count`, etc... So when we fetch the `first` property from the proxy, the proxy sees that a `first` method/key does indeed exist on the `QueryEngine` instance, and simply returns that method. When this method is called, it supplies itself (the query) to the lower-level `connection` method `select` to select some rows from the database, using the query that was just constructed.

Okay! Whoa! I just choked on my tea! That was a lot bro...

Right... information overload. Okay, let me take a step back and explain in a little more detail.

There are actually three primary classes at work in the Query Engine. They are `QueryEngine`, `ModelScope`, and `FieldScope`. All of these classes are a `QueryEngine`, or inherit from a `QueryEngine`. Because of this, all these classes are also proxies around the class instance.

Being proxies, we can intercept any key access, and redirect it to instead do something else. So, for example, when we call `User.where`, what is actually happening behind the scenes is something to the effect of:

```javascript
function where() {
  return new QueryEngine(this.getConnection()).Model('User');
}
```

So actually, when you call `User.where`, what you actually get back is a sub-set of the `QueryEngine`, a `ModelScope`. This happens because we call `.Model('User')` (which is just long-hand for `query['User']`). When we specify a model on the query engine, a `ModelScope` is returned, so that we can call operators that are relevant to models.

We separate concerns into these "scopes" because otherwise some things may not make much sense. Take for example the following erroneous example:

```javascript
let user = User.where.EQ('Bob');
```

What? What are you asking for here? A **model** that equals the value `'Bob'`? That makes no sense at all...

No, it really doesn't make sense. This is why we separate concerns with "scopes". When you are actively inside a "model scope", then there are no conditional operators for example, because that doesn't make sense. The same goes for the `FieldScope`. It wouldn't make sense to do the following for example:

```javascript
let user = User.where.id.EQ.PROJECTION('User:id');
```

What? How can we apply a projection to a single field/column? This makes no sense...

Indeed.

So the query engine will return the correct "scope" based on what was just asked for.

So back to our original example... let's break it down. I will use the "direct" syntax instead of going through `User.where` so we can see all the magic happening. Keep in mind that the following example is identical to using `User.where`:

```javascript
let user = await new QueryEngine({ connection: User.getConnection() }).User.id.EQ(userID).first();
```

This is the *exact* equivalent of our original example. `new QueryEngine({ connection: User.getConnection() }).User` is what is happening behind the scenes when we call `User.where`.

So, let's break this down:

  1. `new QueryEngine({ connection: User.getConnection() })`: Return a new query engine, bound to the model's connection.
  2. `.User`: Ask the proxy class to look for the `User` key on the query engine. It won't find it, and so, seeing as it starts with an upper-case letter, and is part of the "query engine" scope, it will look for the model named "User" from the provided connection. When it finds that model, it will push a "model operation" onto the internal query stack, and return a `ModelScope` for interacting with the model just requested.
  3. `.id`: Ask the `ModelScope` proxy class to look for the `id` key. The model scope itself has no such key, so the proxy looks for a field inside the current model class (which is `User`, as defined by the previous operation at the top of the internal operation stack), and it finds such a field. It then takes this field, pushes it as a "field operation" onto the internal query engine operation stack, and returns a `FieldScope` proxy.
  4. `.EQ`: When we call `.EQ` we are asking the `FieldScope` proxy for a key named `EQ`. This is one of the methods defined on the `FieldScope` proxy, so this method is simply returned for the user to call.
  5. Next, we call the `EQ` method that was returned, providing the value `userID`. When we do this, a "conditional operation" gets pushed onto the internal operation stack, with the operator `EQ`, and the value of `userID`. The call to this method returns a `ModelScope` proxy, so we can continue chaining if we want.
  6. Finally, we request a `first` key, which the `ModelScope` proxy knows nothing about, and which is not a field in the `User` model, so the `ModelScope` redirects the request to its parent `QueryEngine` proxy, which *does* have a `first` method. This method gets returned to the user, and when called, will then call `connection.select` to select the first model using the query just defined.

See? Not quite as complicated as you might have first thought. Really, the entire system boils down to the following moving parts:

  1. An internal "operation" stack is continually added to for each operation.
  2. Proxies are used to lookup keys to figure out what the user is requesting.
  3. The correct "scope" is returned as part of the proxy system to ensure we don't do anything that is wonky.

## AND, OR, and NOT

### AND & OR

The Mythix ORM Query Engine obviously supports logical operations. To properly use them you should understand how they work. The first thing to mention is that `AND` and `OR` operators are *state systems*. What I mean by this is that they "toggle" the internal state. So if for example we do: `User.where.id.EQ('Something').firstName.EQ('Bob').lastName.EQ('Brown')`, this works because `AND` is the default operation of a query engine, and by default it is "toggled *on*". In essence, when you first instantiate a query engine, it is equivalent to `User.where.AND.id.EQ('Something')...`. At any time we can switch into `OR` mode simply by accessing the `OR` key: `User.where.id.EQ('Something').OR.firstName.EQ('Bob').lastName.EQ('Brown')`. As you can see here, we "toggle" into "OR" mode, and all statements following `OR` are in this mode. So the final database query would look like the following: `WHERE user.id = 'Something' OR user.firstName = 'Bob' OR user.lastName = 'Brown'`.

### NOT

Unlike `AND` and `OR`, a `NOT` operation is *not* a toggle. `NOT` behaves differently. `NOT` is only "enabled" until the next operation. Once the next operation has been executed, `NOT` is toggled back off. So if we were to `User.where.id.NOT.EQ('Something').firstName.EQ('Bob')`, then the resulting database query would be `WHERE user.id != 'Something' AND user.firstName = 'Bob'`. Notice how `firstName` is "equals", instead of "not equals". This is because the `NOT` operator is disabled as soon as we call the first `EQ`. The reason for this behavior is that it is often desired for `AND` and `OR` to continue after a condition, but it is rarely desired for `NOT` to continue after a condition.

### Chaining multiple

If you were to chain multiple logical operators, only the last one takes effect, and the previous ones are ignored. For example, if you do a `User.where.AND.AND.AND.OR.id.EQ('Something')` then only the last `OR` is honored.

Once again, as already explained, `NOT` is a one-shot operator, and only takes effect once... so if you threw a `NOT` somewhere in the above example, it would also take effect, but only once, and would be combined with the final `AND` or `OR` operator in the chain.

### Grouping conditions

Any good query engine needs the ability to group conditions. Grouping with the Mythix ORM Query Engine is as simple as *calling* the `AND` or `OR` operator, and providing it a sub-query. Take for example: `User.where.id.EQ('Something').AND(User.where.firstName.EQ('Bob').lastName.EQ('Brown)).OR(User.where.age.GT(18))`. This would produce the following database query `WHERE user.id = 'Something' AND (user.firstName = 'Bob' AND user.lastName = 'Brown') OR (user.age = 18)`.

As you can see, *calling* the logical operator, and providing a query engine as the first and only argument, you can group conditions, and can do so as many times and group as deeply as needed.

## Merging queries

Mythix ORM has the ability for a user to merge two or more queries together. This makes the query engine even more powerful, as queries can be generated and composed. Take a look at the following example to better understand this:

```javascript
let userQuery = User.where.id.EQ('Something').firstName.EQ('Bob').lastName.EQ('Brown');

let roleQuery = Role.where.userID.EQ(User.where.id).name.EQ('admin');

let finalQuery = userQuery.MERGE(roleQuery);
```

Merge is mostly a concatenation operation. It works by concatenating the operation stack of the target query unto the source query. There are a few exceptions though that make this unlike a concatenation. First, any "prefixing" logical operators will be ignored. This is intentional so that `.AND.MERGE` and `.OR.MERGE` actually make sense (otherwise, if the target query started with an `.AND` or `.OR` the previous one on the source query would be ignored). The second exception that makes this unlike concatenation is that any `ORDER`, `GROUP_BY`, or `PROJECTION` on the target query are ADDED to their respective parts on the source query. For example, a `PROJECTION` on the target query will not reset the projection, but instead add to it.

After the merge shown above, we can see by concatenating the two queries "operation stacks" together, that we end up with a final query that is equivalent to:

```javascript
let finalQuery = User.where.id.EQ('Something').firstName.EQ('Bob').lastName.EQ('Brown').AND.Role.where.userID.EQ(User.where.id).name.EQ('admin');
```

A keen reader will have noticed that I threw and `.AND.MERGE` into the above example. Remember how we discussed that `AND` and `OR` operators are toggles, and `AND` is defaulted to "on"? Well, since `AND` is the current logical operator, we "and merge" the query. We could just as easily `.OR.MERGE(query)` and that would `OR` the two queries together instead of `AND`ing them together. This might be important if you are merging two conditional queries together, and want `{first conditions} OR {second conditions}` instead of `{first conditions} AND {second conditions}`.

Mythix ORM is smart enough to know what to do with this query. It will always "scan" a query in the generation process to see if there are any table joins as a first step. In our above example, it will find `.Role.where.userID.EQ(User.where.id)`, which is a table join, so this will come first in our query during generation into SQL (or whatever). It will then walk the chain of operators to build the `WHERE` conditions. In short: it doesn't matter where you place table joins, projections, group clauses, or order clauses. They can be at the beginning of your query, or at the very end, and Mythix ORM will still understand what you want, and do the right thing. The only thing that actually matters is that the order remain consistent in the operation stack.

## Table joins

A table join using the Mythix ORM Query Engine is as simple as `Role.where.userID.EQ(User.where.id)`. You will notice that the internal query provided to `EQ` doesn't have any conditions. It is just a field. Essentially, we are saying `where role.userID = user.id`... which looks very much like a table join. It should be no surprise that is exactly what it is.

If you want to join on multiple fields/columns, then you simply repeat the process: `Role.where.userID.EQ(User.where.id).AND.name.EQ(User.where.primaryRoleName)`, and this would join on `role.userID = user.id AND role.name = user.primaryRoleName`.

You also need not stop at a single join. Join as many times as you like!

```javascript
Role.where.userID.EQ(User.where.id).AND.Posts.userID.EQ(User.where.id).AND.Comments.userID.EQ(User.where.ID)
```

*Note:* The `AND` operators above are not needed... they are simply provided for clarity (`AND` is the default logical operator, and is already toggled "on", so it isn't required).

You can specify what type of join to do with one of the control operations. They are listed below:

1. `INNER_JOIN` (use an inner table join)
2. `LEFT_JOIN` (use a left table join)
3. `RIGHT_JOIN` (use a right table join)
4. `FULL_JOIN` (use a full table join)
5. `CROSS_JOIN` (use a cross table join)
6. `JOIN(type)` (use a custom user specified join type)

To use one of these, simply specify it before you join:

```javascript
let roles = await Role.where.LEFT_JOIN.userID.EQ(User.where.id).all();
```

## Sub-queries

Unlike table joins, sub-queries can be specified by *providing at least one condition for the sub-query*. For example, if we do the following `Role.where.userID.EQ(User.where.id.EQ([ 'value1', 'value2', 'value3' ]))` then this would specify a sub-query. **Sub queries have conditions... table joins do not**. The result of this query would be:

```sql
SELECT Role.* WHERE Role.userID IN (SELECT User.id WHERE user.id IN ('value1', 'value2', 'value3'))
```

So, again, I repeat myself:

  1. Table joins are specified where the provided sub-query results in a field, **and has no conditions**.
  2. A sub-query is specified where the provided sub-query results in a **full query with conditions**.

## Special cases

There are some special cases where the query engine will improve the quality of life of the developer by *inferring* what the developer actually meant. For example, if you noticed the query above, we provided an *array* of values to `EQ`, and this turned into an `IN (...)` condition when it went through the SQL generator. This is the standard behavior when you provide an array to the `EQ` or `NEQ` operators. Furthermore, Mythix ORM takes an extra step to make your life easier, and will pluck out any `null`, `true`, or `false` values inside the array, and appropriately generate the query to reflect these. For example, if you were to query like `User.where.firstName.EQ([ 'Bob', 'Scott', 'Barbera', 'Tina', null ])` then the generated query would be `WHERE (user.firstName IN ('Bob', 'Scott', 'Barbera', 'Tina') OR user.firstName IS NULL)`. This is also the case for `true` and `false`: `Property.where.key.EQ('someKey').AND.value.EQ([ null, true, false ])` would result in `WHERE property.key = 'someKey' AND (property.value IS NULL OR property.value IS TRUE OR property.value IS FALSE)`.

So, in short, providing arrays to `EQ` or `NEQ` operators is equivalent to an `IN` operator (or `NOT IN` for `NEQ`), with special cases handled automatically for you.

*Note:* Mythix ORM **does not** provide an `IN` operator on the query engine, because it is expected that you will simply use `EQ` or `NEQ` with an array of values instead.

## Projections

Projections define which columns you want returned from the database. One important thing to note right up-front is that Mythix ORM by default will *always* **only** return the root model of a query unless you specifically request or project other columns. In Mythix ORM you **never** specify a column name directly. This is a deliberate design decision to try and abstract the engine away from your database so you don't lock yourself into a corner, relying heavily on the exact structure of your database. However, the actual column name can be defined on your field schema, so you can still always target exactly what you *intend* to target.

The next most important thing to inform you of is that there is no "includes" B.S. in Mythix ORM. If you want to include related models in a query then you **project those model's fields**. Said another way, whatever model fields you project will be the models that are constructed on load.

Let's see some examples:

```javascript
let roles = await Role.where.userID.EQ(User.where.id).all();
```

This will return *only* `Role` model instances. There will be no `User` models loaded from the database.

However, if you *project* the `User` fields, then the `User` models will be loaded:

```javascript
let rolesWithUsers = await Role.where.userID.EQ(User.where.id).PROJECT('Role', 'User').all();

let users = rolesWithUsers.reduce((users, role) => {
  return users.concat(role.Users);
}, []);
```

### Adding to the projection

Normally when you specify a projection it will *replace* any previous projection. This is not always desired however. Sometimes you just want to add, or subtract something from the projection. This is easy to do in Mythix ORM. All you need to do to *add* to the projection is prefix your fully qualified field name with a `+` character. For example, `.PROJECT('+User:firstName')` will *add* the first name of the user to the field projection, whereas `.PROJECT('User:firstName')` would *replace* the projection.

You can also define `'+'` first, as any argument, and all arguments following it will be *added*. For example: `.PROJECT('+', 'User:firstName', 'User:lastName', 'User:age')` will *add* the specified user fields to the projection.

*Note:* If even one field is encountered and the operation ("add" or "subtract") isn't known, then the projection will be reset. For example, the following would reset the projection: `.PROJECT('User:id', '+', 'User:firstName', 'User:lastName', 'User:age')`. The first `'User:id'` here would reset the previous projection (if any) because Mythix ORM doesn't know if you want to add or subtract the field, and the default is "replace projection". In this case, the final projection would include *four* user fields. `'User:id'` replaces the projection, and then `'User:firstName'`, '`User:lastName'`, and `'User:age'` are added to it.

### Subtracting from the projection

To subtract from the projection... you guessed it, you just use `-` instead of `+`:

```javascript
let justRoles = await Role.where.userID.EQ(User.where.id).PROJECT('Role', 'User').PROJECT('-User').all();
```

Just like `'+'` you can use `'-'` as any argument, and all fields following it will be subtracted:

```javascript
let justRoles = await Role.where.userID.EQ(User.where.id).PROJECT('Role', 'User').PROJECT('-', 'User:id', 'User:firstName', 'User:lastName', 'User:age').all();
```

### Adding and subtracting to/from projection

You can mix and match `'+'` and `'-'` in any order... the only rule is that any argument following one of these will be either added or subtracted:

```javascript
let strangeQuery = User.where.id.EQ('something').PROJECT('+', 'User:id', 'User:firstName', '-', 'User:lastName', '+', 'User:age')
```

### Include everything

A shorthand to "include everything" is to simply use the `*` operator:

```javascript
let rolesWithUsers = await Role.where.userID.EQ(User.where.id).PROJECT('*').all();
```

You can also include everything, and then subtract some things:

```javascript
let rolesWithPartialUsers = await Role.where.userID.EQ(User.where.id).PROJECT('*', '-', 'User:lastName', 'User:age').all();
```

And, as always, you can use literals anywhere:

```javascript
const { Literals } = require('mythix-orm');
...

let roleCountQuery = Role.where.PROJECT(new Literals.Literal('COUNT(*)'));
```

Though the previous example would be better using the correct literal:

```javascript
const { Literals } = require('mythix-orm');
...

let roleCountQuery = Role.where.PROJECT(new Literals.CountLiteral('Role:id', { as: 'count' }));
```

**Note: Everything discussed above also applies to `GROUP_BY` and `ORDER` operations. These also behave the same way with adding and subtracting fields.**

## Order

Query row order can be specified with the `ORDER` operation. `ORDER` behaves exactly the same as the `PROJECT` operation, allowing you to add and remove order fields in the same way.

So, if fields can be added and removed from the order, great! But, how does one specify "ASC" or "DESC" order for the fields provided? Simple! Just use the `.ASC` and `.DESC` sub-calls to the `ORDER` operation. When you do this, any fields that are being added in the operation will be either "ASC" or "DESC" as defined. For example:

```javascript
let query = User.where.id.EQ('something').ORDER.ASC('+User:firstName').ORDER.DESC('+User:lastName');
```

Here we are adding the `User:firstName` field to the order, in ASC (ascending) order, and we are also adding
`User:lastName`, in DESC (descending) order.

Sometimes you just need to add to the order fields, while also specifying the direction at the same time. This can be useful for example if you are taking field orders directly from a third-party (i.e. a client). A client for example might send along with its request the fields it wants, and in what order.

For this purpose, Mythix ORM provides `ORDER.ADD` and `ORDER.REPLACE`. Unlike the default mode of operation where `+` and `-` prefixes on fields mean `add` or `subtract` the field, when you use `ORDER.ADD` or `ORDER.REPLACE`, the `+` and `-` prefixes instead mean "ASC" or "DESC" respectively.

So for example, we can add the following fields to the order, while specifying if each field is in "ASC" or "DESC" order:

```javascript
let adultUsers = await User.where.age.GTE(18).ORDER('createdAt').ORDER.ADD('+firstName', '-lastName').all();
// will result in SQL like: ... ORDER BY createdAt ASC, firstName ASC, lastName DESC;
```

`ORDER.REPLACE` will replace any previous `ORDER` specified, while also using the `+` and `-` prefix on fields to define their "ASC" or "DESC" order. For example:

```javascript
let adultUsers = await User.where.age.GTE(18).ORDER('createdAt').ORDER.REPLACE('+firstName', '-lastName').all();
// will result in SQL like: ... ORDER BY firstName ASC, lastName DESC;
// notice how "createdAt" was dropped from the ORDER, because we used REPLACE.
```

There is no `SUB` (subtract) operation for this style of ORDER operation, because you can still subtract with the normal `ORDER` operator. For example:

```javascript
let adultUsers = await User.where.age.GTE(18).ORDER('createdAt').ORDER.ADD('+firstName', '-lastName').ORDER('-createdAt').all();
// will result in SQL like: ... ORDER BY firstName ASC, lastName DESC;
// notice how "createdAt" was dropped from the ORDER, because we used ORDER('-createdAt').
```

## Grouping

Grouping (`GROUP BY`) can be achieved by using the `GROUP_BY` operation. It behaves identically to `PROJECT`, and will allow you to add and remove fields to the `GROUP BY` clause in the same way you would add or remove fields from a projection.

With group by you often want to use literals because you might need aggregates. This is fully supported, and can be provided just like you would provide them to `PROJECT`.

You can always combine a `GROUP_BY` operation with `HAVING`. `HAVING` takes a sub-query with conditions specified. Those conditions will be turned into `HAVING` conditions for the `GROUP BY` clause.

```javascript
// Get the number of same last-name's across all adult users
let countLiteral = new Literals.CountLiteral('User:lastName', { as: 'count' });
let sameLastNameCounts = await User.where.GROUP_BY(countLiteral).HAVING(User.where.age.GTE(18)).PROJECT(new Literals.FieldLiteral('User:lastName', { as: 'lastName' }), countLiteral).all();
```
*Note: Mythix ORM will not return models when a `GROUP_BY` is used in a query. When a `GROUP_BY` is active on a query, then the `select` results of the query will always be returned as raw data (the group by results).*

*Note: Mythix ORM will not modify your projection for you simply because a GROUP_BY was specified. You must ensure yourself that your projection matches the GROUP_BY fields that you specify.*

## Distinct

You can turn any query into a `DISTINCT` query using the `DISTINCT` operation. This operation takes two forms. You can *just* specify `DISTINCT`, as in:

```javascript
let roles = await Role.where.DISTINCT.userID.EQ(User.where.id).all();
```

Or, you can specify a field for the `DISTINCT` operation:

```javascript
let roles = await Role.where.DISTINCT('Role:id').userID.EQ(User.where.id).all();
```

*Note:* `DISTINCT` can work differently across databases. For example, some databases won't allow you to specify a field for `DISTINCT`, and some databases might behave differently with a `DISTINCT`. Your query won't change however, the low-level query generator will simply adjust according to the connection/database. Just keep in mind that this might behave differently across databases.

## List of connection interface methods

Connection interface methods are methods which hand off the query to a connection method to interact with the database. The following are methods that can be called directly from a query engine to act upon the query.

  1. `all(options)` - load everything specified in the query from the database. The default `batchSize` is `500`. The entire query is fetched in batches, until the query has been exhausted, and the entire result returned as an array of model instances.
  2. `cursor(options)` - load everything specified in the query from the database. The default `batchSize` is `500`. The entire query is fetched in batches, until the query has been exhausted, and the entire result returned as an array of model instances. Unlike `all`, this returns an async generator, and each model fetched
  will be streamed from the database (using a `yield` for each model fetched).
  3. `first(count, options)` - get only the first (count) rows from the database, using the query provided. If `count` is not specified, or is `null`, then it defaults to `1`. The result(s) (if any) will be returned as a model instance (if a count of `1` was specified), or an array of model instances.
  4. `last(count, options)` - get only the last (count) rows from the database, using the query provided. If `count` is not specified, or is `null`, then it defaults to `1`. This operation works by ordering the query generator interface to invert the `ORDER` specified on the query, and then selects the first (count) rows specified. The result(s) (if any) will be returned as a model instance (if a count of `1` was specified), or an array of model instances.
  5. `updateAll(attributes, options)` - update all rows matching the query using the attributes provided. This is a bulk update operation. It expects a single object of attributes (field values) to apply across all matching rows. *Note: `on*Save` and `on*Update` hooks **will not be run** when using this operation.*
  6. `destroy(options)` - destroy all matching rows. This works by using a sub-query, i.e. `DELETE FROM table WHERE table.id (SELECT id FROM table WHERE ...)`.
  7. `average(field, options)` - select an average across matching rows for a single column. The underlying connection is required to always return a `number`.
  8. `count(field, options)` - count the number of rows matching the query. If field is `null` or `undefined` then it will default to `*` (all fields). The underlying connection is required to always return a `number`.
  9. `min(field, options)` - get the minimum value for a single column for all rows matching the query. The underlying connection is required to always return a `number`.
  10. `max(field, options)` - get the maximum value for a single column for all rows matching the query. The underlying connection is required to always return a `number`.
  11. `sum(field, options)` - get the summed value for a single column for all rows matching the query. The underlying connection is required to always return a `number`.
  12. `pluck(fields, options)` - pluck only certain columns from all matching rows. If a single field is provided, then the return value will be a flat array of column values. If more than one field is specified, then the return value will be a two-dimensional array of row/column values.
  13. `exists(options)` - check to see if any rows match the query. This will return `true` if one or more rows match the query, and `false` otherwise.

## List of operators

### Logical

  1. `AND` or `AND(...)` (see <see>ModelScope.AND</see>)
  2. `OR` or `OR(...)` (see <see>ModelScope.OR</see>)
  3. `NOT` (see <see>ModelScope.NOT</see>)

### Conditional

*Note:* `NOT` is always a logical inversion.

  1. `EQ(...)` (equals `=`) (see <see>FieldScope.EQ</see>)
  2. `EQ([ ... ])` (in list of values `IN (...)`) (see <see>FieldScope.EQ</see>)
  3. `EQ.ANY(subQuery)` (equals any in the list returned by the sub-query) (see <see>FieldScope.EQ</see>)
  4. `EQ.ALL(subQuery)` (equals any in the list returned by the sub-query) (see <see>FieldScope.EQ</see>)
  5. `NOT.EQ(...)` (not equals `!=`) (see <see>FieldScope.NEQ</see>)
  6. `NOT.EQ([ ... ])` (not in list of values `NOT IN (...)`) (see <see>FieldScope.NEQ</see>)
  7. `NEQ(...)` (not equals `!=`) (see <see>FieldScope.NEQ</see>)
  8. `NEQ([ ... ])` (not in list of values `NOT IN (...)`) (see <see>FieldScope.NEQ</see>)
  9. `NEQ.ANY(subQuery)` (not equals any in the list returned by the sub-query) (see <see>FieldScope.NEQ</see>)
  10. `NEQ.ALL(subQuery)` (not equals any in the list returned by the sub-query) (see <see>FieldScope.NEQ</see>)
  11. `NOT.NEQ(...)` (equals `=`) (see <see>FieldScope.EQ</see>)
  12. `NOT.NEQ([ ... ])` (in list of values `IN (...)`) (see <see>FieldScope.EQ</see>)
  13. `GT(...)` (greater than `>`) (see <see>FieldScope.GT</see>)
  14. `GT.ANY(subQuery)` (greater than any value returned by sub-query) (see <see>FieldScope.GT</see>)
  15. `GT.ALL(subQuery)` (greater than largest value returned by sub-query) (see <see>FieldScope.GT</see>)
  16. `NOT.GT(...)` (less than or equal to `<=`) (see <see>FieldScope.LTE</see>)
  17. `GTE(...)` (greater than or equal to `>=`) (see <see>FieldScope.GTE</see>)
  18. `GTE.ANY(subQuery)` (greater than or equal to any value returned by sub-query) (see <see>FieldScope.GTE</see>)
  19. `GTE.ALL(subQuery)` (greater than or equal to largest value returned by sub-query) (see <see>FieldScope.GTE</see>)
  20. `NOT.GTE(...)` (less than `<`) (see <see>FieldScope.LT</see>)
  21. `LT(...)` (less than `<`) (see <see>FieldScope.LT</see>)
  22. `LT.ANY(subQuery)` (less than any value returned by sub-query) (see <see>FieldScope.LT</see>)
  23. `LT.ALL(subQuery)` (less than the smallest value returned by sub-query) (see <see>FieldScope.LT</see>)
  24. `NOT.LT(...)` (greater than or equal to `>=`) (see <see>FieldScope.GTE</see>)
  25. `LTE(...)` (less than or equal to `<=`) (see <see>FieldScope.LTE</see>)
  26. `LTE.ANY(subQuery)` (less than or equal to any value returned by sub-query) (see <see>FieldScope.LTE</see>)
  27. `LTE.ALL(subQuery)` (less than or equal to smallest value returned by sub-query) (see <see>FieldScope.LTE</see>)
  28. `NOT.LTE(...)` (greater than `>`) (see <see>FieldScope.GT</see>)
  29. `LIKE(...)` (pattern match `LIKE '%something%'`) (see <see>FieldScope.LIKE</see>)
  30. `LIKE(..., { caseSensitive: true })` (pattern match `LIKE '%something%'`) [only supported in PostgreSQL -- default in PostgreSQL is `ILIKE`] (see <see>FieldScope.LIKE</see>)
  31. `NOT.LIKE(...)` (not matching pattern `NOT LIKE '%something%'`) (see <see>FieldScope.NOT_LIKE</see>)
  32. `NOT.LIKE(..., { caseSensitive: true })` (not matching pattern `NOT LIKE '%something%'`) [only supported in PostgreSQL -- default in PostgreSQL is `NOT ILIKE`] (see <see>FieldScope.NOT_LIKE</see>)

### Control

  1. `PROJECT('+', ...)` (add specified fields to the projection) (see <see>ModelScope.PROJECT</see>)
  2. `PROJECT('-', ...)` (remove specified fields from the projection) (see <see>ModelScope.PROJECT</see>)
  3. `PROJECT(...)` (replace projection with specified fields) (see <see>ModelScope.PROJECT</see>)
  4. `PROJECT('+Model:field', '-OtherModel:field')` (mixed addition and removal) (see <see>ModelScope.PROJECT</see>)
  5. `DISTINCT` (distinct across entire projection) (see <see>ModelScope.DISTINCT</see>)
  6. `DISTINCT('Model:field')` (distinct on specified field) (see <see>ModelScope.DISTINCT</see>)
  7. `LIMIT(number)` (limit query to number of rows) (see <see>ModelScope.LIMIT</see>)
  8. `OFFSET(number)` (offset into rows to start selecting) (see <see>ModelScope.OFFSET</see>)
  9. `ORDER(...)` (add or remove specified fields from the order clause--added fields are in ascending order) (see <see>ModelScope.ORDER</see>)
  10. `ORDER('+Model:field', '-OtherModel:field')` (mixed addition and removal to the order clause--added fields are in ascending order) (see <see>ModelScope.ORDER</see>)
  11. `ORDER.ASC(...)` (add or remove fields from the order clause--added fields are in ascending order) (see <see>ModelScope.ORDER</see>)
  12. `ORDER.DESC(...)` (add or remove fields from the order clause--added fields are in descending order) (see <see>ModelScope.ORDER</see>)
  13. `ORDER.ADD(...)` (add fields to the order clause--the `+` and `-` prefixes on fields specifies the "ASC" or "DESC" order for each field [instead of specifying to remove or add fields]) (see <see>ModelScope.ORDER</see>)
  14. `ORDER.REPLACE(...)` (replace any previous order with this one, and add the specified fields to the order clause--the `+` and `-` prefixes on fields specifies the "ASC" or "DESC" order for each field [instead of specifying to remove or add fields]) (see <see>ModelScope.ORDER</see>)
  15. `GROUP_BY(...)` (add or remove specified fields from the group by clause) (see <see>ModelScope.GROUP_BY</see>)
  16. `GROUP_BY('+Model:field', '-OtherModel:field')` (mixed addition and removal to the group by clause) (see <see>ModelScope.GROUP_BY</see>)
  17. `HAVING(subQueryWithConditions)` (specify conditions for a GROUP_BY clause... will be ignored if no `GROUP_BY` has been applied to the query) (see <see>ModelScope.HAVING</see>)
  18. `INNER_JOIN` (use an inner table join) (see <see>ModelScope.INNER_JOIN</see>)
  19. `LEFT_JOIN` (use a left table join) (see <see>ModelScope.LEFT_JOIN</see>)
  20. `RIGHT_JOIN` (use a right table join) (see <see>ModelScope.RIGHT_JOIN</see>)
  21. `FULL_JOIN` (use a full table join) (see <see>ModelScope.FULL_JOIN</see>)
  22. `CROSS_JOIN` (use a cross table join) (see <see>ModelScope.CROSS_JOIN</see>)
  23. `JOIN(type)` (use a custom user specified join type) (see <see>ModelScope.JOIN</see>)
  24. `EXISTS(subQuery)` (returns true if the sub-query returns at least one row) (see <see>ModelScope.EXISTS</see>)

## Other

  1. `MERGE` (merge a query onto this query, without modifying the original query) (see <see>QueryEngine.MERGE</see>)
  2. `Model(...)` (select a model directly and return a `ModelScope`) (see <see>QueryEngine.Model</see>)
  3. `Field(...)` (select a field directly and return a `FieldScope`) (see <see>ModelScope.Field</see>)
  4. `toString` (convert the query into a string... generated by the underlying connection) (see <see>QueryEngine.toString</see>)
  5. `unscoped()` (reset the query back to its root model, with no default scope applied) (see <see>QueryEngine.unscoped</see>)

## Final Notes

  1. `$` is a short-hand for `where`, so the following two examples are equivalent: `User.where.id.EQ('something')`, and `User.$.id.EQ('something')`.
  2. Don't forget `$` or `where` in sub-queries. It is easy to do `Role.where.userID.EQ(User.id)`, but this won't work. Mythix ORM doesn't expose the fields directly on the model by design, so `User.id` will simply be `undefined`. Don't forget to turn this properly into a sub-query with `$` or `where`: `Role.where.userID.EQ(User.$.id)`.
  3. You can use `toString` to turn the query into a query string generated by the underlying connection: `query.toString()`
  4. You can use literals almost anywhere there is an input into the query engine.
  5. `Model` is a way to directly specify a model. This can be handy for example if you have a name collision. `query.Model('MyModel')` is the same as `query.MyModel`, but might be important, if for example, your model just happened to be named `Field`, which is the name of another method on the query engine itself: `query.Model('Field')`.
  6. `Field` is a way to directly specify a field. This can be handy for example if you have a name collision. `query.Field('myField')` is the same as `query.myField`, but might be important, if for example, your field just happened to be named `Model`, which is the name of another method on the query engine itself: `query.Field('Model')`.
  7. **Never specify column names directly**. This won't work in Mythix ORM. You must specify the field name defined in your model schema for all operations. Mythix ORM will convert it to the proper column name in all operations. You can always use literals if you need to bypass this constraint.
