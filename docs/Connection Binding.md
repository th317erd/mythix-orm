Connection binding is the process of binding a connection to a model. By default when you create a Mythix ORM connection, it will bind the connection to the models provided, unless you pass the option `{ bindModels: false }` to the connection when you create it.

Connection binding works by setting a `static _mythixBoundConnection = connection` property onto the model class itself. This however comes with its own side-effects, not the least of which is modifying your class directly. While this generally isn't an issue, it may become an issue if you wish to use more than one connection in your application, and it can be a very big issue during unit testing.

Mythix ORM works this way because while architecting Mythix ORM, it was decided that there would be no use (or minimal use) of globals, and that any application can and should be able to use multiple connections simultaneously.

Because of this design decision, it becomes difficult to provide a needed connection to each model. The solution to this problem is to bind the connection used for your application to each model class directly via a static property on the model class itself, or to provide a connection via other means (discussed below).

## The Connection binding problem

Mythix ORM will only allow you to bind a connection to a model class once. Attempting to bind a connection more than once to a specific model will throw an exception (unless you specify `{ forceConnectionBinding: true }` as an option to the connection when you create your connection... which you should only ever do if you know exactly what you are doing).

This can cause very big issues for you when you go to write unit tests for your application. Generally unit tests will run randomly, or in parallel. Because of this, it is often desired to have a connection instance per-test-suite. However, as you may have guessed, since the default connection binding scheme works by modifying your model classes directly, and attempting to bind more than once will throw an exception, this will cause big problems when your model spans unit tests, but connections do not span unit tests, and still need to be bound to the model somehow.

## Opting out of connection binding

Because of the issues connecting binding can cause, Mythix ORM allows the user to opt-out of connection binding if they choose to do so. All you need to do is pass a `{ bindModels: false }` option to your connection when you create it. When you do this, Mythix ORM will no longer modify your model class, and hence won't bind any connection to your models.

...but, now where does Mythix ORM look to find a connection?

By default, Mythix ORM will generally always call the model method <see>Model.getConnection</see>. This method will search for a connection in the following order, and will return the first valid connection found:

  1. The provided `connection` via the arguments to the call (if provided).
  2. The connection provided to the model instance itself (if any was provided) via `model.connection`. A connection can be provided to a model as the "options" when you create a model, i.e. `new MyModel(attributes, { connection })`.
  3. Finally, if both of those fail, <see>Model.getConnection</see> will call <see>Model._getConnection</see> to attempt to find a connection that way.

<see>Model._getConnection</see> is a static per-model "global helper" method for finding the connection for a model. By default, it will search for a connection in the following order, and will return the first valid connection found:

  1. The provided `connection` via the arguments to the call (if provided).
  2. The connection on the [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html) "model context", if any exists.
  3. The `connection` property directly on the [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html), if any exists (which is set by transactions).
  4. Finally, it will see if the `static _mythixBoundConnection` property on the class is a valid connection, and if so, return that.

## Connection binding solution #1

The best solution to connection binding issues, especially in unit tests, is to simply wrap your tests in a <see>Connection.createContext</see> call. <see>Connection.createContext</see> will create a connection context using [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html), that will be available for every method inside your callback to <see>Connection.createContext</see>.

Using this method, your unit test connection can opt-out of connection binding to the models by passing a `{ bindModels: false }` to your test connection. Then, when you wrap all your tests with a <see>Connection.createContext</see> call, the connection will be provided to all models and all operations in your application that uses your models via the [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html) context.

This can be fairly easily done in most test runners by simply hijacking the `it` or `test` methods. For example:

```javascript
// The following methods should probably
// be included from some test-helper file

/* global it, fit */
const _it = it;
const _fit = fit;

// Hijack the "it" method, wrapping the test
// in a `connection.createContext` call.
function createIT(func, getConnection) {
  return function it(desc, runner) {
    return func.call(this, desc, async () => {
      await getConnection().createContext(runner);
    });
  };
}

function createFIT(func, getConnection) {
  return function fit(desc, runner) {
    return func.call(this, desc, async () => {
      await getConnection().createContext(runner);
    });
  };
}

function createRunners(getConnection) {
  return {
    it:   createIT(_it, getConnection),
    fit:  createFIT(_fit, getConnection),
  };
}

const models = require('./my-models');

describe('MyModelTest', () => {
  let connection;

  // Hijack "it" and "fit" so that each test is
  // wrapped in a `connection.createContext` call
  const { it, fit } = createRunners(() => connection);

  beforeAll(async () => {
    connection = createTestConnection({
      bindModels: false,
      models: models,
    });

    await connection.createTables(models);
  });

  afterEach(async () => {
    await connection.dropTables(models);
  });

  it('can create a model', async () => {
    let myModel = await models.MyModel.create({ someAttribute: 'test' });
    expect(myModel.someAttribute).toEqual('test');
  });
});
```

As you can see, this way of solving the connection binding problem can easily solve the issues by providing a connection to all your model and application code via an [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html) context.

## Connection binding solution #2

Another method that will give okay results (as long as you are careful how you write your code) is to subclass all your models, and bind the connection on a subclass.

This can solve the problem entirely if you write your code correctly, but caution needs to be taken on how you use models in your application. You must **always** call <see>connection.getModel</see> or <see>connection.getModels</see> everywhere throughout your code for **every** single model class you interact with. The benefit to this painstaking endeavour is that at least model auto-reloading will work flawlessly for you 😊.

Let's see an example of how this might work:

```javascript

let { MyModel: _MyModel } = require('./my-models);

describe('MyModelTest', () => {
  let connection;
  let models;

  beforeAll(async () => {
    // Bind the connection by subclassing your model
    class MyModel extends _MyModel {
      static getConnection() {
        return connection;
      }
    }

    connection = createTestConnection({
      bindModels: false,
      models: [
        MyModel,
      ],
    });

    // Now we need to ensure we always use our
    // subclass instead of the imported model class.
    models = connection.getModels();

    await connection.createTables(models);
  });

  afterEach(async () => {
    await connection.dropTables(models);
  });

  it('can create a model', async () => {
    // now all this code will work...
    // AS LONG AS _all_ your application code
    // also uses `connection.getModels` so that
    // your application always gets the
    // subclassed model
    let myModel = await models.MyModel.create({ someAttribute: 'test' });
    expect(myModel.someAttribute).toEqual('test');
  });
});
```

This example is incomplete, because in the real-world you would also need to provide this same test connection to your application code... but I will leave that challenge up to you to figure out 🙂.

## Connection binding solution #3

Another solution is just to provide a truly global connection to your models. This might be a wee bit difficult to figure out, especially for unit testing, but can still be accomplished. To make this work, simply overwrite the `static _getConnection` method on all your models, and return the global connection. This is generally best done by providing a base model class that all your other models inherit from.

For example:

```javascript
const globalApplicationConnection = require('./global-connection');

class ModelBase {
  static _getConnection() {
    return globalApplicationConnection;
  }
}

class MyModel extends ModelBase {
  ...
}
```

Keep in mind the possible issues this might cause, especially when unit testing. By sharing a global connection across tests, you might end up with data in your database that you don't expect from other tests that are running in parallel. If you go the route of a global connection, it is highly recommended that you request your test runner to run tests serially, instead of randomly, or in parallel.

## Connection binding solution #4

The final and most tedious solution to the connection binding problem is to simply always supply every model you create with a connection.

For example:

```
let myModel = new MyModel(attributes, { connection });
```

However, you *also* need to supply a connection to the query engine for every query you create:

```
let query = MyModel.where(connection).something.EQ('test');
```

And you may be required to provided a connection in other circumstances as well.

## Final notes:

  1. Generally, Mythix ORM will call <see>Model.getConnection</see> first (if possible), before calling the lower-level <see>Model._getConnection</see>. This will allow the connection to be provided by the model instance itself.
  2. When <see>Model.getConnection</see> is not available (for example, in a context where there is no instance of a model), then <see>Model._getConnection</see> will be called directly to fetch a connection.
  3. The static property `_mythixBoundConnection` on a model class is where Mythix ORM will finally attempt to locate a connection.
  4. An [AsyncLocalStorage](https://nodejs.org/docs/latest-v16.x/api/async_context.html) context can always be used, anywhere in your code, to provide all code beneath a connection. This can be accomplished by calling <see>Connection.createContext</see> and providing it an async callback.
  5. Lastly, you can manually supply connections to your models when you create your models, and manually supply your connection elsewhere that it is needed, such as to queries.
  6. Relational fields, such as <see>ForeignKeyType</see>, <see>ModelType</see>, and <see>ModelsType</see> require a connection, or things will explode. Most other model operations will attempt to do their best without a connection, but these field types will throw exceptions if no connection is available.