# Connection Binding

TODO: Blurb about connection binding

## The Connection binding problem

By default, Mythix ORM will "bind" your models to a connection when you first supply them to said connection. It does this by setting a `static _getConnection` method on each of your model classes. This allows all underlying Mythix ORM code to then fetch the connection whenever needed.

However, this can cause big problems during unit testing. Tests are often ran in random order, or in parallel, and so you might (and probably do) have multiple connections in use at the same time. Mythix ORM will throw an error if you attempt to bind a connection to a model more than once, and so when each of your test suites creates a connection, and binds the connection to your models, now your models already have a `static _getConnection` method bound at the "root" level of your application, on the models themselves. This will then cause big issues, because each test will try to bind your models, but at the model level a connection is already bound, and your tests will explode.

Not ideal, I know... The reason for this is that Mythix ORM was designed from the ground up to be able to use multiple connections simultaneously, so there is no "global scope" to access connections from, and so we are stuck with this problem.

Now you *can* ask Mythix ORM not to bind the connection to your model's by passing `bindModels: false` to the options of any connection driver. However, when you do this, you are then required to pass a `connection` instance to most method calls (and to all query interfaces). Because this is far from ideal, and quite annoying, when you request `bindModels: false` to a connection, then the connection *will still* bind the models, but instead of injecting a `static _getConnection` on your models, it will instead subclass all your models, essentially doing:

```javascript
class BoundModel extends UserProvidedModel {
  static _getConnection() {
    return thisConnection;
  }
}
```

This is great, and it fixes most of the problem, but now we have a new problem... now we have two copies of all models, the "default" models as the user defined them, and "bound" copies, that exist on the connection itself. This is why it is recommended to always use `connection.getModel` and `connection.getModels`, because no matter how your models were bound, these methods will always give you the correct version of your models.

As just stated however, these two methods don't work very well in TypeScript, because TypeScript can't know the return type of your models. So now if you rely on model imports to access your models, they must be bound, so that we get that `static _getConnection` method injected into all your model classes. But... this breaks unit testing. Dang!

I don't currently have a great solution to this problem, **and am certainly open to any creative ideas anyone might have to fix it**. One idea I have been toying around with is using the [node:async_hooks](https://nodejs.org/docs/latest-v16.x/api/async_context.html) module to resolve some of these issues, by providing the connection via an `async_hooks` context. However, this comes with its own set of problems, not the least of which is a considerable hit to performance, and it is still difficult to use an `async_hooks` context in unit tests.

Hopefully at least understanding the problem will help you come up with a creative solution for your specific application code. You can always provide a `static getConnection` method on your model classes (notice no underscore on this method name, as `static _getConnection` is reserved for injecting connections), and then from this method figure out whatever works best for your application and unit testing to provide a valid `connection` to your models.

## Subclass your connection and model classes

Another option is to subclass your connection, and provide a custom `ModelBase` class that all your other models inherit from, and in these subclasses overload `getModel` and `getModels` methods to properly return your model types.

## Ditch TypeScript

Another option that will probably turn me into a heretic just by mentioning it is to ditch TypeScript altogether and just use vanilla JavaScript. This way, you can easily call `connection.getModel` and `connection.getModels` anywhere without having type errors, and away you go without issues. Just make sure you ALWAYS use these methods everywhere, and it is recommended that you prefix your model classes with an underscore, or use some other mechanism to ensure you never use a model class directly by accident without it coming from one of these two methods first.

## Get a better test runner

As all the issues discussed in this page are generally encountered during unit testing, where more than one connection is in use at the same time, maybe get a better test runner, one that won't run tests randomly, or one that runs each test suite in its own VM/context. This won't entirely solve your problem however if you choose **not** to bind your models to a connection, as some of your application code might still be using different copies of your models than the connection is using.

## Provide your own connection globally

One other possible solution is to globally instantiate your connection, and re-export all your models through a single "models" module that will bind all your models to the global connection. This will fix all the issues listed on this page, even for unit testing. You might for example have a *different* connection you globally bind all your models to while testing, such as to a `SQLite` connection. This method might look something like below:

`application-models.ts`
```javascript
import { globalConnection } from './my-connection-creator'
import * as MyApplicationModels from './models';

globalConnection.registerModels(MyApplicationModels);

const models = globalConnection.getModels();
const MyBoundApplicationModels = {
  User: models.User as User,
  ...
};

export default MyBoundApplicationModels;
```

And then you would always import all your models from `application-models` always, and away you go. You could then create a second `test-application-models` that does the same thing, but using a different connection for your unit tests.

One side effect of this possible solution is that now your unit tests will all be sharing a connection, so you need to be extra careful to properly clean up data, to prevent data "leaks" in your tests from other tests that are sharing the same database.
