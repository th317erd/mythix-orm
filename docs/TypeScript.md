Mythix ORM is fully typed and ready for TypeScript.

However, there are some important things to know when using TypeScript with Mythix ORM:

  1. Mythix ORM was written in vanilla JavaScript, and it will stay that way. I personally prefer to work in vanilla JavaScript, and have no plans to convert the entire code base to TypeScript. I am however happy to continue to support the TypeScript community as well as I am able to with my limited knowledge of TypeScript.
  2. I am not master guru of TypeScript. If you find something incorrect, or that could be better written in the TypeScript definition files, then please submit a PR, and I will be happy to merge it and release a new version.
  3. Some things are a little funky until the TypeScript authors fix the issue with `this` in static methods: [issue #5863](https://github.com/microsoft/TypeScript/issues/5863).
  4. Mythix ORM was designed to heavily rely on `connection.getModel` and `connection.getModels`. However these methods are quite difficult to use with TypeScript. It is recommended that you overload these methods in your own classes to provide the correct model types (via an interface) for your application. You can also instead just import your models, and use them directly, and skip using these methods. If you import and use models by reference directly, be aware that the auto-reloading feature for your models will no longer work.
  5. If you are directly importing and using model references, you need to be aware of connection binding, and some issues that can arise with models and their bound connections, especially for unit testing. See the [Connection Binding](./ConnectionBinding) page for more details on connection binding, some of the issues you might encounter, and possible solutions to those issues.
