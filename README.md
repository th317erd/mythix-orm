# mythix-orm

Mythix ORM aims to replace Sequelize and the few other terrible solutions that the poor destitute Node community has to work with. Mythix ORM has been designed to replace all current ORMs for Node, with a focus on what is lacking in the community, namely good engineering, good documentation, and ease of use.

Mythix ORMs feature set includes:
  1. An advanced, seamless, and powerful (yet simple) query engine that is easy to use, and works across database drivers, even for No-SQL databases.
  2. Powerful model classes and helpers that don't violate good design patterns, stay out of your face, and have no undocumented auto-magic built in. The model system is also designed to work seamlessly across different databases, including No-SQL databases.
  3. Simple, clean, and slim... Mythix ORM isn't intended to be a sledge hammer, nor a 'batteries included' framework. Instead, it is designed to be a useful tool, and was designed to be easily extended. It can be used for large enterprise applications, or it can be used as a simple slim layer to interact with different databases in a human-friendly way.
  4. A modular design. Instead of being a large bloated library that attempts to handle every database and every type of operation, it instead only provides exactly what you need. Mythix ORM is itself just a base connection, a query engine, and a model and type system. That is all. To interact with databases you can choose between any number of drivers for Mythix ORM (coming soon!), and can use community-built plugins for adding features (or simply write your own!).
  5. A deliberate design to be extended and added onto. Easily modify the Query Engine to add more features, create your own database driver, modify how models behave, or add your own custom data types. The sky is the limit!
  6. Complete feature parity (and soon greater functionality) then all existing ORMs for Node. Model validation, hooks, model attributes and data types, model relations, support for multiple databases, an advanced query engine, transactions, transactions inside transactions, useful utility methods, an extensible type system, virtual types, an extensible query generator, support for older Node versions, support for multiple connections and multiplex connections (at the same time), and more!

Mythix ORM is still in its very early stages, and is looking for users! It is stable, and currently has native support for SQLite and PostgreSQL. A mongo driver will be added next, and after that MySQL. If you want to help then drop me a line! All help is welcome.

## Install

```bash
npm i --save mythix-orm mythix-orm-sqlite
```

## Documentation

Check out the [WIKI](https://github.com/th317erd/mythix-orm/wiki) for documentation.

## Getting started

Just start creating models!

```javascript
const { Model, Types }  = require('mythix-orm');
const { SQLiteConnection }  = require('mythix-orm-sqlite');

class User extends Model {
  static fields = {
    id: {
      type: Types.XID(),
      defaultValue: Types.XID.Defaults.XID,
      allowNull: false,
      primaryKey: true,
    },
    email: {
      type: Types.STRING(128),
      allowNull: false,
      index: true,
      unique: true,
    },
    firstName: {
      type: Types.STRING(64),
      allowNull: false,
      index: true,
    },
    lastName: {
      type: Types.STRING(64),
      allowNull: false,
      index: true,
    },
  };
}

// Entry point
(async function() {
  // Define a connection, and "bind" our models to it
  let connection = new SQLiteConnection({
    models: [
      User,
    ],
  });

  // Fire up our connection
  await connection.start();

  // Create our tables needed for our User
  // model in SQLite
  await connection.createTables([ User ]);

  // Now we can store and load a user
  let user = new User({
    email: 'test@example.com',
    firstName: 'Test',
    lastName: 'User',
  });

  // Store user
  await user.save();

  // Reload user by querying on the
  // user's email address
  user = await User.where.email.EQ('test@example.com').first();

  // Serialize to JSON
  console.log('My user: ', JSON.stringify(user, undefined, 2));

  // Shutdown our connection
  await connection.stop();
})();
```

## Notes

1. The [WIKI](https://github.com/th317erd/mythix-orm/wiki) is still being worked on. Most of the documentation is complete, but there is still a lot more to write. Documentation is the main focus right now. If you have any questions, feel free to drop a line, or open an issue! We will be happy to answer any questions. We aren't "done" until our documentation is pristine.
2. Right now there are only database drivers for [SQLite](https://www.npmjs.com/package/mythix-orm-sqlite) and [PostgreSQL](https://www.npmjs.com/package/mythix-orm-postgresql). More are planned, with a Mongo driver likely to land next, followed by MySQL. Help wanted!
3. Check out the [Mythix](https://www.npmjs.com/package/mythix) web-app framework. It is also still in active development, and the documentation is poor (to say the least), but it is up and coming, and will soon have fantastic documentation, and even though still in active development is fully functional. To get started try `npx mythix-cli create 'Test App'`

## Goals

The `Mythix` suite of technologies are being developed to give a rock-solid full-stack framework to build web-apps on Node. I got tired of the piecemeal garbage that currently exist in the Node ecosystem for building apps. My end goal is to have `Mythix` technologies take the Node community by storm, providing top-notch technologies for developers to create amazing things. Get involved with me, and let's change the world for the better!
