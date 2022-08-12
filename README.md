# mythix-orm

ORM for Mythix framework

Mythix ORM aims to replace Sequelize and the few other terrible solutions that the poor destitute Node community has to work with. Mythix ORM is not yet quite ready for prime time however, so please check back soon!

What to expect while you are waiting:
  1. Advanced, seamless, and powerful (yet simple) query engine that is easy to use, and works across database drivers, even for No-SQL databases. Here is a simple example to fetch users and their roles: `let users = await User.where.id.EQ(Role.where.userID).firstName.EQ('Mythix').lastName.EQ('ORM').Roles.name.EQ('superuser').PROJECT('User', 'Role').all();`
  2. Powerful model classes and helpers that don't violate good design patterns, stay out of your face, and have no undocumented auto-magic built in. The model system is also designed to work seamlessly across different databases, including No-SQL databases.
  3. Simple, clean, and slim... Mythix ORM isn't intended to be a sledge hammer, nor a 'batteries included' framework. Instead, it is designed to be a useful tool, and was designed to be easily extended. It can be used for large enterprise applications, or it can be used as a simple slim layer to interact with different databases in a human-friendly way.
  4. Mythix ORM is modular by design. Instead of being a large bloated library that attempts to handle every database and every type of operation, it instead only provides exactly what you need. Mythix ORM is itself just a base connection, a query engine, and a model and type system. That is all. To interact with databases you can choose between any number of drivers for Mythix ORM, and can use community built plugins for adding features (or simply write your own!).
  5. Mythix ORM is designed from the ground-up to be extended/modified. Want to change the nature of the Query Engine? Just extend from it and away you go! Want to change the way models behave? No problem! Want to make your own connection? Go for it! Want to add your own custom data types for models? Super easy. Every part of Mythix ORM is designed to be swapped out in a non-global way so that its feature set can be extended and added onto.
  6. Has complete feature parity (and soon greater functionality) then all existing ORMs for Node. Model validation, hooks, model attributes and data types, model relations, support for multiple databases, an advanced query engine, transactions, transactions inside transactions, useful utility methods, an extensible type system, virtual types, an extensible query generator, support for older Node versions, support for multiple connections and multiplex connections (at the same time), and more!

Stay tuned! Mythix ORM should be released and fully documented by Q1 of 2023!
