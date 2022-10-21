Models encapsulate the functionality of a single entity. For example, a `User`, `Product`, or a `ScientificMeasurement`.

In Mythix ORM, models are defined by class, inheriting from the `Model` class exported by Mythix ORM. Models have a few moving parts built-in to make the developers life nicer. At a high level, a model has the following functionality:
  1. **Models define fields.** These are useful for mapping the data to a persisting store, such as a database, and also assist in validating, serializing, and deserializing data. In Mythix ORM, fields are backed by the <see>Field</see> type, and are defined simply by adding a `static fields` property to your model class.
  2. **Models define relationships.** There are two categories of fields, "concrete" and "virtual". Concrete fields are fields which are backed by storage directly. For example, a field such as `id`, `username`, `email`, `phone`, etc... are all examples of concrete fields. Concrete fields have types like `VARCHAR`, or `INTEGER` in common databases for example. Virtual fields on the other hand are not directly backed by any storage, and instead define relationships between fields, or provide other functionality to the model. Examples of virtual fields are the `Types.Model` and `Types.Models` fields. These are virtual fields that define model relationships. Notice how there is a singular version (for defining 1x1 relationships), and a plural version (for defining one to many, or many to many relationships). Another form of relational field is the concrete field `Types.FOREIGN_KEY`. This is a concrete field because it is actually backed by storage, however it is also a relational field because it will target other fields on other models. The fields in a model can be of any type. Notice how concrete types are generally defined as UPPER_CASE, whereas virtual types are generally defined as camelCase.
  3. **Models track the state of their fields.** If you set a field, then the model will mark itself as "dirty", and record the previous value of the field internally. This is used later on by persisting methods to know what has changed, and what to persist to the underlying database. It is also commonly useful for the end user to know what fields on a model have changed and how. Unlike concrete fields, virtual fields generally will not effect the if a model is dirty or not. This model system of tracking field states and values is called the "dirty system". A model is termed "dirty" if one or more of its fields is updated to a different value. A model is termed "clean" if no fields have been modified.
  4. **Models assist in serializing and deserializing themselves.** Not only to the database, but also through other processes, such as conversion to and from JSON, or for representation when debugging. It is the job of a <see>Connection</see> and generally also a <see>QueryGenerator</see> to break the model down to its respective fields and persist and load those from a database.
  5. **Models can be used to query data.** Models can be used to generate queries against any given database. This happens through the <see>QueryEngine</see> interface. Not only can load or select queries be created from models, but models are also used to define table or bucket schemas, relationships, and are also used to know how to destroy or update data.
  6. **Models have life-cycle hooks.** Models have certain life-cycle hooks which are similar to events. These can be used to know what is happening to any given model. For example, there is an `onBeforeSave` hook that will be called any time before a model is inserted or updated. There are also hooks like `onBeforeCreate`, `onAfterSave`, `onAfterCreate`, etc... Mythix ORM doesn't have any hooks for destroy operations due to the performance overhead (and possible data syncing issues) these impose.
  7. **Models define and provide functionality.** A model might for example contain a method to generate a report, update roles for a given entity, connect to a third-party service, or any number of things.
  8. **Models ultimately define a single entity.** Ultimately it is the purpose of a model to define a single entity, and all the operations or "actions" that can be applied to that entity. For this reason models should always be named singularly. For example, you would have a `User` model, not a `Users` model. Models commonly define static methods, and so are also namespaces. A model should define not only its own structure, but also its relationships to other data, and any functionality it exposes to interact with the entity it defines.

## How to define a model

There are two necessary ingredients to define a model in Mythix ORM. These are 1) The Model itself, and 2) A connection. A <see>Connection</see> in Mythix ORM is like an "application" in other frameworks. It defines how queries operate, stores all the models for the application, works between models based on their relationships, and is the interface between your models and your database (or other services).

To define a model class, simply define a class that extends from `Model`, and define its `static fields` property:

```javascript
const { Model, Types } = require('mythix-orm');

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
```

That is all that is needed to define a model in Mythix ORM. However, though this could be useful and used all by itself, it becomes much more when we also start using a connection. Let's select the `mythix-orm-sqlite` connection for our examples here.

```javascript
const { Model, Types }      = require('mythix-orm');
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

# Model fields

Model fields are always defined as the `static fields` property on your model class. Fields can be defined using a few different types and methods. For example, fields can be an `Array`, an `Object`, a `Map`, or `Set`. Generally you will see fields defined as an object, because this is the simplest way to define them. However, if order is important to you, then you can use an `Array`, a `Map`, or a `Set`.

When using an `Array` or a `Set` to define the model's fields, you **must** define a `fieldName` property for each field. For the `Object` and `Map` types, this is defined by the key for each field, though if a `fieldName` property exists on a field, that will take priority over the key.

For example, with an array, we must define a `fieldName` for each field:

```javascript
const { Model, Types } = require('mythix-orm');

class User extends Model {
  static fields = [
    {
      fieldName: 'id',
      type: Types.XID(),
      defaultValue: Types.XID.Defaults.XID,
      allowNull: false,
      primaryKey: true,
    },
    {
      fieldName: 'email',
      type: Types.STRING(128),
      allowNull: false,
      index: true,
      unique: true,
    },
    {
      fieldName: 'firstName',
      type: Types.STRING(64),
      allowNull: false,
      index: true,
    },
    {
      fieldName: 'lastName',
      type: Types.STRING(64),
      allowNull: false,
      index: true,
    },
  ];
}
```

Because the `static fields` property of a model is simply an object, fields can and should be merged. For example, if you would like `createdAt` and `updatedAt` fields on every one of your models, then simply create a "base" model that all your other models inherit from. For example:

```javascript
const { Model, Types } = require('mythix-orm');

class ModelBase extends Model {
  static fields = {
    createdAt: {
      type: Types.DATETIME,
      defaultValue: Types.DATETIME.Defaults.NOW,
      allowNull: false,
      index: true,
    },
    updatedAt: {
      type: Types.DATETIME,
      defaultValue: Types.DATETIME.Defaults.NOW.UPDATE,
      allowNull: false,
      index: true,
    },
  };
}

// Inherit from our "base" model
class User extends ModelBase {
  static fields = {
    // Merge the parent fields into my fields
    ...ModelBase.fields,
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
```

Mythix ORM *will not* automatically merge fields for you simply through class inheritance. You must manually "merge" fields from the base model (or elsewhere) yourself. However, since `static fields` can be one of `Array`, `Object`, `Map`, or `Set` types, it can become difficult to merge fields from other sources. Because of this, Mythix ORM provides a `static mergeFields` method on `Model` itself. Our above example would be better written as:

```javascript
const { Model, Types } = require('mythix-orm');

class ModelBase extends Model {
  static fields = {
    createdAt: {
      type: Types.DATETIME,
      defaultValue: Types.DATETIME.Defaults.NOW,
      allowNull: false,
      index: true,
    },
    updatedAt: {
      type: Types.DATETIME,
      defaultValue: Types.DATETIME.Defaults.NOW.UPDATE,
      allowNull: false,
      index: true,
    },
  };
}

// Inherit from our "base" model
class User extends ModelBase {
  // Use the proper "mergeFields" method to merge
  // any type of fields into these fields
  static fields = ModelBase.mergeFields({
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
  });
}
```

The `mergeFields` method will take the `static fields` defined on the class it is called from, and will merge those with the provided fields. If a `fieldName` matches between the parent class and the provided fields, then the field itself will be merged. In short, this is not a simple "concat" or spread operation, but even for `Array` and `Set` types, the fields will be individually merged if they have matching `fieldName` properties.

This method will allow you to conveniently and consistently merge fields from other sources, regardless of the format those fields are defined as. `mergeFields` can also be used to clone fields, as it always will make copies of all fields it operates on (from the source model, plus any provided fields). Providing fields to this call is optional. If no fields are provided as arguments to the call, then the fields defined on the model it is called from are simply cloned.

Please note that there is no important "magic" in the `mergeFields` method, except that it will be able to iterate and merge fields across different data types. If you want to define all your model fields as objects and use the spread operator to merge them, go for it! If you want to define all model fields as arrays and `concat` them, do so!

# Defining the model and underlying table/bucket name

Each model can define how it interacts with the database it will be stored in. A model can define a "table name", a "model name", and "column names" for each field. Mythix ORM will not mutate model or field names for you to try and match some "standard". Instead, it will do exactly as you define. Mythix ORM is a WYSIWYG framework... What You See Is What You Get... whatever is in-front of your eyes in your IDE is likely exactly what you will find in every layer all the way to the database (unless your own application code modifies things). What this means is that if you name a certain field `firstName`, it will also be `firstName` in the underlying database, without any mutations applied to it. The only modification that Mythix ORM will ever do is when it needs the "plural name" of your model. In this case, it will use the singular model name defined by default (or defined by you) to generate a plural model name.

In Mythix ORM defining the table name, model name, and each column name is optional. By default, the model name of your model will be the name of the class used to define the model. The plural version of this same name is then used as the table name for the model in the underlying database. Further, each `fieldName` also by default defines the column name for that field.

All of these can be modified however. For example, even though the model name defaults to the class name used to define the model, you can set a completely different name by supplying a `static getModelName` method on your model class.

In the following example, we name our model a `Member` instead of a `User`:

```javascript
const { Model, Types } = require('mythix-orm');

class User extends Model {
  static getModelName() {
    return 'Member';
  }
}
```

You can also define a plural model name to help Mythix ORM out:

```javascript
const { Model, Types } = require('mythix-orm');

class User extends Model {
  static getModelName() {
    return 'Member';
  }

  static getPluralModelName() {
    return 'Members';
  }
}
```

You can also define your own table name for the model (or bucket name if you are using a No-SQL database):

```javascript
const { Model, Types } = require('mythix-orm');

class User extends Model {
  static getModelName() {
    return 'Member';
  }

  static getTableName() {
    return 'service_members';
  }
}
```

Mythix ORM always calls data-spaces "tables", even if they are actually "buckets" or something else in your underlying database. Here we are telling Mythix ORM that our `Member` model is stored in the `service_members` table in the underlying database.

Lastly, you can define a custom column name for each field on your model:

```javascript
const { Model, Types } = require('mythix-orm');

class User extends Model {
  static getModelName() {
    return 'Member';
  }

  static getTableName() {
    return 'service_members';
  }

  static fields = {
    id: {
      // Define a custom column name in the underlying database
      columnName: 'id',
      type: Types.XID(),
      defaultValue: Types.XID.Defaults.XID,
      allowNull: false,
      primaryKey: true,
    },
    email: {
      // Define a custom column name in the underlying database
      columnName: 'email',
      type: Types.STRING(128),
      allowNull: false,
      index: true,
      unique: true,
    },
    firstName: {
      // Define a custom column name in the underlying database
      columnName: 'first_name',
      type: Types.STRING(64),
      allowNull: false,
      index: true,
    },
    lastName: {
      // Define a custom column name in the underlying database
      columnName: 'last_name',
      type: Types.STRING(64),
      allowNull: false,
      index: true,
    },
  };
}
```

It is important to note that in Mythix ORM you will personally **never** use or encounter the column name in your code (except for where you define it). Mythix ORM always requires that you use the **`fieldName`** when specifying fields, and **never** the **`columnName`** of a field for all its operations. This is to ensure a certain level of abstraction away from your storage engine. So for example, never use a column name in a query or to try and fetch a model attribute, as you will be sorely disappointed with the result.

Mythix ORM will convert your `fieldName` to `columnName` when it interacts with the database, and will convert the `columnName` back to a `fieldName` when it loads data from the database.

## Validating fields

In Mythix ORM you can validate field values before a store operation. This makes it easy to ensure data integrity, and that nothing is running afoul in your application. To validate fields, simply supply a `validate` property to any field you choose:

```javascript
const { Model, Types } = require('mythix-orm');

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
      validate: (value) => {
        if (value == null)
          throw new Error('"email" is required');

        if (value.indexOf('@') < 0)
          throw new Error('Invalid "email" address');
      },
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
```

Any `validate` property defined **must** be a method (patterns, and regular expressions are not supported--outside your validate method that is). A `validate` method can be asynchronous. A `validate` method will "pass" validation if no error is thrown in the method.

You can also define your own <see>Model.onValidate</see> method on the model itself to validate your model however you choose. Note that field validation happens via the default implementation of the `onValidate` method on the `Model` class itself. If you overload this method, you must either completely provide your own model validation, or call `super.onValidate` inside your overloaded method to fall-back to Mythix ORM default model validation. It is also important to note that the default `onValidate` method is called from the default `onBeforeSave` hook, so if you overload (or skip) this hook, make certain you call `super.onBeforeSave`, or directly call `this.onValidate` yourself.

The default `onValidate` method Mythix ORM provides will collect and return the results from all `validate` methods on fields. Mythix ORM itself completely ignores these return values from field validators (it only cares about exceptions thrown), however it still collects and returns the results, in case the developer needs them.

## Custom field getters/setters

In Mythix ORM, every field defined for the model will be turned into a getter/setter for the model instance. This is so that Mythix ORM can provide the awesome functionality it provides. For example, if you were to set the `firstName` field on a `User` model instance like: `myUser.firstName = 'Something Else';`, then this will call the setter for the `firstName` property on the model instance. The setter updates the field to the new value specified, and simultaneously marks this field as "dirty".

These internal getters/setters for each field are also used to provide *custom* getters and setters that the end user can define for each field. For example, we can use custom getters and setters on a field to create our own "enum" type:

```javascript
const { Model, Types } = require('mythix-orm');

const USER_TYPE_ENUM = {
  MEMBER: 1,
  ADMIN: 2,
  SUPERADMIN: 3,
};

class User extends Model {
  static fields = {
    id: {
      type: Types.XID(),
      defaultValue: Types.XID.Defaults.XID,
      allowNull: false,
      primaryKey: true,
    },
    userType: {
      type: Types.INTEGER(),
      allowNull: false,
      index: true,
      // Define a custom getter/setter
      // to map incoming and outgoing values
      // to our "enum" map.
      get: ({ value }) => {
        for (let [ key, enumValue ] of USER_TYPE_ENUM) {
          if (key === value)
            return key;

          if (enumValue === value)
            return key;
        }

        throw new TypeError(`Bad "enum" value provided: "${value}"`);
      },
      set: ({ value, set }) => {
        // Handle "key" case, i.e. `"MEMBER"`
        if (Object.prototype.hasOwnProperty(USER_TYPE_ENUM, value))
          return set(USER_TYPE_ENUM[value]);

        // Handle "raw" case. i.e. `1`
        if (Object.values(USER_TYPE_ENUM).indexOf(value) >= 0)
          return set(value);

        throw new TypeError(`Bad "enum" value provided: "${value}"`);
      },
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
```

Here you can see we created our own "enum" for our `userType` field. First, we define this field as an `INTEGER` type, so we know it will be stored in the database as an integer. Second, we create a map that maps key names to integer values. We then define our own `get` (getter) and `set` (setter) methods on the field, which map back and forth between these integer values and their corresponding "name", and visa-versa. With the above example, we could define a user type by string: `myUser.userType = "MEMBER";`, and it would map the `"MEMBER"` string to `1` using our defined enum map, and then be stored to the database as `1`. When we go to get this attribute on the model, such as `let userType = myUser.userType;`, then our `get` (getter) method will be called, see that the value is an `1`, and return `"MEMBER"`.

Getters and setters can intercept any get or set of the field, and return (or set) a completely different value that you define.

**It is vitally important that you never access the attribute directly upon the instance inside these methods.** Because these methods are called from the field getter/setter of the model instance, if you try to for example access the field directly in the getter:

```javascript
get: () => {
  let value = this.userType;
},
```

Then you will encounter infinite recursion and a `MAX CALL STACK` error will be thrown. What happens is that `this.userType` calls the model instances getter for this attribute, which calls your custom `get` method for the field, which accesses the `this.userType` attribute, which calls the model instances getter for this attribute, which calls your custom `get` method for the field, which accesses `this.userType` attribute, which... you get the idea... and infinite loop.

The *proper* way to get or set the value on your field inside a `get` or `set` property on the field is to use the provided `get` and `set` methods that are passed into the method. For example, our broken code above could be fixed as follows:

```javascript
get: ({ get }) => {
  let value = get(); // Get the field value in a safe way
},
```

This is also true for `set`, where you should *always* call the provided `set` method to set the value of your field:

```javascript
get: ({ get }) => {
  let value = get(); // Get the field value in a safe way
},
set: ({ set, get }) => {
  set(`prefix_${get()}`); // Set the field value in a safe way
},
```

By sticking with the provided `get` and `set` methods passed to your functions, you will escape any shenanagin leprechauns hiding in the shadows. The provided `get` method will always return the exact same value as provided by the `value` argument.

# Model relationships (associations)

Models can be related to each other. If they are, then a <see>Connection</see> **must** be used. A <see>Connection</see> instance is used to fetch related models, and walk model relationships.

A model can be related to another model in one of three ways:

  1. Through a `FOREIGN_KEY` relationship.
  2. Through a 1x1 relationship defined by the singular `Model` virtual type.
  3. Through a one to many, or many to many relationship, as defined by the plural `Models` virtual type.

For example, to define a relationship using foreign keys:

```javascript
const { Model, Types } = require('mythix-orm');

class Role extends Model {
  static fields = {
    id: {
      type: Types.XID(),
      defaultValue: Types.XID.Defaults.XID,
      allowNull: false,
      primaryKey: true,
    },
    // The name of the role, i.e. "admin",
    // "member", or "superadmin"
    name: {
      type: Types.STRING(32),
      allowNull: false,
      index: true,
    },
    // User that owns this role,
    // as a foreign key
    userID: {
      type: Types.FOREIGN_KEY('User:id', {
        // When the user is deleted or updated
        // also delete or update this role
        onDelete: 'CASCADE',
        onUpdate: 'CASCADE',
      }),
      allowNull:    false,
      index:        true,
    },
  };
}

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
```

As you can see, we use the `Types.FOREIGN_KEY` type to define a foreign key. The foreign key type is simple: It is created on its source (child) table as the exact same type of the field it points to on the target (parent) table. Since in our case we defined (as the first argument) that it targets the `'User:id'` column (the `id` field in the `User` table), it will itself be the same type as this field, a `Types.XID` type. Mythix ORM will then apply the proper constraints to this field when the table/bucket is created in the underlying database.

Mythix ORM also uses this field to know how to update related values on store operations. For example, if instead of having a one to many relationship as we have defined here, if we instead had a one to one relationship, such as a `primaryRoleID` field on the `User` model, then the `primaryRoleID` field for the `User` will be updated to the `id` of a `Role` when that role is saved for a given user. In short, Mythix ORM will walk all foreign keys during a store operation, and if two models are being stored that are related to each other, then Mythix ORM will cross-update their related fields with each other, both before and after the store operation. Mythix ORM updates related fields _before_ store and _after_ store because if you are using client-generated values, for example, an `XID` type, then it can update the foreign key field before it stores the model, saving an extra trip to the database. If however you are using auto-incrementing ids, then Mythix ORM is not able to update foreign fields until it has the proper id from the database, and so will update the related fields _after_ models are persisted. Mythix ORM will do its best to persist the models in the "correct order", so for example, if the `Role` model has a foreign key to `User`, then the `User` model will be persisted first, since the `Role` model needs the `id` field from the `User` model in order to properly save. In theory, the only time Mythix ORM might get the order incorrect is when you have cyclic relationships.

Now that we have used a foreign key in our models, let's go ahead and expand on this a bit, making it more useful by creating a virtual many to many relationship.

Example:

```javascript
const { Model, Types } = require('mythix-orm');

class Role extends Model {
  static fields = {
    id: {
      type: Types.XID(),
      defaultValue: Types.XID.Defaults.XID,
      allowNull: false,
      primaryKey: true,
    },
    // The name of the role, i.e. "admin",
    // "member", or "superadmin"
    name: {
      type: Types.STRING(32),
      allowNull: false,
      index: true,
    },
    // User that owns this role,
    // as a foreign key
    userID: {
      type: Types.FOREIGN_KEY('User:id', {
        // When the user is deleted or updated
        // also delete or update this role
        onDelete: 'CASCADE',
        onUpdate: 'CASCADE',
      }),
      allowNull:    false,
      index:        true,
    },
  };
}

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
    // Add this new field, which creates a virtual
    // many to many relationship with the Role model.
    //
    // Notice how a type can always be used directly as
    // the field definition, instead of defining an object
    // with a "type" property.
    roles: Types.Models('Role', (context, connectionModels, ...userArgs) => {
      // Get the model we need from the connection,
      // which is conveniently passed the models
      // to us here
      let { Role } = connectionModels;

      // Get the "self", which is the model instance
      // calling this method
      let { self } = context;

      // Now return a relationship query
      return Role.where
        .userID
          .EQ(self.id)
        .AND
        // The user themselves may have added to the query
        // so merge in the user provided query here
        .MERGE(userArgs[0]);
    }),
  };
}

// ... later on
// get roles for the "first" user in the database
let user = await User.first();
let userRoles = await user.getRoles();

// Since we added the `.MERGE(userArgs[0])` above to
// our relationship, we can pass in `userArgs`, the
// first which we have defined as an extra query the
// user can add to the base query. This allows the user
// to modify the relationship query, such that the final
// result for this example would be:
// Role.where
//   .userID
//     .EQ(self.id)
//   .AND
//   .name
//     .EQ([ 'admin', 'superadmin' ])
//   .PROJECT('Role:name')

let isAdmin = ((await user.getRoles(Role.where.name.EQ([ 'admin', 'superadmin' ]).PROJECT('Role:name'))).length > 0);
```

Most other frameworks define related fields in tedious ways, where you must define the target model name, if it is a one to one, one to many, or many to many relationship, if it is polymorphic, what fields to join on, what through tables to go through, etc... tedious...

Mythix ORM instead gets straight to the point by requiring the user to return a _query_ for related fields. This makes it crystal clear simply by looking at the method what is happening, removes the tedium, and also allows for very complex operations, including table joins, polymorphism, and even allowing the user to add on to the relationship query at the call-site. These relationship "query generating" methods can also be asynchronous, so for example, if you wanted to query the database first for a list of values to provide to your base query, you can.

All virtual fields added by `Types.Model` or `Types.Models` become "relationship sets", that have a number of different methods that can be used to interact with the set. For example, with the code we have above, we could also add to the user's roles simply by calling `user.addToRoles({ name: 'admin' })`, or `user.destroyRoles(role)`, or `user.setRoles(newRoles)` to completely overwrite all user roles with a new set. For a complete list of methods available in sets, please refer to <see>ModelType</see> and <see>ModelsType</see>. It might also be useful to check out the article on associations here: [Associations](./Associations).

One method that is always available for both the <see>ModelType</see> and the <see>ModelsType</see> is `queryFor*`. So for example, given our above code, we could call `let rolesQuery = user.queryForRoles();`. This would return the _relationship query itself_, instead of interacting with any models. This allows you to fetch a relationship query, and then add onto it, or modify it before you execute the query.

Polymorphic relationships are supported in Mythix ORM, however they are a little more "manual" than other frameworks. To define a polymorphic relationship, simply create two columns, one that stores a foreign id (that is *not* a foreign key, since it can point to many different tables), and one that stores the model type. You can then manually query against the stored model type and id.

Please checkout the [Associations](./Associations) article for an in-depth dive into associations in Mythix ORM.
