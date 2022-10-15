Mythix ORM makes associations really easy.

In many other ORMs you need to define how two models are related... and what fields are related on those models, and if the relationship is polymorphic, and if it should load related models while accessing the relation, and if there is a default scope that should be applied, and...

Lame.

Let's not repeat that design pattern.

Instead, in Mythix ORM, you define relationships *with* queries. This means all that garbage you need to manually define in other ORMs is consistently and conveniently defined all in one place: the relationship query itself. This can include a polymorphic relation, a projection and related models to load, a "default scope", and everything else that can be done in other ORMs, in a simple and intuitive interface.

For this to work, when you define a relationship using the `Types.Model` or `Types.Models` types, you always define two parameters: 1) The target model of the relationship, and 2) The "query provider", which is a simple method that returns the query for the relationship.

## Getting started

Fields in a Mythix ORM model can be either "virtual" or "concrete".

Concrete fields are backed by storage (the database), and will have a direct value they can be associated with.

Virtual fields are not backed by storage (at least not directly), and instead will dynamically fetch their value.

A field in Mythix ORM with be "concrete" or "virtual" simply based on its type. For example, the types `Types.Model` and `Types.Models` will by nature define a virtual field. These types define relations to other tables, and so don't have a 1x1 concrete field in the DB themselves, and instead will pull data based on the relationship defined by the type.

Before we get started, there are a few things to keep in mind:

  1. `Types.Model` is used for a 1x1 relationship
  2. `Types.Models` is used for a one-to-many, or a many-to-many relationship
  3. These two types specify field relationships
  4. In field relationships, it is required to specify a target model
  5. All concrete fields must be manually defined on all models. There is no "automagic" or "hidden" fields in `mythix-orm`. Said another way, **you must manually define ALL table columns/model fields, always, without exception**... none will be auto-defined for you. If you want auto-defined fields, such as "createdAt", and "updatedAt" for all your models, then the recommendation is to define a BaseModel that all of your other models inherit from.

## Example #1 - Defining a 1x1 relationship

Let's say you want to have a Users table, and have each user have a single Role.

This is a 1x1 relationship, with one User to one Role.

You could define the association like so:

```javascript
class User extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'firstName': {
      type:         Types.STRING(64),
      allowNull:    true,
      index:        true,
    },
    'lastName': {
      type:         Types.STRING(64),
      allowNull:    true,
      index:        true,
    },
    // Define a foreign key relationship
    // to the Roles table, targeting the
    // "id" column of that table.
    'roleID': {
      type:         Types.FOREIGN_KEY('Role:id', {
        onDelete: 'SET NULL',
        onUpdate: 'SET NULL',
      }),
      allowNull:    true,
      index:        true,
    },
    // This defines a "virtual" field,
    // that will be used to define
    // methods to interact with the role
    'role': {
      // Relationships are defined by a query.
      // Simply generate the query for the data
      // you want the relationship to interact
      // with, and away you go! The first model
      // specified in the query is the "root"
      // model, and must always match the target
      // model specified in the first argument.
      // "self" is always the origin model instance.
      // So for example, when we want to get the role
      // for this user, we would do a:
      // `await user.getRole()`, so "self" would be
      // "user" (the origin model instance).
      //
      //                        target   query provider
      type:         Types.Model('Role', ({ Role, self, userQuery }) => {
        return Role.where.id.EQ(self.roleID).MERGE(userQuery);
      }),
    },
  };
}

// Now let's not forget to define
// our Role model
class Role extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'name': {
      type:         Types.STRING(64),
      allowNull:    false,
      index:        true,
    },
    // This defines a "virtual" field,
    // that will be used to define
    // methods to interact with the user
    'user': {
      // Now we simply define the relationship
      // in reverse, using a simple query.
      type:         Types.Model('User', ({ User, self, userQuery }) => {
        return User.where.roleID.EQ(self.id).MERGE(userQuery);
      }),
    },
  };
}
```

Let's look at the `User` model `role` field first, which is of the type:

```javascript
type: Types.Model('Role', ({ Role, self, userQuery }) => {
  return Role.where.id.EQ(self.roleID).MERGE(userQuery);
}),
```

This may seem confusing at first, until you understand the pattern here. The pattern is fairly simple. The `Types.Model` is singular (`Models` on the other hand, if used, defines a one-to-many, or many-to-many relationship), so right away we know this is going to fetch a single model. It is a 1x1 relationship. Next, we have the two arguments we provide. It helps a lot to think of these as "target model", and "query provider"... as in, "What is the target model we are interacting with, and how do we interact with it?" It also helps quite a bit to think of this from the perspective of "this model instance", instead of "this table", or "this relationship". The relationship is always from an instance of a model... a single instance (or a single row in the DB, if that helps instead).

With this in mind, the above model definitions should make a little more sense:

  1. From the perspective of this specific user, what are we targeting?
  2. We are targeting the `Role` model (the first argument)
  3. Great! Now, how are we interacting with this model?
  4. Oh, that is easy, we are interacting with a `Role` model where the `Role.id` equals `user.roleID` (self.roleID).

Simple!

The last thing you might be wondering is "What is this `userQuery` garbage?". Good question! The last part, the `.MERGE(userQuery)` is simply merging in any user query that might have been passed along with the call. What is a `userQuery`? A user query is a query that the caller provides to any relationship method call. For example, if we made the call `await user.getRole(Role.where.name.EQ('admin'))` then the `userQuery` passed into the query provider would be `Role.where.name.EQ('admin')`. When we merge this into our "default" query, this would then fetch the user's defined role, but only if the `name` attribute of the target `Role` was equal to `"admin"`. `userQuery` generally makes more sense in the context of many-to-many relationships, but can still be quite useful for 1x1 relationships.

One thing to note here is that Mythix ORM expects the developer to do something with the `userQuery`. If it is not used by the developer than it will be silently discarded. Why? Because with complex relationships you might want to apply the `userQuery` to some other part of the primary query... or you actually might not want to use it at all... or something else entirely. Allowing the developer to define what happens with `userQuery` might make the code more verbose, but it also allows much more flexibility and power in how it is used.


Let's look at it from the `Role` perspective now.

From the perspective of the `Role` it is very similar, except the "query provider" has switched to the `User` model (as the target/root model), where the `roleID` field matches the current `Role` model instance (self) `id` attribute.

We have:

```javascript
type: Types.Model('User', ({ User, self, userQuery }) => {
  return User.where.roleID.EQ(self.id).MERGE(userQuery);
}),
```

So now, let's take a look at the process here:
  1. From the perspective of this specific role, what are we targeting?
  2. We are targeting the `User` model, and its `roleID` field
  3. Great! Now, who is providing the value to match against the `User:roleID` field?
  4. Oh, that is easy, it is me (self), this specific role, and the field to pull the `User:roleID` value from is `Role:id`... as in, this (self) very roles's primary key `id`.

One thing that is important to know is that the model name defined in the *first* argument will be the actual model type fetched. So, from the perspective of the user, we specify `Role` as the model in the first argument, so this is the role model type that will be fetched. From the perspective of the role, we specify `User` as the model in the first argument, so this is the model type that will be fetched.

## Injected relationship methods

When we use a virtual relationship type, such as `Types.Model` or `Types.Models`, then when a model with these field types is instantiated, the model will have methods injected into it for the specified relationships. For example, above we are creating a `role` relationship to the `Role` model. When we do this, the user model will automatically get a certain set of methods injected to work with this 1x1 relationship.

For `Types.Model` (1x1 relationship), these methods are:
  1. `queryFor`
  2. `create`
  3. `get`
  4. `update`
  5. `destroy`
  6. `has`
  7. `pluck`

Now what was just listed are *prefixes* for the injected methods. These methods obviously need to exist for every relationship field on the model, so these prefixes are used in combination with the field name to create these injected methods. In our specific case, `Role` (the `role` field capitalized) will be added to the end of these prefixes, giving us:
  1. `queryForRole`
  2. `createRole`
  3. `getRole`
  4. `updateRole`
  5. `destroyRole`
  6. `hasRole`
  7. `pluckRole`

This is handy, because now from our user we can call any of these methods to interact with the relationship. For example, we could check to see if a user has a role simply by `await user.hasRole()`. Or we could pluck fields from the related `Role` model simply by `await user.pluckRole([ 'name' ])`.

For `Types.Models` (many-to-n relationships), these methods are:
  1. `queryFor`
  2. `addTo`
  3. `get`
  4. `set`
  5. `removeFrom`
  6. `destroy`
  7. `count`
  8. `pluck`
  9. `has`

As with the singular `Types.Model`, the name of the field is added to the end of each... so if we had many `roles`, these would turn into:
  1. `queryForRoles`
  2. `addToRoles`
  3. `getRoles`
  4. `setRoles`
  5. `removeFromRoles`
  6. `destroyRoles`
  7. `countRoles`
  8. `pluckRoles`
  9. `hasRoles`

Most of these injected methods are fairly self-explanatory... however, you might be wondering what the `queryFor` injected methods are used for. Simply put, these methods return the relationship query itself, and do nothing else. This can be really handy if you want to modify the query beyond what you can just do by providing a `userQuery`. For example, you could fetch the `role` relationship query, and modify it:

```javascript
let query = await user.queryForRole();

query = query.AND.Role.name.EQ('admin');

let roles = await query.all();
```

Now you know! When you define a virtual relationship field, these methods will always be injected by default, for every relationship field.

Note: One other important thing to note here is that Mythix ORM will *not* overwrite your class methods. If you define a method on your `User` model called `getRole`, then Mythix ORM won't touch that method when it is injecting methods onto the model. For this reason, Mythix ORM *also* injects the same methods but prefixed with an underscore (i.e. `_queryForRole`, `_createRole`, etc...). This will allow developers to easily implement their own methods of the same name as an override, while still being able to access the injected relationship method via the underscore prefix.

i.e.:
```javascript
async getRole(...args) {
  let role = await this._getRole(...args);
  // do something with `role`
  return role;
}
```

## Example #2 - Defining a One To Many relationship

Okay, so your boss comes in on Monday morning, and yells at you for implementing the previous example, because users only being able to have a single role is "just plain stupid... what were you thinking?"

Well, it is hard to argue with the boss, especially since he is correct. Users only being able to have a single role is a just a *wee* bit limiting. So, lets update this so a user can have many roles. To do so, we will need to define a one-to-many relationship.

Let's jump right into the code this time:

```javascript
class User extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'firstName': {
      type:         Types.STRING(64),
      allowNull:    true,
      index:        true,
    },
    'lastName': {
      type:         Types.STRING(64),
      allowNull:    true,
      index:        true,
    },
    // Let's drop this column, because now it makes
    // no sense to have it live on the User table
    //
    // 'roleID': {
    //   type:         Types.FOREIGN_KEY('Role:id', {
    //     onDelete: 'SET NULL',
    //     onUpdate: 'SET NULL',
    //   }),
    //   allowNull:    true,
    //   index:        true,
    // },
    //
    // Let's change "role" to "roles", because
    // now it will be plural (one to many)
    'roles': {
      // Now we simply need to change our primary
      // query slightly. We will now be targeting
      // a "userID" column on the Role model.
      //
      // Notice the use of "plural" "Models" here
      type:       Types.Models('Role', ({ Role, self, userQuery }) => {
        return Role.where.userID.EQ(self.id).MERGE(userQuery);
      }),
    },
  };
}

class Role extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'name': {
      type:         Types.STRING(64),
      allowNull:    false,
      index:        true,
    },
    // Define a foreign key relationship
    // to the User table, targeting the
    // "id" column of that table.
    'userID': {
      type:         Types.FOREIGN_KEY('User:id', {
        // Before we didn't want to delete
        // a User if we deleted a role, so
        // we set these values to "SET NULL".
        // Now, if we delete a User, it would
        // make sense that we would want to
        // delete all the User's roles... so
        // this time we "CASCADE".
        onDelete: 'CASCADE',
        onUpdate: 'CASCADE',
      }),
      allowNull:    false,
      index:        true,
    },
    // This is still correctly named, as
    // each role will still only link back
    // to a single user
    'user': {
      // Now we update this, flipping the
      // relationship. If we store a "userID"
      // on the Role table, then a user can
      // have many roles.
      type:         Types.Model('User', ({ User, self, userQuery }) => {
        return User.where.id.EQ(self.userID).MERGE(userQuery);
      }),
    },
  };
}
```

Hopefully this makes sense. We simply removed `roleID` from the User model, and moved it over to instead be `userID` on the other side of the relationship. Now a user can have many roles, because instead of each user only defining a single `roleID`, now Roles define a `userID`, and since many roles can define the same `userID`, a user can have many roles.

## A quick aside on Foreign Keys

Great! So far so good. Hopefully my reader is still following me. If you don't know what "foreign keys" are, or don't understand the concept behind them, I suggest you go take a moment to read about them. In short, "foreign keys" simultaneously define an index, and one or more constraints. This means the database will disallow certain things. For example, if you specify that a relationship between a user and a role **must** exist, and **must** be valid, then the database will throw an error if you try to add a role without a user.

The `Types.FOREIGN_KEY` type allows us to define a foreign key relationship. The first argument is the target model and field. The second argument are simply the "options" for the foreign key.

Using the `FOREIGN_KEY` where appropriate is important. It is the **only** way that Mythix ORM knows your models are related. Mythix ORM uses this field type to update related attributes on models during load and store. Without using foreign keys, Mythix ORM will not know that your models are related, and so you might struggle when attempting to store or load related models. For example, because we defined `FOREIGN_KEY` types above for our `Role` model, linking to the `User` model `id` field, when Mythix ORM loads a `Role` model from the database, it will automatically know that it needs to assign the `userID` attribute of the `Role` model to the user's `id` field. This will happen even if the `userID` column is not loaded from the database.

The other thing `FOREIGN_KEY` types open up to you as the developer is easier bulk-model creation. For example, the following is completely valid, but **only** when using the `FOREIGN_KEY` type, so that Mythix ORM knows the models are related: `await Role.create({ name: 'admin', user: { firstName: 'Bob', lastName: 'Brown' } });`. What this is doing (even though it is strange, and likely doesn't reflect a real-world example), is creating the user *at the same time* that it creates the role. Mythix ORM is smart enough to understand the relationships, and it knows that in order to create a `Role` model it must first have a `userID`, so it will create and store the specified user first, and then create the role requested using the created user's `id`. It knows that `user: {some value}` is a `User`, because of the target model defined on that virtual field.

If the provided value to the role's `user` attribute was instead an already persisted `User` model instance, than Mythix ORM would skip storing the model, and simply pull its `id` for the `userID` attribute of the role.

**Important note:** Mythix ORM ignores many-to-n relationships by default with these types of operations. So you *can not* do the reverse, such as: `await User.create({ ..., roles: [ { name: 'admin' } ] })`. This **will not** work. Mythix ORM doesn't know what is desired in this case. What if the user already had some roles? Would they be removed? What if we wanted to just add instead? Because Mythix ORM can not know what you want to do with many-to-n relationships in a case like this, it simply isn't supported. With many-to-n relationships you must be explicit, and generally will use the relationship methods injected by the type itself. For example, instead of the above, you would have to:

```javascript
let user = await User.create({ firstName: 'Bob', lastName: 'Brown' });
let role = await user.addToRoles({ name: 'admin' });
```

This *will* work, because Mythix ORM now understands that you want to *add* to the many-to-n model set.

## Example #3 - Using a "through" table in relationships

So far we have just defined relationships between two different tables, our `User` and `Role` tables. What if we instead want to define more that two tables in our relationship? What if, for example, our boss comes back to us on Friday morning and says "Dang, Shayla, okay, well, this situation is slightly better. Now users can have more than one role, but we need to define role information *on the relationship* itself."

Back to work! Now the boss wants extra information defined *on the relationship*, not on users, and not on roles, but rather on the relationship between the two. The boss wants us to define if a user, with a specific role, is able to use the front-door of the office, the back-door of the office, or both.

Okay, strange request boss... but then again, bosses generally don't do the thinking very well, which is why they hired you, right? Great! Let's get r' done.

For this to work, we now need to define a third table. We will call this table `UserRole`. This will define the user id, the role id, and which door the user is allowed to use. This will also mean that we need to remove the `userID` from the `Role` table.

So to sum up, we need to do the following:
  1. Drop the `userID` from the `Role` table
  2. Create a new table/model called `UserRole`
  3. Adjust the relationships between the models

Let's dig in:

```javascript
class User extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'firstName': {
      type:         Types.STRING(64),
      allowNull:    true,
      index:        true,
    },
    'lastName': {
      type:         Types.STRING(64),
      allowNull:    true,
      index:        true,
    },
    'roles': {
      // Now we update our primary query to
      // go through the UserRole table.
      //
      // Note: Notice how we are .EQ against
      // a query that has no conditions. We
      // ask for `Role.where.id.EQ(UserRole.where.id)`.
      // This is a table join. When you use any
      // conditional operator on a FIELD from
      // another table, without any conditions of
      // its own, then Mythix ORM translates this
      // as a request to join tables.
      type:       Types.Models('Role', ({ Role, UserRole, self, userQuery }) => {
        return Role.where.id.EQ(UserRole.where.roleID).AND.UserRole.userID.EQ(self.id).MERGE(userQuery);
      }),
    },
  };
}

class Role extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    'name': {
      type:         Types.STRING(64),
      allowNull:    false,
      index:        true,
    },
    'user': {
      type:         Types.Model('User', ({ User, UserRole, self, userQuery }) => {
        return User.where.id.EQ(UserRole.where.userID).AND.UserRole.roleID.EQ(self.id).MERGE(userQuery);
      }),
    },
  };
}

class UserRole extends Model {
  static fields = {
    'id': {
      type:         Types.UUIDV4,
      defaultValue: Types.UUIDV4.Default.UUIDV4,
      allowNull:    false,
      primaryKey:   true,
    },
    // The new data point our boss wanted us to add
    'doorUsage': {
      type:         Types.STRING(16),
      allowNull:    false,
      index:        true,
    },
    'userID': {
      type:         Types.FOREIGN_KEY('User:id', {
        onDelete: 'CASCADE',
        onUpdate: 'CASCADE',
      }),
      allowNull:    false,
      index:        true,
    },
    'roleID': {
      type:         Types.FOREIGN_KEY('Role:id', {
        onDelete: 'CASCADE',
        onUpdate: 'CASCADE',
      }),
      allowNull:    false,
      index:        true,
    },
    // These aren't needed, but they might be
    // nice to have if we already have an instance
    // of a UserRole model, and want to interact
    // with the user or role of that instance.
    'role': {
      type:       Types.Model('Role', ({ Role, self, userQuery }) => {
        return Role.where.id.EQ(self.roleID).MERGE(userQuery);
      })
    },
    'user': {
      type:       Types.Model('User', ({ User, self, userQuery }) => {
        return User.where.id.EQ(self.userID).MERGE(userQuery);
      }),
    },
  };
}
```

This is fantastic! But... we have one problem. This through-table relationship will indeed do a three-way join to collect the information requested from all tables, but what about that new `doorUsage` field we added to the `UserRole` table? Nowhere have we specified where that can be found... Mythix ORM could just inject it onto `Role` models on load... but that doesn't make sense, because the user could have multiple roles, and each role link could have a different `doorUsage` value. Also, what if the `Role` model itself had a `doorUsage` field? That would not be ideal... Instead, it would be better if we could simply access the `doorUsage` attribute on any loaded through-table relationship.

When Mythix ORM loads a model through a relationship like this, it will by default *only* load the root/target model. This default behavior is deliberate to improve the performance and efficiency of the library. In order to also fetch the related models at the same time, you must directly specify a projection that includes the related model. When you do this, Mythix ORM will *then* include the specified related models while loading data.

Because we *want* to access the `doorUsage` attribute on the through-table model (`UserRole`), in the examples below you will see we use a `.PROJECT('+UserRole:doorUsage')`. This informs Mythix ORM that we want to add (`+`) the field `UserRole:doorUsage` to the query projection, which also means that Mythix ORM will load the `UserRole` model and assign it to our role.

```javascript
// To get the roles along with their related
// UserRole model, we PROJECT on the UserRole
// model `doorUsage` field. This will ensure
// that Mythix ORM includes the related models
// (even though those models will only be
// partially loaded, and will only contain the
// `doorUsage` field).
let roles = await thisUser.getRoles(Role.where.PROJECT('+UserRole:doorUsage'));
```

When we load the relationship this way, every `role` in the return `roles` array will also have a `UserRoles` key, containing the related `UserRole` models from the through table relationship.

Note: Because we only projected on the `UserRole:doorUsage` field, when you access one of the related `UserRole` models, the models will be incomplete, and will only have a `doorUsage` field set on them. This is fine though, because that is the only data point we need. Do **not** try and directly store these models back to the database however, because 1) without their primary key (`id`) any attempt to save will immediately fail, and 2) with missing attributes, even if you were able to get the model to save, you might end up with corruption (missing field values) in the database.

Now, we can access the related model and find the `doorUsage` attribute we are looking for.

```javascript
let roles = await loadedUserInstance.getRoles(Role.where.name.EQ('restricted-door-usage').PROJECT('+UserRole:doorUsage'));
let doorUsage = roles[0].UserRoles[0].doorUsage;
```

Great! Now we can easily access this attribute and get the job done as the boss asked:

```javascript
let user = await User.where.id.EQ(userIDFromDoorRFIDScanner).first();
let roles = await user.getRoles(Role.where.name.EQ('restricted-door-usage').PROJECT('+UserRole:doorUsage'));
let doorUsage = roles[0].UserRoles[0].doorUsage;

if (doorUsage === 'back') {
  // User is only allowed to use the back door
} else if (doorUsage === 'front') {
  // User is only allowed to use the front door
} else {
  // User can use either door... the boss
  // has conveniently ensured that this
  // case applies to him...
}
```

## Final Notes

  1. I would like to bring it to the attention of the reader that concrete types are defined all *UPPERCASE*, whereas virtual types are defined as *CamelCase*.
  2. "But, what if I am using two, three, or more through-tables?" you ask. Well, you are in luck! Nothing described above changes. You simply define a more complex primary query using the query provider, and Mythix ORM will be smart enough to recursively walk the relationships in the query, join as many tables as it needs to to get the job done, and do the right thing.
  3. Mythix forces you to manually define all table/model fields. Yes, this is extra overhead, but it comes with the benefit of not needing to painstakingly lookup fields that have been "magically" defined somewhere deep in the library. By forcing the user to always define all columns/fields manually, it simplifies seeing what fields exist on the table, removes down-stream dependencies, and prevents the user from needing to go look-up documentation to understand how things are working and why. Feel free to write your own helper methods that will automatically inject fields into your schema for you!
  4. When pulling related models, Mythix ORM will always put them in a plural {model name} key... i.e. if you noticed above, `UserRole` models that were fetched were placed into `.UserRoles`. This is true for all relationship operations. If you opt-in to loading other relationships during an operation, the related models will always be placed on the loaded model instances, under their plural name (always as an array of models).

Happy coding!
