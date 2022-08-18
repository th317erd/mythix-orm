# Mythix ORM associations

Mythix ORM makes associations really easy.

Fields in a Mythix ORM model can be either "virtual" or "concrete".

Concrete fields are backed by storage (the database), and will have a direct value they can be associated with.

Virtual fields are not backed by storage (at least not directly), and instead will dynamically fetch their value.

A field can be virtual in Mythix ORM simply because of its type. For example, the types `Types.Model` and `Types.Models` will by nature define a virtual field. These types define relations to other tables, and so don't have a 1x1 concrete field in the DB themselves, and instead will pull data based on the relationship defined by the type.

Before we get started, there are a few things to keep in mind:

  1. `Types.Model` is used for a 1x1 relationship
  2. `Types.Models` is used for a one-to-many, or a many-to-many relationship
  3. These two types specify field relationships
  4. In field relationships, it is common to specify a target model, which is done by prefixing the field with the model name plus a single colon (i.e. `User:id`)
  5. In field relationships, it is common to specify a target field (which optionally comes after the model name and colon)
  6. In field relationships, if a model isn't defined, then `Self` ("this model") is assumed
  7. In field relationships, if a field isn't defined, then the target model's primary key is assumed
  8. Field relationships work by defining a "target", and a "value provider" (where to get the value to match against the target)
  9. All concrete fields must be manually defined on all models. There is no "automagic" or "hidden" fields that the relationships themselves will create. Said another way, **you must manually define ALL table columns, always, without exception**... none will be auto-defined for you. If you want auto-defined fields, such as "createdAt", and "updatedAt" for all your models, then the recommendation is to define a BaseModel that all of your other models inherit from.

To properly understand how Mythix ORM creates table relationships/associations, it helps to view the situation from the perspective of "this model instance". Don't think of "tables", or "many" when you are thinking of the relationship. Think only of "this model instance".

## Example #1 - Defining a 1x1 relationship

Let's say you want to have a Users table, and have each user have a single Role.

This is a 1x1 relationship, with one User to one Role.

You could define the association like so:

```javascript
class User extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'firstName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'lastName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'roleID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    // This defines a "virtual" field,
    // that will be used to define
    // methods to interact with the role
    'role': {
      // Remember "target field" and "value provider"
      // If no model is specified, then "Self" is assumed
      // so 'roleID', could be expanded to 'User:roleID'
      //
      //                      target     value provider
      type:       Types.Model('Role:id', 'roleID'),
    },
  };
}

class Role extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'name': {
      type:       Types.STRING(64),
      allowNull:  false,
      index:      true,
    },
    // This defines a "virtual" field,
    // that will be used to define
    // methods to interact with the user
    'user': {
      // Second argument could be omitted here,
      // since the model's PK is assumed if not
      // specified
      type:       Types.Model('User:roleID', 'id'),

      // Equivalent
      // type:       Types.Model('User:roleID'),
    },
  };
}
```

Let's look at the `User` model `role` field first, which is of the type:

```javascript
type: Types.Model('Role:id', 'roleID'),
```

This may seem confusing at first, until you understand the pattern here. The pattern is fairly simple. The `Types.Model` is singular (`Models` on the other hand, if used, defines a one-to-many, or many-to-many relationship), so right away we know this is going to fetch a single model, and not many models. It is a 1x1 relationship. Next, we have the two arguments we provide. It helps a lot to think of these as "target", and "value provider"... as in, "What is the target field we are matching on, and who/what is providing the value to be matched against?" It also helps quite a bit to think of this from the perspective of "this model instance", instead of "this table", or "this relationship". The relationship is always from an instance of a model... a single instance (or a single row in the DB, if that helps instead).

With this in mind, the above model definitions should make a little more sense:

  1. From the perspective of this specific user, what are we targeting?
  2. We are targeting the `Role` model, and its `id` field
  3. Great! Now, who is providing the value to match against the `Role:id` field?
  4. Oh, that is easy, it is me, this specific user, and the field to pull the `Role:id` from is `User:roleID`... as in, this very user's `roleID` ("self.roleID").

Simple!

From the perspective of the Role it is very similar, except the "value provider" has switched to the `User` model, where the `roleID` field exists.

Let's look at it from the `Role` perspective now.

We have:

```javascript
type: Types.Model('User:roleID', 'id'),
```

Remember, this could also be written as:

```javascript
type: Types.Model('User:roleID'),
```

When no model is specified in the "target", or the "value provider" arguments, then `Self` is implied. So when we say `'id'`, what is really being specified is `Role:id`. The PK of a model is assumed as the default if no "value provider" is given. So, `Role:id`, `id`, and empty are all equivalent in this situation.

So now, let's take a look at the process here:

  1. From the perspective of this specific role, what are we targeting?
  2. We are targeting the `User` model, and its `roleID` field
  3. Great! Now, who is providing the value to match against the `User:roleID` field?
  4. Oh, that is easy, it is me, this specific role, and the field to pull the `User:roleID` value from is `Role:id`... as in, this very roles's primary key `id`

One thing that is important to know is that the model name defined in the *first* argument will be the actual model type fetched. So, from the perspective of the user, we specify `Role` as the model in the first argument, so this is the role model type that will be fetched. From the perspective of the role, we specify `User` as the model in the first argument, so this is the model type that will be fetched.

## Example #2 - Defining a One To Many relationship

Okay, so your boss comes in on Monday morning, and yells at you for implementing the previous example, because users only being able to have a single role is "just plain stupid... what were you thinking?"

Well, it is hard to argue with the boss, especially since he is correct, users only being able to have a single role is a just a *wee* bit limiting. So, lets update this so a user can have many roles. To do so, we will need to define a one-to-many relationship.

Let's jump right into the code this time:

```javascript
class User extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'firstName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'lastName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    // Let's drop this column, because now it makes
    // no sense to have it live on the User table
    // 'roleID': {
    //   type:       Types.UUIDV4,
    //   allowNull:  false,
    //   index:      true,
    // },
    //
    // Let's change "role" to "roles", because
    // now it will be plural (one to many)
    'roles': {
      // Here we don't need to define a second argument
      // because "Self" and "PK" are assumed, so this
      // works fine. We want the User.id to match against
      // the Role.userID, so we can just leave off the
      // second argument
      //
      // Notice the use of "plural" "Models" here
      type:       Types.Models('Role:userID'),

      // Equivalent:
      // type:       Types.Models('Role:userID', 'User:id'),
      //
      // or:
      //
      // type:       Types.Models('Role:userID', 'id'),
    },
  };
}

class Role extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'name': {
      type:       Types.STRING(64),
      allowNull:  false,
      index:      true,
    },
    // Now we need to define the "userID"
    // for each role
    'userID': {
      type:       Types.UUIDV4,
      allowNull:  false,
      index:      true,
    },
    // This is still correctly named, as
    // each role will only link back to
    // a single user
    'user': {
      // Now we update this, flipping the
      // relationship. If we store a "userID"
      // on the Role table, then a user can
      // have many roles.
      type:       Types.Model('User', 'userID'),

      // Equivalent
      // type:       Types.Model('User:id', 'userID'),
      //
      // or:
      //
      // type:       Types.Model('User:id', 'Role:userID'),
    },
  };
}
```

Hopefully this makes sense. We simply removed `roleID` from the User model, and moved it over to instead be `userID` on the other side of the relationship. Now a user can have many roles, because instead of each user only defining a single `roleID`, now Roles define a `userID`, and since many roles can define the same `userID`, a user can have many roles.

## Example #3 - Foreign Keys

Great! So far so good. Hopefully my reader is still following me. What we have done so far will work, but there is a better way. We can have the database assist us in our relationships. If you don't know what "foreign keys" are, or don't understand the concept behind them, I suggest you go take a moment to read about them. In short, "foreign keys" simultaneously define an index, and one or more constraints. This means the database will disallow certain things. For example, if you specify that a relationship between a user and a role MUST exist, and MUST be valid, then the database will throw an error if you try to add a role without a user.

Let's go ahead and do that now. After all, we really shouldn't have a `Role` that points to a `NULL` user.

So, let's make the following minor changes:

```javascript
class User extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'firstName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'lastName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'roles': {
      type:       Types.Models('Role:userID'),
    },
  };
}

class Role extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'name': {
      type:       Types.STRING(64),
      allowNull:  false,
      index:      true,
    },
    'userID': {
      type:       Types.ForeignKey('User:id', {
        // if User is deleted, also delete all roles belonging to the user
        onDelete:   'CASCADE',
      }),

      // Disallow `userID` from ever being `NULL`
      allowNull:  false,

      // "index" is no longer needed (but it also won't hurt).
      // ForeignKeys are indexed by default.
      // index:      true,
    },
    'user': {
      type:       Types.Model('User', 'userID'),
    },
  };
}
```

So this change was fairly simple and straight forward. As you can see, we simply changed the type of the `userID` column on the `Roles` table. Instead of just being a `UUIDV4` type, we instead changed it to be a `ForeignKey` type. The `ForeignKey` type will look up the target field, and pull its type and other options from the target field. So this `ForeignKey('User:id')` will go look-up the `User` model, find its `id` field, and use that to define the `userID` field. Plus, it also defines a foreign-key relationship in the database simultaneously.

## Example #4 - Using a "through" table in relationships

So far we have just defined relationships between two different tables, our `User` and `Role` tables. What if we instead want to define more that two tables in our relationship? What if, for example, our boss comes back to us on Friday morning and says "Dang, Shayla, okay, well, this situation is slightly better. Now users can have more than one role, but we need to define role information ON THE RELATIONSHIP itself.".

Back to work! Now the boss wants extra information defined ON THE RELATIONSHIP, not on users, and not on roles, but rather on the relationship between the two. The boss wants us to define if a user, with a specific role, is able to use the front-door of the office, the back-door of the office, or both.

Okay, strange request boss... but then again, bosses generally don't do the thinking very well, which is why they hired you, right? Great! Let's get r' done.

For this to work, we now need to define a third table. We will call this table `UserRole`. This will define the user id, the role id, and which door the user is allowed to use. This will also mean that we need to remove the `userID` from the `Role` table.

So to sum up, we need to do the following:

  1. Drop the `userID` from the `Role` table
  2. Create a new table called `UserRole`
  3. Adjust the relationships between the fields

Let's dig in:

```javascript
class User extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'firstName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'lastName': {
      type:       Types.STRING(64),
      allowNull:  true,
      index:      true,
    },
    'roles': {
      // Notice here how we specify the virtual field 'UserRole:role'
      // this will be used to collect all the information
      // necessary to join the tables.
      //
      // Mythix ORM is smart enough to follow all fields
      // until it can build the full relationship.
      // So Mythix ORM will first lookup `UserRole:role`,
      // find that it is a virtual field that targets `roleID`,
      // which it will then look-up, and find that this is a
      // foreign key that points to `Role.id`.
      // So now it knows how to get to the role.
      //
      // But... how do we get to the proper UserRole based
      // on "this instance" of a User? This is why the
      // "value provider" is also targeting a virtual
      // field. When Mythix ORM does a look-up on the field
      // it will notice that it is a virtual field, targeting
      // the "User" model, and so it now knows all relationships.
      type:       Types.Models(
        // Target field (will be fully resolved)
        'UserRole:role',
        // Value provider (will be fully resolved)
        'UserRole:user',
      ),
    },
  };
}

class Role extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    'name': {
      type:       Types.STRING(64),
      allowNull:  false,
      index:      true,
    },
    'user': {
      type:       Types.Model(
        // Target field (will be fully resolved)
        'UserRole:user',
        // Value provider (will be fully resolved)
        'UserRole:role',
      ),
    },
  };
}

class UserRole extends Model {
  static fields = {
    'id': {
      type:       Types.UUIDV4,
      allowNull:  false,
      primaryKey: true,
    },
    // The new data point our boss wanted us to add
    'doorUsage': {
      type:       Types.STRING,
      allowNull:  false,
      index:      true,
    },
    'userID': {
      type:       Types.ForeignKey('User:id', {
        // if User is deleted, also delete all UserRole belonging to the user
        onDelete:   'CASCADE',
      }),
      allowNull:  false,
    },
    'roleID': {
      type:       Types.ForeignKey('Role:id', {
        // if Role is deleted, also delete all UserRole belonging to the role
        onDelete:   'CASCADE',
      }),
      allowNull:  false,
    },
    'role': {
      //                      'Role:id' is assumed
      type:       Types.Model('Role', 'roleID')
    },
    'user': {
      //                      'User:id' is assumed
      type:       Types.Model('User', 'userID'),
    },
  };
}
```

This is fantastic! But... we have one problem. This through-table relationship will indeed do a three-way join to collect the information requested from all tables, but what about that new `doorUsage` field we added to the `UserRole` table? Nowhere have we specified where that should be used... We could just inject it onto `User` models on load... but that doesn't make sense, because the user could have multiple roles, and each role link could have a different `doorUsage` value. Instead, it would be better if we injected this `doorUsage` value onto the `Roles` that are loaded. That would make more sense, as the `doorUsage` is based on the role, and not based on user.

Okay, that is all gravy... but *how* exactly do we request Mythix ORM inject this field on the Role? Well, Mythix ORM will automatically include the through table model on the **loaded** models by default. This means if you load from the perspective of the user:

```javascript
let roles = await thisUser.getRoles();
```

Then every `role` in the `roles` array will also have a `userRole` key, defining the row from the through table relationship.

```javascript
let roles = await thisUser.getRoles();
// roles[0].userRole = UserRole { userID, roleID, id, doorUsage }
```

If instead you load from the perspective of a role, then `userRole` will be added to the user:

```javascript
let user = await role.getUser();
// user.userRole = UserRole { userID, roleID, id, doorUsage }
```

*NOTE:
If this was a many-to-many relationship, then `userRole` would be added to **both-sides** of the relation, being added to both loaded `User` models, and also to loaded `Role` models.*

Great! Now we can easily access this and get the job done as the boss asked:

```javascript
let role = await pseudoCodeToFetchRoleBasedOnDoorBeingUsed(doorID);
let user = await role.getUser();

if (user.userRole.doorUsage === 'back') {
  // User is only allowed to use the back door
} else if (user.userRole.doorUsage === 'front') {
  // User is only allowed to use the front door
} else {
  // User can use either door... the boss has conveniently
  // ensured that this case applies to him
}
```

## Final Notes

  1. I would like to bring it to the attention of the reader that concrete types are defined all *UPPERCASE*, whereas virtual types are defined as *CamelCase*.
  2. "But, what if I am using two, three, or more through-tables?" you ask. Well, you are in luck! Nothing described above changes. You simply target virtual fields as your "target" and "value provider" in the relationships you define, and Mythix ORM will be smart enough to recursively walk all fields, understand all relationships, and join as many tables as it needs to to get the job done.
  3. Mythix forces you to manually define all table fields. Yes, this is extra overhead, but it comes with the benefit of not needing to painstakingly manually define all *relationships*. This design pattern was also decided upon so that there aren't any table columns that are "hidden" from the user, or ambiguous. By forcing the user to always define all columns manually, it simplifies seeing what fields exist on the table, removes down-stream dependencies, and prevents the user from needing to go look-up documentation to understand how things are working and why. Feel free to write your own helper methods that will automatically inject fields into your schema for you!

Happy coding!
