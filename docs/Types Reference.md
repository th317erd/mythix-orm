This article is intended as a quick overview of the database types that Mythix ORM provides. See the documentation for any given type for more information.

All database types that Mythix ORM provides can be accessed via the `Types` import from the `mythix-orm` module. For example,

```javascript
const { Types } = require('mythix-orm');

let stringType = Types.STRING(128);
...
```

<hr>

A type can be instantiated either by its wrapper, or directly via its type class. For example, both of the following ways to construct types are valid, and equivalent:

```javascript
const { Types } = require('mythix-orm');

let stringType1 = Types.STRING(128);
let stringType2 = new Types.StringType(128);
...
```

<hr>

Most types don't require that you "call" the wrapper, though some do require arguments, and so require that you provide them when creating the type (i.e. `FOREIGN_KEY` type requires arguments). Most types provided by Mythix ORM have "sane" defaults for options that can be provided to them.

```javascript
const { Types } = require('mythix-orm');

// This...
let stringType1 = Types.STRING;

// ... is the same as this...
let stringType2 = Types.STRING(256);
```

<hr>

There are two kinds of database types in Mythix ORM: "concrete" types, and "virtual" types. Concrete types are types that are backed by storage (a database), these include types like `Types.STRING`, `Types.INTEGER`, etc... Virtual types are types like `Types.Model` and `Types.Models`. These don't have any storage backing them (at least not directly), and instead define relationships between data.

A keen observer will notice the naming convention employed here. Concrete types always have ALL_UPPER_CASE_NAMES, whereas virtual types have CamelCase names. This makes it easy to look at any model's fields, and immediately know what fields are stored in the database, and what fields aren't (directly) stored.

<hr>

# Concrete Types

## [Types.BIGINT](./BigIntType)

A 64+ bit integer type. In most SQL-type databases this would be a `BIGINT` type.

## [Types.BLOB](./BlobType)

A "blob" type, for storing random blobs of raw data (i.e. such as images). In most SQL-type databases this would be a `BLOB` or `BYTEA` type.

## [Types.BOOLEAN](./BooleanType)

A boolean type for storing `true` and `false` values. In most SQL-type databases this would be a `BOOLEAN` type.

## [Types.CHAR](./CharType)

A character type for storing a single character. In most SQL-type databases this would be a `CHAR` type.

## [Types.DATE](./DateType)

A date type for storing just a date (without a time). In most SQL-type databases this would be a `BIGINT` type.

*Note: Mythix ORM will always attempt to store all date and time types as timestamps, generally backed by a `BIGINT` database type.*

## [Types.DATETIME](./DateTimeType)

A date type for storing a date as well as a time. In most SQL-type databases this would be a `BIGINT` type. Timestamps are stored with millisecond resolution in most databases.

*Note: Mythix ORM will always attempt to store all date and time types as timestamps, generally backed by a `BIGINT` database type.*

## [Types.FOREIGN_KEY](./ForeignKeyType)

A foreign key type. This type will mimic the concrete type of its target, and also simultaneously defines a relationship with its target.

## [Types.INTEGER](./IntegerType)

An integer type that can specify integer storage in multiple different byte sizes. For example, this is used to specify `TINYINT`, `SMALLINT`, and `MEDIUMINT` in MySQL. In most SQL-type databases this would be a `INT` or `INTEGER` type.

## [Types.NUMERIC](./NumericType)

A high-resolution floating-point type, often used for things like currency. In most SQL-type databases this would be a `NUMERIC` or `DECIMAL` type.

## [Types.REAL](./RealType)

A low-resolution floating-point type. In most SQL-type databases this would be a `FLOAT` or `DOUBLE` type.

## [Types.SERIALIZED](./SerializedType)

A type that will serialize and deserialize data, by default in JSON format. Its underlying concrete type must be specified, and would generally be a `Types.STRING` or `Types.TEXT` concrete type (a string-based storage type is not required). Though this defaults to serializing and deserializing JSON, it can be configured to serialize and deserialize using any algorithm desired.

## [Types.STRING](./StringType)

A variable length string type, generally stored in the table-space of the underlying database. In most SQL-type databases this would be a `VARCHAR` type.

## [Types.TEXT](./TextType)

A variable length string type, generally stored outside the table-space of the underlying database (making it slightly slower than `Types.STRING`). This string type generally has a much larger capacity than `Types.STRING`. In most SQL-type databases this would be a `TEXT` type.

## [Types.UUIDV1](./UUIDV1Type)

A UUID version 1 type, backed by the concrete type `Types.STRING`. It will calculate its own length based on the length of a UUID version 1, and of any prefix given to it. In most SQL-type databases this would be a `VARCHAR` type.

*Note: This type uses the [uuid](https://www.npmjs.com/package/uuid) Node module to validate and generate UUIDs.*

## [Types.UUIDV3](./UUIDV3Type)

A UUID version 3 type, backed by the concrete type `Types.STRING`. It will calculate its own length based on the length of a UUID version 3, and of any prefix given to it. In most SQL-type databases this would be a `VARCHAR` type.

*Note: This type uses the [uuid](https://www.npmjs.com/package/uuid) Node module to validate and generate UUIDs.*

## [Types.UUIDV4](./UUIDV4Type)

A UUID version 4 type, backed by the concrete type `Types.STRING`. It will calculate its own length based on the length of a UUID version 4, and of any prefix given to it. In most SQL-type databases this would be a `VARCHAR` type.

*Note: This type uses the [uuid](https://www.npmjs.com/package/uuid) Node module to validate and generate UUIDs.*

## [Types.UUIDV5](./UUIDV5Type)

A UUID version 5 type, backed by the concrete type `Types.STRING`. It will calculate its own length based on the length of a UUID version 5, and of any prefix given to it. In most SQL-type databases this would be a `VARCHAR` type.

*Note: This type uses the [uuid](https://www.npmjs.com/package/uuid) Node module to validate and generate UUIDs.*

## [Types.XID](./XIDType)

An XID type, backed by the concrete type `Types.STRING`. It will calculate its own length based on the length of an XID, and of any prefix given to it. In most SQL-type databases this would be a `VARCHAR` type.

*Note: This type uses the [xid-js](https://www.npmjs.com/package/xid-js) Node module to validate and generate XIDs.*

# Virtual Types

## [Types.Model](./ModelType)

A virtual type for defining a 1-to-1 relationship with another model. The relationship can define one or more "through tables".

*Note: Notice its singular name*

## [Types.Models](./ModelsType)

A virtual type for defining a 1-to-many, or many-to-many relationship with another model(s). The relationship can define one or more "through tables".

*Note: Notice its plural name*
