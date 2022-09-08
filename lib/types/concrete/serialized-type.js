'use strict';

const Type = require('../type');

/// A field type that serializes and deserializes raw objects.
///
/// The `SERIALIZED` type can be used to serialize
/// other data into a single field value. By default this
/// field type is configured to serialize `JSON`.
/// However, it can be configured to serialize and
/// deserialize in any encoding you desire.
///
/// Something important to note right up front is that
/// this type hooks into the model instance's "dirty" system,
/// and will call your provided `serialize` and `deserialize`
/// methods to check if your object has *internally* become
/// dirty. For example, if you use this type for a field called
/// `metadata` on your `User` model, and you have an instance
/// of that user model you are working with, and you *internally*
/// change the `metadata` field like so `user.metadata.class = 'wizard';`
/// then this field type will detect that the `metadata` attribute
/// of your model is "dirty" by serializing the `metadata` value
/// and comparing it to the "undirty" serialized value, even if
/// the actual value of the `metadata` field itself hasn't changed.
/// For this reason it is important that your serializer sort
/// the serialized data if possible. If sorting isn't possible,
/// then the side-effect will be that this field may be serialized
/// and stored more often than actually needed. This system was
/// designed this way because it is likely better to get a false
/// positive and save your data, then not check at all and lose data.
/// If needed, you can always create your own child type of
/// `SerializedType`, and overload the `isDirty` method, or provided
/// an `isDirty` method to the field `options` to check
/// for dirtiness yourself, based on what makes sense with your
/// serializing algorithm.
///
/// See: Type
class SerializedType extends Type {
  /// Get the "display" name for this type.
  ///
  /// This method is called from <see>Model.toString</see>
  /// when stringifying the model for representation.
  ///
  /// Note:
  ///   This is also an instance method that can be called from
  ///   an instance of the type.
  ///
  /// Return: string
  ///   Return the string value `'SERIALIZED'`
  static getDisplayName() {
    return 'SERIALIZED';
  }

  getDisplayName() {
    return this.constructor.getDisplayName();
  }

  /// Construct a new instance of the `SerializedType` type.
  ///
  /// Interface:
  ///   interface SerializeCallContext {
  ///     // The value to serialize or deserialize
  ///     value: any;
  ///
  ///     // The database driver connection.
  ///     connection: Connection;
  ///   }
  ///
  /// Interface:
  ///   interface SerializedTypeOptions {
  ///     // Method to serialize data. Note how the
  ///     // return type is `any`. Though commonly an
  ///     // underlying `STRING` type will be used for
  ///     // the field value, that isn't a requirement.
  ///     // For example, you might want to encode a bitmask
  ///     // into a `BIGINT`. The world is your oyster.
  ///     //
  ///     // Unlike other fields, `null` and `undefined` are
  ///     // not simply returned. They are left up to the
  ///     // serializer and deserializer to handle and do
  ///     // with as they want. For this reason, you need
  ///     // to properly handle `null` and `undefined` values,
  ///     // even if you just return them (allowing the underlying
  ///     // database field to contain a `NULL` value).
  ///     serialize?: (context: SerializeCallContext) => any;
  ///
  ///     // Deserialize the data provided.
  ///     deserialize?: (context: SerializeCallContext) => any;
  ///
  ///     // The underlying storage type of this field. This will
  ///     // commonly be a `STRING` of some larger size, but isn't
  ///     // required to be. It can be any type that works for your
  ///     // serializer and deserializer.
  ///     type: Type
  ///
  ///     // Provide your own method to detect
  ///     // "dirtiness" of the field's value
  ///     // See the "isDirty" method of this
  ///     // class for more information.
  ///     isDirty?: (context: DirtyFieldValueContext) => any;
  ///   }
  ///
  /// Return: SerializedType
  ///
  /// Arguments:
  ///   options?: Type | SerializedTypeOptions
  ///     If this is a `Type`, then it will specify
  ///     the underlying field type that will store
  ///     the serialized value, and is the same as
  ///     specifying the `{ type: Type }` option.
  constructor(_options) {
    let options = _options;
    if (Type.isTypeClass(options) || Type.isType(options))
      options = { type: options };

    options = {
      serialize:    ({ value }) => {
        if (value == null)
          return value;

        return JSON.stringify(value);
      },
      deserialize:  ({ value }) => {
        if (value == null)
          return value;

        return JSON.parse(value);
      },
      ...(options || {}),
    };

    if (!options.type)
      throw new TypeError('SerializedType::constructor: "type" must be specified on the "options" for this type. "type" defines the storage type for this field. i.e. Types.SERIALIZED({ type: Types.STRING(1024) })');

    super(options);

    options.type = Type.instantiateType(options.type);

    Object.defineProperties(this, {
      'options': {
        writable:     true,
        enumerable:   false,
        configurable: true,
        value:        options,
      },
    });
  }

  /// Get the options passed to the field
  /// when it was defined.
  ///
  /// Return: object
  ///   The options object provided to the field.
  getOptions() {
    return this.options;
  }

  /// Call initialize on the underling type
  /// field.
  ///
  /// See: Type.initialize
  initialize(connection, self) {
    let options = this.getOptions();
    options.type.initialize(connection, self);
  }

  /// Cast provided value to underlying type.
  ///
  /// This will cast the incoming value by calling
  /// <see>SerializedType.deserialize</see> on the type with the provided
  /// value. If <see>SerializedType.deserialize</see> detects that the type
  /// is the same as the underlying storage type (i.e.
  /// `STRING`), then it will deserialize and return the
  /// value. However, if the provided value is not the
  /// type of the underlying storage field type, and the
  /// value is not `null` or `undefined`, then it will
  /// be assumed to already be deserialized and will be
  /// simply returned.
  ///
  /// See <see>Type.castToType</see> for a more
  /// detailed description.
  ///
  /// Return: any | null | undefined
  ///   Return the incoming `value`, cast to this
  ///   type (deserialized). `null` and `undefined` are simply
  ///   returned without casting by the default built-in JSON
  ///   deserializer... however, they may be treated differently
  ///   for other serializers.
  ///
  /// Arguments:
  ///   context: <see name="CastToTypeContext">Type.castToType</see>
  castToType({ value, connection }) {
    return this.deserialize(value, connection);
  }

  /// This is called whenever the model's field
  /// owning this type is changed. This method
  /// will update the "dirty" serialized cache
  /// contained on the model for this type. This
  /// is later used to see if the field value is
  /// dirty because any of the internal data of
  /// the field value has changed (via a serialize
  /// call that compares the value to last serialize).
  ///
  /// In short, this updates the last serialize cache
  /// to compare it with a serialize on insert/update
  /// to see if the serialized data is "dirty".
  ///
  /// Interface:
  ///   interface SetFieldValueContext {
  ///     // The field value that was just set onto the field.
  ///     value: any;
  ///
  ///     // The field that contains this type
  ///     field: Field;
  ///
  ///     // The name of the field that contains this type
  ///     fieldName: string;
  ///
  ///     // The model instance itself
  ///     self: Model;
  ///   }
  ///
  /// Note:
  ///   Because type instances for fields are shared across model
  ///   instances, the actual "dirty" cache is stored on the model
  ///   itself in a `_typeData` property on the model instance.
  ///
  /// Return: undefined
  ///
  /// Arguments:
  ///   context: SetFieldValueContext
  onSetFieldValue({ value, fieldName, self }) {
    // we make a copy here
    // so that when the value is modified
    // we can detect the changes later
    let clonedValue = this.deserialize(this.serialize(value, self.getConnection() || {}));
    self._typeData[fieldName] = clonedValue;
  }

  /// Check if this field is dirty, including if
  /// the object value stored on the field has changed
  /// since the last persist.
  ///
  /// This compares the current value of the field
  /// to the last persisted value by serializing the
  /// field's value and comparing it to the last serialized
  /// value when the field was persisted/loaded.
  ///
  /// Interface:
  ///   interface DirtyFieldValueContext {
  ///     // The current field value to check.
  ///     value: any;
  ///
  ///     // The field that contains this type
  ///     field: Field;
  ///
  ///     // The name of the field that contains this type
  ///     fieldName: string;
  ///
  ///     // The model instance itself
  ///     self: Model;
  ///
  ///     // The underlying database connection
  ///     // that is attempting to insert or update
  ///     // this field
  ///     connection: Connection;
  ///   }
  ///
  /// Return: any
  ///   If `undefined` is returned, then this field is not considered
  ///   dirty. If any other value is returned, then that will be used
  ///   as the "dirty" value for the field.
  ///
  /// Arguments:
  ///   context: DirtyFieldValueContext
  isDirty(context) {
    let options = this.getOptions();
    if (typeof options.isDirty === 'function')
      return options.isDirty.call(this, context);

    let { value, fieldName, self, connection } = context;
    let currentValue = self._typeData[fieldName];
    if (value == null && currentValue == null)
      return;

    if (this.serialize(value, connection) !== this.serialize(currentValue, connection))
      return value;
  }

  /// Validate the underlying field value.
  /// For the `SerializedType` this simply
  /// always returns `true`.
  ///
  /// Return: boolean
  ///   Always return `true`.
  isValidValue() {
    return true;
  }

  /// Serialize the field's value, and
  /// return the serialized result.
  ///
  /// Note that if no `connection` is provided
  /// the current field's value is simply returned.
  /// This is because it is assumed that serializing
  /// will only be happening when the database driver
  /// connection requests it, otherwise you likely
  /// just want the raw value of the field.
  ///
  /// Return: any
  ///   The underlying field's value if no `connection`
  ///   argument was provided, or the serialized result
  ///   if a `connection` argument was provided.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to serialize. If no `connection` argument
  ///     is provided, then this value will simply be returned.
  ///   connection?: <see>Connection</see>
  ///     The database driver connection requesting that this
  ///     field's value be serialized for storage.
  serialize(value, connection) {
    if (!connection)
      return value;

    let options   = this.getOptions();
    let innerType = options.type;

    if (value != null && innerType.isValidValue(value))
      return value;

    return options.serialize({ value, connection });
  }

  /// Deserialize the field's value, and
  /// return the deserialized result.
  ///
  /// This is called any time `castToType`
  /// is called. If the value provided is
  /// not the same type as the underlying
  /// storage type (i.e. `STRING`), and the
  /// value is also not `null` or `undefined`,
  /// then the value is simply returned. This
  /// is because it is assumed if the value
  /// isn't of the same type as storage
  /// (i.e. `string` type), then it is already
  /// deserialized, so will simply be returned.
  /// `null` and `undefined` are handed directly
  /// off to the provided `serialize` and `deserialize`
  /// methods, so make sure your serializer can
  /// properly handle these values (even if that
  /// means simply returning them).
  ///
  /// Return: any
  ///   The underlying field's value. If the value
  ///   provided has a type that matches the underlying
  ///   storage type (i.e. typeof value === 'string' && internalType === 'STRING')
  ///   then the value will first be passed through the
  ///   provided `deserialize` method before it is returned.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to deserialize. If this is *not*
  ///     the same type as the underlying storage type,
  ///     and is also *not* `null` or `undefined`, then
  ///     the value will simply be returned.
  ///   connection?: <see>Connection</see>
  ///     The database driver connection requesting that this
  ///     field's value be deserialized (usually during load).
  deserialize(value, connection) {
    let options   = this.getOptions();
    let innerType = options.type;

    if (value != null && !innerType.isValidValue(value))
      return value;

    return options.deserialize({ value, connection });
  }

  /// Stringify the type itself. **This will be
  /// the type of the underlying storage field**.
  ///
  /// If a `connection` argument is provided, then this
  /// will go through the connection to generate the type
  /// for the underlying database. If no connection is
  /// provided, then a "standard" SQL type will be returned
  /// for underlying storage field type instead.
  ///
  /// Return: string
  ///
  /// Arguments:
  ///   connection?: <see>Connection</see>
  ///     An optional connection. If provided, send this
  ///     type through <see>Type.toConnectionType</see>
  ///     to have the connection itself generate the underlying
  ///     type for the database. If `connection` is not provided,
  ///     then this will simply return a "standard" generic matching
  ///     SQL type of the underlying storage field type.
  toString(connection) {
    let options   = this.getOptions();
    let innerType = options.type;
    return innerType.toString(connection);
  }

  toConnectionType(connection, options) {
    let typeOptions = this.getOptions();
    let innerType   = typeOptions.type;

    if (!connection)
      return innerType.toString();

    return connection.typeToString(innerType, options);
  }
}

module.exports = {
  SERIALIZED: Type.wrapConstructor(SerializedType),
  SerializedType,
};
