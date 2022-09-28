'use strict';

/// Field class, that holds the properties
/// of all field descriptors that define
/// the model's fields.
///
/// Example:
///   class User extends Model {
///     // Each field below is termed a "field descriptor"
///     // and will eventually be turned into a Field.
///     static fields = {
///       id: {
///         type:         Types.BIGINT,
///         defaultValue: Types.BIGINT.Default.AUTO_INCREMENT,
///         allowNull:    false,
///         primaryKey:   true,
///       },
///       email: {
///         type:         Types.STRING(64),
///         allowNull:    false,
///         index:        true,
///         unique:       true,
///         valiate:      (value) => {
///           if (('' + value).indexOf('@') < 0)
///             throw new Error(`Invalid email address: "${value}"`);
///         },
///       },
///       gender: {
///         type:         Types.STRING(32),
///         allowNull:    false,
///         index:        true,
///         defaultValue: 'other',
///         valiate:      (value) => {
///           if (!(/^(male|female|other)$/).test(value))
///             throw new Error(`Invalid gender: "${value}"`);
///         },
///       },
///       prefix: {
///         type:         Types.STRING(10),
///         allowNull:    true,
///         index:        true,
///         get:          ({ self, value }) => {
///           if (self.gender === 'male')
///             return 'Mr.';
///           else if (self.gender === 'female')
///             return 'Ms.';
///           else
///             return value;
///         },
///         set:          ({ set, value }) => {
///           set(value);
///         },
///       },
///       firstName: {
///         type:         Types.STRING(32),
///         allowNull:    false,
///         index:        true,
///       },
///       lastName: {
///         type:         Types.STRING(32),
///         allowNull:    false,
///         index:        true,
///       },
///     };
///   }
///
/// Interface:
///   interface DefaultValueContext {
///     _initial: boolean;        // If `true`, then this is the initial set of the field's value.
///     _static: boolean;         // If `true`, then this will be used in a "static" context, such as the `DEFAULT` of the DB.
///     connection: Connection;   // The connection from the model.
///     data: object | undefined; // All attributes provided to the model upon instantiation.
///     field: Field;             // The field descriptor where this `defaultValue` is defined.
///     fieldName: string;        // The name of this field where this `defaultValue` is defined.
///     fieldValue: any;          // The field value as provided to the model instance upon instantiation.
///     self: Model;              // The model instance.
///   }
///
/// Interface:
///   interface GetSetContext {
///     field: Field;       // The field descriptor of this field.
///     fieldName: string;  // The field name of this field.
///     get: Function;      // A method to fetch the current value of this field.
///     self: Model;        // The model instance of this field value.
///     set: Function;      // A method to set the current value of this field.
///     value: any;         // The current value of this field.
///   }
///
/// Properties:
///   type: Type
///     The type of the field. Can be either a virtual or concrete type.
///     You can supply either a `Type` instance (`type: new Types.BigIntType()`),
///     a `Type` class (`type: Types.BigIntType`), a type wrapper
///     (`type: Types.BIGINT`), or a `Type` instance created through a type
///     wrapper (`type: Types.BIGINT(8)`).
///   primaryKey: boolean = false
///     If `true`, then this field will be the primary key of the model.
///     It will also be marked as the primary key in the underlying database.
///   fieldName: string
///     The name of this field. If not provided, this will
///     be the key that defined the field. If your model
///     fields are defined as an `Array` or `Set`, then this
///     attribute is required, and must be supplied. If
///     your fields are defined as an `Object`, or a `Map`,
///     then if this attribute is not supplied, the key
///     will be used as the `fieldName` instead. The
///     `fieldName` always takes priority, so if you
///     have a key and a `fieldName`, then the field name
///     will take precedence over the key name.
///  allowNull: boolean = true
///    If `true`, then this field is allowed to have a `NULL`
///    value in the underlying database. If `false`, then this
///    field must not have a `NULL` value in the underlying database.
///    This has no effect client-side in JavaScript. Even if `allowNull`
///    is `false`, the field property on your model may still have a
///    `null` value in JavaScript.
///  index: Array<boolean | string> | boolean = false
///    If `true`, then this field will be indexed in the database.
///    If an array is provided, then it must be an array containing
///    other field names to combine with this field to create a combined
///    index. For example: `index: [ true, 'firstName', 'lastName', [ 'firstName', 'lastName' ] ]`
///    would create four indexes: `true` means index this field, `firstName` would create
///    the combination index `this_field_first_name`, `lastName` would create the
///    combination index `this_field_last_name`, and `[ 'firstName', 'lastName ]` would
///    create the combination index `this_field_first_name_last_name`.
///  unique: boolean = false
///    If `true`, then add a unique constraint to this field in
///    the underlying database.
///  defaultValue: any | Function(context: DefaultValueContext)
///    Provide a default value for this field. `defaultValue` in
///    Mythix ORM can be either a literal value, such as a `string`
///    or a `number`, it can be `null`, or it can be a function. If
///    it is a function, it will be called when the model is initialized
///    if no value for the field was provided to the model during instantiation.
///    As a method, it can have numerous different "flags" set on it, via
///    the <see>DefaultHelpers.defaultValueFlags</see> factory. Reference this
///    methods documentation for a better understanding of what default value
///    flags are, and what they do.
///  get: Function(context: GetSetContext)
///    A "getter" for this field. If defined, this will be called any time
///    the field is accessed. The return value will be the value of the field.
///    Use the `get` method provided by the `context` argument to get the underlying
///    field value, or use <see>Model.getDataValue</see> to get the underlying value.<br>
///    *Note: The provided `get` method, and <see>Model.getDataValue</see> will bypass this method.*<br>
///    *Note: Do NOT get the field property directly on the model inside this method, or you will enter an infinite recursive loop.*
///  set: Function(context: GetSetContext)
///    A "setter" for this field. If defined, this will be called any time
///    the field is set. This field must call the `set` method provided by
///    the `context` argument, or it must call <see>Model.setDataValue</see>
///    on the `self` model instance provided by the `context`.<br>
///    *Note: The provided `set` method, and <see>Model.setDataValue</see> will bypass this method.*<br>
///    *Note: Do NOT set the field name directly on the model inside this method, or you will enter an infinite recursive loop.*
///  validate: async Function(value: any, context: { connection, Model, options })
///    If this method is provided, then it will be called from <see>Model.onValidate</see>
///    immediately before an insert or update operation. The return value of this method
///    is ignored by Mythix ORM (though if you want, you can do something with it if you
///    overload the `onValidate` method of the model). If validation fails, it is expected
///    that this method will throw an error. Notice that RegExp or pattern values are not
///    supported. `validate` MUST be a method. How you implement it is up to you.
///    The `value` argument is the current value of the field. The `context` argument
///    is provided by the underlying connection. The `Model` inside the context is the
///    model class of the current model that is being validated. The `options` inside
///    the context is simply an object containing the current options for the underlying
///    connection operation.
class Field {
  /// This helps with type checking
  static _isMythixField = true;

  /// Check if the provided `value` is
  /// a <see>Field</see> class.
  ///
  /// This will check if the provided value's
  /// `prototype` is an instance of <see>Field</see>
  /// if it is, this method will return `true`.
  /// If that check fails, then it will see if
  /// the provided value has a `_isMythixField`
  /// property that is `true`. If this is the
  /// case, then this method will return `true`.
  /// Finally, if all checks have failed, this
  /// method will return false.
  ///
  /// Return: boolean
  ///   `true` if the provided value is a <see>Field</see>
  ///    class, or inherits from <see>Field</see>. `false`
  ///    otherwise.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check.
  static isFieldClass(value) {
    if (!value)
      return false;

    if (value.prototype instanceof Field)
      return true;

    if (value._isMythixField)
      return true;

    return false;
  }

  /// Check if the provided `value` is
  /// a <see>Field</see> instance.
  ///
  /// This will check if the provided value
  /// is an `instanceof` <see>Field</see>,
  /// if it is this method will return `true`.
  /// If that check fails, then it will see if
  /// the provided value has a `value.constructor_isMythixField`
  /// property that is `true`. If this is the
  /// case, then this method will return `true`.
  /// Finally, if all checks have failed, this
  /// method will return false.
  ///
  /// Return: boolean
  ///   `true` if the provided value is a <see>Field</see>
  ///    instance, or inherits from <see>Field</see>. `false`
  ///    otherwise.
  ///
  /// Arguments:
  ///   value: any
  ///     The value to check.
  static isField(value) {
    if (!value)
      return false;

    if (value instanceof Field)
      return true;

    if (value.constructor && value.constructor._isMythixField)
      return true;

    return false;
  }

  /// Construct a Field.
  ///
  /// This method will create a new instance of <see>Field</see>.
  /// The provided `fieldDefinition` argument is expected to
  /// be another <see>Field</see>, or an iterable object containing
  /// the field attributes.
  ///
  /// Return: <see>Field</see>
  ///
  /// Arguments:
  ///   fieldDescription: object | <see>Field</see>
  ///     The attributes to provide to this field.
  constructor(fieldDefinition) {
    this.primaryKey = false;
    this.allowNull = true;
    this.index = false;
    this.unique = false;
    this.columnName = (fieldDefinition && fieldDefinition.fieldName);

    Object.assign(this, fieldDefinition || {});
  }

  /// Clone this field instance.
  ///
  /// Return: <see>Field</see>
  ///   This field, cloned into a new instance.
  clone() {
    const FieldClass = this.constructor;
    return new FieldClass(this);
  }

  /// Set the parent <see>Model</see> class
  /// for this field.
  ///
  /// Return: undefined
  ///
  /// Arguments:
  ///   Model: class <see>Model</see>
  ///     The parent model class of this field.
  setModel(Model) {
    this.Model = Model;
  }
}

module.exports = Field;
