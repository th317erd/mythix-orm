/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const ConnectionBase            = require('../../../lib/connection/connection-base');
const { FieldLiteral, Literal } = require('../../../lib/connection/literals');

describe('FieldLiteral', () => {
  let connection;
  let User;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });

    let models = connection.getModels();

    User = models.User;
  });

  describe('toString', () => {
    it('can turn a fully qualified name into a projection field', () => {
      expect((new FieldLiteral('User:id')).toString(connection)).toEqual('"users"."id"');
    });

    it('will throw an exception if no field is present', () => {
      expect(() => (new FieldLiteral()).toString(connection)).toThrow(new TypeError('FieldLiteral::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "undefined".'));
    });

    it('can turn a raw field into a projection field', () => {
      expect((new FieldLiteral(User.fields.firstName)).toString(connection)).toEqual('"users"."firstName"');
    });

    it('can provide a SQL literal', () => {
      expect((new FieldLiteral(new Literal('test'))).toString(connection)).toEqual('test');
    });
  });
});
