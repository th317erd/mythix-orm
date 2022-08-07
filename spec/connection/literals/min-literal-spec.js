/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const ConnectionBase          = require('../../../lib/connection/connection-base');
const { MinLiteral, Literal } = require('../../../lib/connection/literals');

describe('MinLiteral', () => {
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
    it('can turn a fully qualified name into a min projection', () => {
      expect((new MinLiteral('User:id')).toString(connection)).toEqual('MIN("users"."id")');
    });

    it('will throw an exception if no field is present', () => {
      expect(() => (new MinLiteral()).toString(connection)).toThrow(new TypeError('MinLiteral::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "undefined".'));
    });

    it('can turn a raw field into a projection field', () => {
      expect((new MinLiteral(User.fields.firstName)).toString(connection)).toEqual('MIN("users"."firstName")');
    });

    it('can provide a SQL literal', () => {
      expect((new MinLiteral(new Literal('test'))).toString(connection)).toEqual('MIN(test)');
    });
  });
});
