/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const ConnectionBase          = require('../../../lib/connection/connection-base');
const { MaxLiteral, Literal } = require('../../../lib/connection/literals');

describe('MaxLiteral', () => {
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
      expect((new MaxLiteral('User:id')).toString(connection)).toEqual('MAX("users"."id")');
    });

    it('will throw an exception if no field is present', () => {
      expect(() => (new MaxLiteral()).toString(connection)).toThrow(new TypeError('MaxLiteral::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "undefined".'));
    });

    it('can turn a raw field into a projection field', () => {
      expect((new MaxLiteral(User.fields.firstName)).toString(connection)).toEqual('MAX("users"."firstName")');
    });

    it('can provide a SQL literal', () => {
      expect((new MaxLiteral(new Literal('test'))).toString(connection)).toEqual('MAX(test)');
    });
  });
});
