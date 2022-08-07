/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const ConnectionBase            = require('../../../lib/connection/connection-base');
const { CountLiteral, Literal } = require('../../../lib/connection/literals');

describe('CountLiteral', () => {
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
    it('can turn a fully qualified name into a count projection', () => {
      expect((new CountLiteral('User:id')).toString(connection)).toEqual('COUNT("users"."id")');
    });

    it('will default to star if no field is present', () => {
      expect((new CountLiteral()).toString(connection)).toEqual('COUNT(*)');
    });

    it('can turn a raw field into a projection field', () => {
      expect((new CountLiteral(User.fields.firstName)).toString(connection)).toEqual('COUNT("users"."firstName")');
    });

    it('can provide a SQL literal', () => {
      expect((new CountLiteral(new Literal('test'))).toString(connection)).toEqual('COUNT(test)');
    });
  });
});
