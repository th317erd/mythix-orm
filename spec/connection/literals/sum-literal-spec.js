/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const ConnectionBase          = require('../../../lib/connection/connection-base');
const { SumLiteral, Literal } = require('../../../lib/connection/literals');

describe('SumLiteral', () => {
  let connection;
  let User;

  beforeEach(async () => {
    connection = new ConnectionBase({
      models: require('../../support/models'),
    });

    let models = connection.getModels();

    User = models.User;
  });

  describe('toString', () => {
    it('can turn a fully qualified name into a min projection', () => {
      expect((new SumLiteral('User:id')).toString(connection)).toEqual('SUM("users"."id")');
    });

    it('will throw an exception if no field is present', () => {
      expect(() => (new SumLiteral()).toString(connection)).toThrow(new TypeError('SumLiteral::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "undefined".'));
    });

    it('can turn a raw field into a projection field', () => {
      expect((new SumLiteral(User.fields.firstName)).toString(connection)).toEqual('SUM("users"."firstName")');
    });

    it('can provide a SQL literal', () => {
      expect((new SumLiteral(new Literal('test'))).toString(connection)).toEqual('SUM(test)');
    });
  });
});
