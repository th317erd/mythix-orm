/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const ConnectionBase                = require('../../../lib/connection/connection-base');
const { DistinctLiteral, Literal }  = require('../../../lib/connection/literals');

describe('DistinctLiteral', () => {
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
    it('can turn a fully qualified name into a projection field', () => {
      expect((new DistinctLiteral('User:id')).toString(connection)).toEqual('DISTINCT "users"."id" AS "User:id"');
    });

    it('will throw an exception if no field is present', () => {
      expect(() => (new DistinctLiteral()).toString(connection)).toThrow(new TypeError('DistinctLiteral::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "undefined".'));
    });

    it('can turn a raw field into a projection field', () => {
      expect((new DistinctLiteral(User.fields.firstName)).toString(connection)).toEqual('DISTINCT "users"."firstName" AS "User:firstName"');
    });

    it('can provide a SQL literal', () => {
      expect((new DistinctLiteral(new Literal('test'))).toString(connection)).toEqual('DISTINCT test');
    });
  });
});
