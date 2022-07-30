/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../lib/connection/sqlite-connection');
const { DistinctSQLLiteral, SQLLiteral } = require('../../../lib/connection/sql-literals');

describe('DistinctSQLLiteral', () => {
  let connection;
  let User;

  beforeEach(async () => {
    connection = new SQLiteConnection({
      models: require('../../support/models'),
    });

    let models = connection.getModels();

    User = models.User;
  });

  describe('toString', () => {
    it('can turn a fully qualified name into a projection field', () => {
      expect((new DistinctSQLLiteral('User:id')).toString(connection)).toEqual('DISTINCT "users"."id" AS "User:id"');
    });

    it('will throw an exception if no field is present', () => {
      expect(() => (new DistinctSQLLiteral()).toString(connection)).toThrow(new TypeError('DistinctSQLLiteral::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "undefined".'));
    });

    it('can turn a raw field into a projection field', () => {
      expect((new DistinctSQLLiteral(User.fields.firstName)).toString(connection)).toEqual('DISTINCT "users"."firstName" AS "User:firstName"');
    });

    it('can provide a SQL literal', () => {
      expect((new DistinctSQLLiteral(new SQLLiteral('test'))).toString(connection)).toEqual('DISTINCT test');
    });
  });
});
