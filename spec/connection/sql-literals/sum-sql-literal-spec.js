/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../lib/connection/sqlite-connection');
const { SumSQLLiteral, SQLLiteral } = require('../../../lib/connection/sql-literals');

describe('SumSQLLiteral', () => {
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
    it('can turn a fully qualified name into a min projection', () => {
      expect((new SumSQLLiteral('User:id')).toString(connection)).toEqual('SUM("users"."id")');
    });

    it('will throw an exception if no field is present', () => {
      expect(() => (new SumSQLLiteral()).toString(connection)).toThrow(new TypeError('SumSQLLiteral::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "undefined".'));
    });

    it('can turn a raw field into a projection field', () => {
      expect((new SumSQLLiteral(User.fields.firstName)).toString(connection)).toEqual('SUM("users"."firstName")');
    });

    it('can provide a SQL literal', () => {
      expect((new SumSQLLiteral(new SQLLiteral('test'))).toString(connection)).toEqual('SUM(test)');
    });
  });
});
