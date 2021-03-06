/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../lib/connection/sqlite-connection');
const { MinSQLLiteral, SQLLiteral } = require('../../../lib/connection/sql-literals');

describe('MinSQLLiteral', () => {
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
      expect((new MinSQLLiteral('User:id')).toString(connection)).toEqual('MIN("users"."id")');
    });

    it('will throw an exception if no field is present', () => {
      expect(() => (new MinSQLLiteral()).toString(connection)).toThrow(new TypeError('MinSQLLiteral::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "undefined".'));
    });

    it('can turn a raw field into a projection field', () => {
      expect((new MinSQLLiteral(User.fields.firstName)).toString(connection)).toEqual('MIN("users"."firstName")');
    });

    it('can provide a SQL literal', () => {
      expect((new MinSQLLiteral(new SQLLiteral('test'))).toString(connection)).toEqual('MIN(test)');
    });
  });
});
