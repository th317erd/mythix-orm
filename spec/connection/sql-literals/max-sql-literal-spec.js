/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../src/connection/sqlite-connection');
const { MaxSQLLiteral, SQLLiteral } = require('../../../src/connection/sql-literals');

describe('MaxSQLLiteral', () => {
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
      expect((new MaxSQLLiteral('User:id')).toString(connection)).toEqual('MAX("users"."id")');
    });

    it('will throw an exception if no field is present', () => {
      expect(() => (new MaxSQLLiteral()).toString(connection)).toThrow(new TypeError('MaxSQLLiteral::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "undefined".'));
    });

    it('can turn a raw field into a projection field', () => {
      expect((new MaxSQLLiteral(User.fields.firstName)).toString(connection)).toEqual('MAX("users"."firstName")');
    });

    it('can provide a SQL literal', () => {
      expect((new MaxSQLLiteral(new SQLLiteral('test'))).toString(connection)).toEqual('MAX(test)');
    });
  });
});
