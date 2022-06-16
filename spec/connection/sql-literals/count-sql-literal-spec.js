/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../src/connection/sqlite-connection');
const { CountSQLLiteral, SQLLiteral } = require('../../../src/connection/sql-literals');

describe('CountSQLLiteral', () => {
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
    it('can turn a fully qualified name into a count projection', () => {
      expect((new CountSQLLiteral('User:id')).toString(connection)).toEqual('COUNT("users"."id")');
    });

    it('will default to star if no field is present', () => {
      expect((new CountSQLLiteral()).toString(connection)).toEqual('COUNT(*)');
    });

    it('can turn a raw field into a projection field', () => {
      expect((new CountSQLLiteral(User.fields.firstName)).toString(connection)).toEqual('COUNT("users"."firstName")');
    });

    it('can provide a SQL literal', () => {
      expect((new CountSQLLiteral(new SQLLiteral('test'))).toString(connection)).toEqual('COUNT(test)');
    });
  });
});
