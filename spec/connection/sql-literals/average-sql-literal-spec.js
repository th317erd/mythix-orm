/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../src/connection/sqlite-connection');
const { AverageSQLLiteral, SQLLiteral } = require('../../../src/connection/sql-literals');

describe('AverageSQLLiteral', () => {
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
      expect((new AverageSQLLiteral('User:id')).toString(connection)).toEqual('AVG("users"."id")');
    });

    it('will throw an exception if no field is present', () => {
      expect(() => (new AverageSQLLiteral()).toString(connection)).toThrow(new TypeError('AverageSQLLiteral::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "undefined".'));
    });

    it('can turn a raw field into a projection field', () => {
      expect((new AverageSQLLiteral(User.fields.firstName)).toString(connection)).toEqual('AVG("users"."firstName")');
    });

    it('can provide a SQL literal', () => {
      expect((new AverageSQLLiteral(new SQLLiteral('test'))).toString(connection)).toEqual('AVG(test)');
    });
  });
});
