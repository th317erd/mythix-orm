/* eslint-disable indent */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../src/connection/sqlite-connection');

describe('SQLiteQueryGenerator', () => {
  let connection;
  let User;
  let RoleThing;
  let ExtendedUser;

  beforeEach(() => {
    connection = new SQLiteConnection({
      models: require('../../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
    RoleThing = models.RoleThing;
    ExtendedUser = models.ExtendedUser;
  });

  describe('generateCreateTableStatement', () => {
    it('can generate a create table statement #1', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateCreateTableStatement(User)).toEqual('CREATE TABLE IF NOT EXISTS "users" (  "id" VARCHAR(36) PRIMARY KEY,\n  "firstName" VARCHAR(64),\n  "lastName" VARCHAR(64),\n  "primaryRoleID" VARCHAR(36),\n  FOREIGN KEY("primaryRoleID") REFERENCES "roles"("id") ON DELETE SET NULL ON UPDATE SET NULL\n);');
    });

    it('can generate a create table statement #2', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateCreateTableStatement(ExtendedUser)).toEqual('CREATE TABLE IF NOT EXISTS "extended_users" (  "id" INTEGER PRIMARY KEY AUTOINCREMENT,\n  "createdAt" DATETIME NOT NULL DEFAULT (datetime(\'now\')),\n  "email" VARCHAR(256) UNIQUE NOT NULL,\n  "firstName" VARCHAR(64),\n  "lastName" VARCHAR(64),\n  "playerType" VARCHAR(256) NOT NULL DEFAULT \'wizard\',\n  "primaryRole" VARCHAR(256) NOT NULL,\n  "primaryRoleID" VARCHAR(36),\n  FOREIGN KEY("primaryRoleID") REFERENCES "roles"("id") ON DELETE SET NULL ON UPDATE SET NULL\n);');
    });

    it('can generate a create table statement with a foreign key', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateCreateTableStatement(RoleThing)).toEqual('CREATE TABLE IF NOT EXISTS "role_things" (  "id" VARCHAR(36) PRIMARY KEY,\n  "roleID" VARCHAR(36),\n  FOREIGN KEY("roleID") REFERENCES "roles"("id") ON DELETE CASCADE ON UPDATE CASCADE\n);');
    });
  });

  describe('getEscapedModelFields', () => {
    it('can generate escaped field list from model', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let fieldList       = queryGenerator.getEscapedModelFields(User, { asProjection: true });
      expect(fieldList).toEqual({
        'User:id':             '"users"."id" AS "User:id"',
        'User:firstName':      '"users"."firstName" AS "User:firstName"',
        'User:lastName':       '"users"."lastName" AS "User:lastName"',
        'User:primaryRoleID':  '"users"."primaryRoleID" AS "User:primaryRoleID"',
      });
    });
  });

  describe('getEscapedFieldName', () => {
    it('should generate escaped column name', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getEscapedFieldName(User.fields.firstName)).toEqual('"User:firstName"');
    });

    it('should bypass table name when requested to do so', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getEscapedFieldName(User.fields.firstName, { fieldNameOnly: true })).toEqual('"firstName"');
    });
  });

  describe('getEscapedColumnName', () => {
    it('should generate escaped column name', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getEscapedColumnName(User.fields.firstName)).toEqual('"users"."firstName"');
    });

    it('should bypass table name when requested to do so', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getEscapedColumnName(User.fields.firstName, { columnNameOnly: true })).toEqual('"firstName"');
    });
  });

  describe('prepareAllModelsForOperation', () => {
    it('should convert everything to a model', () => {
      let result          = connection.prepareAllModelsForOperation(User, [
        { firstName: 'Test', lastName: 'User' },
        new User({ lastName: 'Brain' }),
      ]);

      expect(result.models[0]).toBeInstanceOf(User);
      expect(result.models[1]).toBeInstanceOf(User);
      expect(result.dirtyFields).toBeInstanceOf(Array);
      expect(result.dirtyFields.length).toEqual(3);
    });

    it('should get dirty fields for models', () => {
      let models = [
        new User(),
        new User(),
      ];

      Object.assign(models[0], { firstName: 'Test' });
      Object.assign(models[1], { lastName: 'User' });

      let result = connection.prepareAllModelsForOperation(User, models);

      expect(result.models[0]).toBeInstanceOf(User);
      expect(result.models[1]).toBeInstanceOf(User);
      expect(result.dirtyFields).toBeInstanceOf(Array);
      expect(result.dirtyFields.length).toEqual(2);

      let dirtyFields = result.dirtyFields.sort((a, b) => {
        if (a.fieldName === b.fieldName)
          return 0;

        return (a.fieldName < b.fieldName) ? -1 : 1;
      });

      expect(dirtyFields.map((field) => field.fieldName)).toEqual([ 'firstName', 'lastName' ]);
    });
  });
});
