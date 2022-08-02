/* eslint-disable indent */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../../lib/connection/sqlite-connection');
const { SQLLiteral } = require('../../../../lib/connection/sql-literals');

describe('SQLiteQueryGenerator', () => {
  let connection;
  let User;
  let Role;

  beforeEach(() => {
    connection = new SQLiteConnection({
      models: require('../../../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
    Role = models.Role;
  });

  describe('getProjectionRequiredFields', () => {
    it('can get required projection fields #1', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.getProjectionRequiredFields(User.where.primaryRoleID.EQ(1).ORDER('+id'));

      let mappedResult = [];
      for (let [ key, value ] of result)
        mappedResult.push({ [key]: value.projectedName });

      expect(mappedResult).toEqual([
        { 'User:id': '"users"."id" AS "User:id"' },
      ]);
    });

    it('can get required projection fields #2', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.getProjectionRequiredFields(User.where.primaryRoleID.EQ(1).ORDER('+id', 'primaryRoleID', '-firstName'));

      let mappedResult = [];
      for (let [ key, value ] of result)
        mappedResult.push({ [key]: value.projectedName });

      expect(mappedResult).toEqual([
        { 'User:id':            '"users"."id" AS "User:id"' },
        { 'User:primaryRoleID': '"users"."primaryRoleID" AS "User:primaryRoleID"' },
        { 'User:firstName':     '"users"."firstName" AS "User:firstName"' },
      ]);
    });
  });

  describe('getProjectionFromQueryEngine', () => {
    it('can get projected fields', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.getProjectionFromQueryEngine(User.where.primaryRoleID.EQ(1).PROJECT('firstName'));

      expect(result).toEqual([
        {
          fullFieldName:  'User:firstName',
          projectedName:  '"users"."firstName" AS "User:firstName"',
          Model:          User,
          Field:          User.fields.firstName,
          fieldName:      'firstName',
          modelName:      'User',
          direction:      '+',
        },
      ]);
    });

    it('should not include fields from a model not in use in the query', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.getProjectionFromQueryEngine(User.where.primaryRoleID.EQ(1).PROJECT('id', 'Role:id'));

      expect(result).toEqual([
        {
          fullFieldName:  'User:id',
          projectedName:  '"users"."id" AS "User:id"',
          Model:          User,
          Field:          User.fields.id,
          fieldName:      'id',
          modelName:      'User',
          direction:      '+',
        },
      ]);
    });

    it('should be able to reset the projection', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.getProjectionFromQueryEngine(User.where.primaryRoleID.EQ(1).PROJECT('+primaryRoleID', '+id').PROJECT('-').PROJECT('+id'));

      expect(result).toEqual([
        {
          fullFieldName:  'User:id',
          projectedName:  '"users"."id" AS "User:id"',
          Model:          User,
          Field:          User.fields.id,
          fieldName:      'id',
          modelName:      'User',
          direction:      '+',
        },
      ]);
    });

    it('will return root model fields if no projection present', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.getProjectionFromQueryEngine(Role.where.name.EQ('test'));

      expect(result).toEqual([
        {
          projectedName:  '"roles"."id" AS "Role:id"',
          Model:          Role,
          Field:          Role.fields.id,
          direction:      '+',
          fullFieldName:  'Role:id',
          fieldName:      'id',
          modelName:      'Role',
        },
        {
          projectedName:  '"roles"."name" AS "Role:name"',
          Model:          Role,
          Field:          Role.fields.name,
          direction:      '+',
          fullFieldName:  'Role:name',
          fieldName:      'name',
          modelName:      'Role',
        },
      ]);
    });

    it('can project a raw field', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.getProjectionFromQueryEngine(User.where.PROJECT(User.fields.id));

      expect(result).toEqual([
        {
          fullFieldName:  'User:id',
          projectedName:  '"users"."id" AS "User:id"',
          Model:          User,
          Field:          User.fields.id,
          fieldName:      'id',
          modelName:      'User',
          direction:      '+',
        },
      ]);
    });

    it('can get projected fields (multiple fields)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getProjectionFromQueryEngine(User.where.primaryRoleID.EQ(1).PROJECT('id').PROJECT('-firstName'))).toEqual([
        {
          fullFieldName:  'User:id',
          projectedName:  '"users"."id" AS "User:id"',
          Model:          User,
          Field:          User.fields.id,
          fieldName:      'id',
          modelName:      'User',
          direction:      '+',
        },
      ]);
    });
  });

  describe('getProjectedFields', () => {
    it('can generate escaped field list from projected fields', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let fieldList       = queryGenerator.getProjectedFields(User.where.AND.Role.PROJECT('*'));

      expect(fieldList).toEqual([
        '"roles"."id" AS "Role:id"',
        '"roles"."name" AS "Role:name"',
        '"users"."firstName" AS "User:firstName"',
        '"users"."id" AS "User:id"',
        '"users"."lastName" AS "User:lastName"',
        '"users"."primaryRoleID" AS "User:primaryRoleID"',
      ]);
    });

    it('can set projected fields', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let fieldList       = queryGenerator.getProjectedFields(User.where.PROJECT('User:id'));
      expect(fieldList).toEqual([
        '"users"."id" AS "User:id"',
      ]);
    });

    it('can set projected fields with literals', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let fieldList       = queryGenerator.getProjectedFields(User.where.PROJECT('User:id', new SQLLiteral('DISTINCT "users"."firstName" AS "User:firstName"')));

      expect(fieldList).toEqual([
        'DISTINCT "users"."firstName" AS "User:firstName"',
        '"users"."id" AS "User:id"',
      ]);
    });

    it('can subtract from projected fields', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let fieldList       = queryGenerator.getProjectedFields(User.where.PROJECT('-User:id'));
      expect(fieldList).toEqual([
        '"users"."firstName" AS "User:firstName"',
        '"users"."lastName" AS "User:lastName"',
        '"users"."primaryRoleID" AS "User:primaryRoleID"',
      ]);

      fieldList = queryGenerator.getProjectedFields(User.where.PROJECT('*', '-User:id'));
      expect(fieldList).toEqual([
        '"users"."firstName" AS "User:firstName"',
        '"users"."lastName" AS "User:lastName"',
        '"users"."primaryRoleID" AS "User:primaryRoleID"',
      ]);
    });
  });
});
