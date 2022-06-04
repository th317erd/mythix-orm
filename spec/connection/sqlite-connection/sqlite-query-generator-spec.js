/* eslint-disable indent */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../src/connection/sqlite-connection');
const { SQLLiteral } = require('../../../src/connection/sql-literals');

describe('SQLiteQueryGenerator', () => {
  let connection;
  let User;
  let Role;
  let UserThing;
  let RoleThing;
  let ExtendedUser;

  beforeEach(() => {
    connection = new SQLiteConnection({
      models: require('../../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
    Role = models.Role;
    UserThing = models.UserThing;
    RoleThing = models.RoleThing;
    ExtendedUser = models.ExtendedUser;
  });

  describe('getProjectionRequiredFields', () => {
    it('can get required projection fields #1', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getProjectionRequiredFields(User.where.primaryRoleID.EQ(1).ORDER('+id'))).toEqual({
        'User:id': '"users"."id" AS "User"."id"',
      });
    });

    it('can get required projection fields #2', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getProjectionRequiredFields(User.where.primaryRoleID.EQ(1).ORDER('+id', 'primaryRoleID', '-firstName'))).toEqual({
        'User:id':            '"users"."id" AS "User"."id"',
        'User:primaryRoleID': '"users"."primaryRoleID" AS "User"."primaryRoleID"',
        'User:firstName':     '"users"."firstName" AS "User"."firstName"',
      });
    });
  });

  describe('getProjectionFromQueryEngine', () => {
    it('can get projected fields', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getProjectionFromQueryEngine(User.where.primaryRoleID.EQ(1).PROJECT('id'))).toEqual([
        {
          fullFieldName:  'User:id',
          projectedName:  '"users"."id" AS "User"."id"',
          Model:          User,
          Field:          User.fields.id,
          fieldName:      'id',
          modelName:      'User',
          direction:      '+',
        },
      ]);
    });

    it('should not include fields from a model not in use in the query', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getProjectionFromQueryEngine(User.where.primaryRoleID.EQ(1).PROJECT('id', 'Role:id'))).toEqual([
        {
          fullFieldName:  'User:id',
          projectedName:  '"users"."id" AS "User"."id"',
          Model:          User,
          Field:          User.fields.id,
          fieldName:      'id',
          modelName:      'User',
          direction:      '+',
        },
      ]);
    });

    it('will return "*" if no projection present', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getProjectionFromQueryEngine(User.where.primaryRoleID.EQ(1))).toEqual([ '*' ]);
    });

    it('can get projected fields (multiple fields)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getProjectionFromQueryEngine(User.where.primaryRoleID.EQ(1).PROJECT('id').PROJECT('-firstName'))).toEqual([
        {
          fullFieldName:  'User:id',
          projectedName:  '"users"."id" AS "User"."id"',
          Model:          User,
          Field:          User.fields.id,
          fieldName:      'id',
          modelName:      'User',
          direction:      '+',
        },
        {
          fullFieldName:  'User:firstName',
          projectedName:  '"users"."firstName" AS "User"."firstName"',
          Model:          User,
          Field:          User.fields.firstName,
          fieldName:      'firstName',
          modelName:      'User',
          direction:      '-',
        },
      ]);
    });
  });

  describe('generateOrderClause', () => {
    it('can generate proper order clause', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateOrderClause([
        {
          Model:      User,
          Field:      User.fields.id,
          direction: '-',
        },
      ])).toEqual('ORDER BY "users"."id" DESC');
    });

    it('can generate proper order clause with multiple orders', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateOrderClause([
        {
          Model:      User,
          Field:      User.fields.id,
          direction: '-',
        },
        {
          Model:      User,
          Field:      User.fields.firstName,
          direction: '+',
        },
      ])).toEqual('ORDER BY "users"."id" DESC,"users"."firstName" ASC');
    });

    it('should return an empty string if nothing was provided', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateOrderClause()).toEqual('');
      expect(queryGenerator.generateOrderClause([])).toEqual('');
      expect(queryGenerator.generateOrderClause([ null, false, '' ])).toEqual('');
    });
  });

  describe('getOrderLimitOffset', () => {
    it('can get order, limit, and offset from query', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getOrderLimitOffset(User.where.primaryRoleID.EQ(1).LIMIT(100).OFFSET(5).ORDER('+id'))).toEqual({
        limit:  100,
        offset: 5,
        order:  [
          {
            Model:      User,
            Field:      User.fields.id,
            direction:  '+',
          },
        ],
      });
    });

    it('will allow limit to be infinity', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getOrderLimitOffset(User.where.primaryRoleID.EQ(1).LIMIT(Infinity).OFFSET(5).ORDER('+id'))).toEqual({
        limit:  Infinity,
        offset: 5,
        order:  [
          {
            Model:      User,
            Field:      User.fields.id,
            direction:  '+',
          },
        ],
      });
    });

    it('can order should be able to take mixed args', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getOrderLimitOffset(User.where.primaryRoleID.EQ(1).ORDER('+id', [ 'firstName', '-lastName' ], 'primaryRoleID'))).toEqual({
        limit:  undefined,
        offset: undefined,
        order:  [
          {
            Model:      User,
            Field:      User.fields.id,
            direction:  '+',
          },
          {
            Model:      User,
            Field:      User.fields.firstName,
            direction:  '+',
          },
          {
            Model:      User,
            Field:      User.fields.lastName,
            direction:  '-',
          },
          {
            Model:      User,
            Field:      User.fields.primaryRoleID,
            direction:  '+',
          },
        ],
      });
    });

    it('can overwrite order, limit, and offset from query', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.getOrderLimitOffset(User.where.primaryRoleID.EQ(1).LIMIT(100).OFFSET(5).ORDER('+id').LIMIT(200).OFFSET(50).ORDER([ '+id', '--firstName' ]))).toEqual({
        limit:  200,
        offset: 50,
        order:  [
          {
            Model:      User,
            Field:      User.fields.id,
            direction:  '+',
          },
          {
            Model:      User,
            Field:      User.fields.firstName,
            direction:  '-',
          },
        ],
      });
    });

    it('will throw an ambiguous error', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(() => queryGenerator.getOrderLimitOffset(
        User.where
          .primaryRoleID
            .EQ(Role.where.id)
          .LIMIT(100)
          .OFFSET(5)
          .ORDER([ '+id', '-firstName' ]),
      )).toThrow(new Error('QueryGeneratorBase::getOrderLimitOffset: "id" ambiguous. You must use a fully qualified field name for an ORDER clause. Example: "+Model:id".'));
    });

    it('will throw a no fields found error', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(() => queryGenerator.getOrderLimitOffset(
        User.where
          .primaryRoleID
            .EQ(Role.where.id)
          .LIMIT(100)
          .OFFSET(5)
          .ORDER([ 'User:' ]),
      )).toThrow(new Error('QueryGeneratorBase::getOrderLimitOffset: No field names found for "User:".'));
    });

    it('will throw error if it can not find the field', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(() => queryGenerator.getOrderLimitOffset(
        User.where
          .primaryRoleID
            .EQ(Role.where.id)
          .LIMIT(100)
          .OFFSET(5)
          .ORDER([ 'User:derp' ]),
      )).toThrow(new Error('QueryGeneratorBase::getOrderLimitOffset: Unable to locate field "User"."derp".'));
    });
  });

  describe('generateLimitClause', () => {
    it('can generate proper limit clause', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateLimitClause(50)).toEqual('LIMIT 50');
    });
  });

  describe('generateOffsetClause', () => {
    it('can generate proper offset clause', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateOffsetClause(50)).toEqual('OFFSET 50');
    });
  });

  describe('generateSelectOrderLimitOffset', () => {
    it('can generate proper order clause', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectOrderLimitOffset(
        User.where
          .primaryRoleID
            .EQ(1)
          .LIMIT(100)
          .OFFSET(5)
          .ORDER([ '-id' ]),
      )).toEqual('ORDER BY "users"."id" DESC LIMIT 100 OFFSET 5');
    });

    it('can generate nothing', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectOrderLimitOffset(
        User.where
          .primaryRoleID
            .EQ(1),
      )).toEqual('');
    });

    it('can generate proper order clause with multiple orders', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectOrderLimitOffset(
        User.where
          .primaryRoleID
            .EQ(1)
          .LIMIT(100)
          .OFFSET(5)
          .ORDER([ '+id', '-firstName' ]),
      )).toEqual('ORDER BY "users"."id" ASC,"users"."firstName" DESC LIMIT 100 OFFSET 5');
    });

    it('will ignore the limit clause when limit is Infinity', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectOrderLimitOffset(
        User.where
          .primaryRoleID
            .EQ(1)
          .LIMIT(Infinity)
          .OFFSET(5)
          .ORDER([ '+id', '-firstName' ]),
      )).toEqual('ORDER BY "users"."id" ASC,"users"."firstName" DESC OFFSET 5');
    });

    it('will ignore the limit clause when limit is nothing', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectOrderLimitOffset(
        User.where
          .primaryRoleID
            .EQ(1)
          .OFFSET(5)
          .ORDER([ '+id', '-firstName' ]),
      )).toEqual('ORDER BY "users"."id" ASC,"users"."firstName" DESC OFFSET 5');
    });

    it('will ignore the offset clause when offset is nothing', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectOrderLimitOffset(
        User.where
          .primaryRoleID
            .EQ(1)
          .LIMIT(10)
          .ORDER([ '+id', '-firstName' ]),
      )).toEqual('ORDER BY "users"."id" ASC,"users"."firstName" DESC LIMIT 10');
    });

    it('will ignore the limit and offset clause when they are nothing', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectOrderLimitOffset(
        User.where
          .primaryRoleID
            .EQ(1)
          .ORDER([ '+id', '-firstName' ]),
      )).toEqual('ORDER BY "users"."id" ASC,"users"."firstName" DESC');
    });

    it('will ignore the order clause when order is nothing', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectOrderLimitOffset(
        User.where
          .primaryRoleID
            .EQ(1)
          .LIMIT(100)
          .OFFSET(10),
      )).toEqual('LIMIT 100 OFFSET 10');
    });
  });

  describe('generateCreateTableStatement', () => {
    it('can generate a create table statement #1', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateCreateTableStatement(User)).toEqual('CREATE TABLE IF NOT EXISTS "users" (  "id" VARCHAR(36) PRIMARY KEY,\n  "firstName" VARCHAR(64),\n  "lastName" VARCHAR(64),\n  "primaryRoleID" VARCHAR(36) NOT NULL\n);');
    });

    it('can generate a create table statement #2', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateCreateTableStatement(ExtendedUser)).toEqual('CREATE TABLE IF NOT EXISTS "extended_users" (  "id" INTEGER PRIMARY KEY AUTOINCREMENT,\n  "createdAt" DATETIME NOT NULL DEFAULT (datetime(\'now\')),\n  "email" VARCHAR(256) UNIQUE NOT NULL,\n  "firstName" VARCHAR(64),\n  "lastName" VARCHAR(64),\n  "playerType" VARCHAR(256) NOT NULL DEFAULT \'wizard\',\n  "primaryRole" VARCHAR(256) NOT NULL DEFAULT \'user\',\n  "primaryRoleID" VARCHAR(36) NOT NULL\n);');
    });
  });

  describe('getEscapedModelFields', () => {
    it('can generate escaped field list from model', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let fieldList       = queryGenerator.getEscapedModelFields(User);
      expect(fieldList).toEqual({
        'User:id':             '"users"."id" AS "User"."id"',
        'User:firstName':      '"users"."firstName" AS "User"."firstName"',
        'User:lastName':       '"users"."lastName" AS "User"."lastName"',
        'User:primaryRoleID':  '"users"."primaryRoleID" AS "User"."primaryRoleID"',
      });
    });
  });

  describe('getProjectedFields', () => {
    it('can generate escaped field list from projected fields', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let fieldList       = queryGenerator.getProjectedFields(User.where.AND.Role);
      expect(fieldList).toEqual([
        '"roles"."id" AS "Role"."id"',
        '"roles"."name" AS "Role"."name"',
        '"users"."firstName" AS "User"."firstName"',
        '"users"."id" AS "User"."id"',
        '"users"."lastName" AS "User"."lastName"',
        '"users"."primaryRoleID" AS "User"."primaryRoleID"',
      ]);
    });

    it('can set projected fields', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let fieldList       = queryGenerator.getProjectedFields(User.where.PROJECT('User:id'));
      expect(fieldList).toEqual([
        '"users"."id" AS "User"."id"',
      ]);
    });

    it('can set projected fields with literals', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let fieldList       = queryGenerator.getProjectedFields(User.where.PROJECT('User:id', new SQLLiteral('DISTINCT("User"."id")')));
      expect(fieldList).toEqual([
        '"users"."id" AS "User"."id"',
        'DISTINCT("User"."id")',
      ]);
    });

    it('can subtract from projected fields', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let fieldList       = queryGenerator.getProjectedFields(User.where.PROJECT('-User:id'));
      expect(fieldList).toEqual([
        '"users"."firstName" AS "User"."firstName"',
        '"users"."id" AS "User"."id"',
        '"users"."lastName" AS "User"."lastName"',
        '"users"."primaryRoleID" AS "User"."primaryRoleID"',
      ]);

      fieldList = queryGenerator.getProjectedFields(User.where.PROJECT('*', '-User:id'));
      expect(fieldList).toEqual([
        '"users"."firstName" AS "User"."firstName"',
        '"users"."id" AS "User"."id"',
        '"users"."lastName" AS "User"."lastName"',
        '"users"."primaryRoleID" AS "User"."primaryRoleID"',
      ]);
    });
  });

  describe('generateSelectQueryFromTable', () => {
    it('can generate FROM and JOIN statements', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryFromTable('User')).toEqual('FROM "users"');
      expect(queryGenerator.generateSelectQueryFromTable('User', 'LEFT JOIN')).toEqual('LEFT JOIN "users"');
    });
  });

  describe('generateSelectQueryJoinTables', () => {
    it('can generate condition operators for EQ', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', null)).toEqual('IS');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', true)).toEqual('IS');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', false)).toEqual('IS');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', 'derp')).toEqual('=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', 1)).toEqual('=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', BigInt(1))).toEqual('=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', [])).toEqual('IN');
    });

    it('can generate condition operators for EQ (as reference)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', null, true)).toEqual('=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', true, true)).toEqual('=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', false, true)).toEqual('=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', 'derp', true)).toEqual('=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', 1, true)).toEqual('=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', BigInt(1), true)).toEqual('=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('EQ', [], true)).toEqual('=');
    });

    it('can generate condition operators for NEQ', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', null)).toEqual('IS NOT');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', true)).toEqual('IS NOT');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', false)).toEqual('IS NOT');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', 'derp')).toEqual('!=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', 1)).toEqual('!=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', BigInt(1))).toEqual('!=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', [])).toEqual('NOT IN');
    });

    it('can generate condition operators for NEQ (as reference)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', null, true)).toEqual('!=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', true, true)).toEqual('!=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', false, true)).toEqual('!=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', 'derp', true)).toEqual('!=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', 1, true)).toEqual('!=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', BigInt(1), true)).toEqual('!=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('NEQ', [], true)).toEqual('!=');
    });

    it('can generate condition operators for GT', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GT', null)).toEqual('>');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GT', true)).toEqual('>');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GT', false)).toEqual('>');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GT', 'derp')).toEqual('>');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GT', 1)).toEqual('>');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GT', BigInt(1))).toEqual('>');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GT', [])).toEqual('>');
    });

    it('can generate condition operators for GTE', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GTE', null)).toEqual('>=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GTE', true)).toEqual('>=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GTE', false)).toEqual('>=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GTE', 'derp')).toEqual('>=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GTE', 1)).toEqual('>=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GTE', BigInt(1))).toEqual('>=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('GTE', [])).toEqual('>=');
    });

    it('can generate condition operators for LT', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LT', null)).toEqual('<');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LT', true)).toEqual('<');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LT', false)).toEqual('<');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LT', 'derp')).toEqual('<');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LT', 1)).toEqual('<');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LT', BigInt(1))).toEqual('<');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LT', [])).toEqual('<');
    });

    it('can generate condition operators for LTE', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LTE', null)).toEqual('<=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LTE', true)).toEqual('<=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LTE', false)).toEqual('<=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LTE', 'derp')).toEqual('<=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LTE', 1)).toEqual('<=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LTE', BigInt(1))).toEqual('<=');
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('LTE', [])).toEqual('<=');
    });

    it('can generate condition operators with a literal', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator(new SQLLiteral('EXISTS'), null)).toEqual('EXISTS');
    });

    it('should throw an error on unknown operator', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(() => queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('UNKNOWN', null)).toThrow(new Error('SQLiteQueryGenerator::generateSelectQueryOperatorFromQueryEngineOperator: Unknown operator "UNKNOWN".'));
    });
  });

  describe('generateSelectQueryCondition', () => {
    it('can generate a query condition (EQ)', () => {
      const queryPart = { Model: User };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', null)).toEqual('"users"."id" IS NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', true)).toEqual('"users"."id" IS TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', false)).toEqual('"users"."id" IS FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', 'derp')).toEqual('"users"."id" = \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', 1)).toEqual('"users"."id" = 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', BigInt(1))).toEqual('"users"."id" = 1');
    });

    it('can generate a query condition for array of items (EQ)', () => {
      const queryPart = { Model: User };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', [ 'stuff', 'derp' ])).toEqual('"users"."id" IN (\'stuff\',\'derp\')');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', [ 'stuff', {} ])).toEqual('"users"."id" IN (\'stuff\')');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', [ 'stuff', 1 ])).toEqual('"users"."id" IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', [ 'stuff', BigInt(1) ])).toEqual('"users"."id" IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', [ 'stuff', 1, undefined ])).toEqual('"users"."id" IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', [ 'stuff', 1, null ])).toEqual('("users"."id" IS NULL OR "users"."id" IN (\'stuff\',1))');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', [ true, false, null ])).toEqual('("users"."id" IS TRUE OR "users"."id" IS FALSE OR "users"."id" IS NULL)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', [])).toEqual('');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'EQ', [ undefined, {} ])).toEqual('');
    });

    it('can generate a query condition (NEQ)', () => {
      const queryPart = { Model: User };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', null)).toEqual('"users"."id" IS NOT NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', true)).toEqual('"users"."id" IS NOT TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', false)).toEqual('"users"."id" IS NOT FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', 'derp')).toEqual('"users"."id" != \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', 1)).toEqual('"users"."id" != 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', BigInt(1))).toEqual('"users"."id" != 1');
    });

    it('can generate a query condition for array of items (NEQ)', () => {
      const queryPart = { Model: User };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', [ 'stuff', 'derp' ])).toEqual('"users"."id" NOT IN (\'stuff\',\'derp\')');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', [ 'stuff', {} ])).toEqual('"users"."id" NOT IN (\'stuff\')');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', [ 'stuff', 1 ])).toEqual('"users"."id" NOT IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', [ 'stuff', BigInt(1) ])).toEqual('"users"."id" NOT IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', [ 'stuff', 1, undefined ])).toEqual('"users"."id" NOT IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', [ 'stuff', 1, null ])).toEqual('("users"."id" IS NOT NULL OR "users"."id" NOT IN (\'stuff\',1))');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', [ true, false, null ])).toEqual('("users"."id" IS NOT TRUE OR "users"."id" IS NOT FALSE OR "users"."id" IS NOT NULL)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', [])).toEqual('');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'NEQ', [ undefined, {} ])).toEqual('');
    });

    it('can generate a query condition (GT)', () => {
      const queryPart = { Model: User };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GT', null)).toEqual('"users"."id" > NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GT', true)).toEqual('"users"."id" > TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GT', false)).toEqual('"users"."id" > FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GT', 'derp')).toEqual('"users"."id" > \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GT', 1)).toEqual('"users"."id" > 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GT', BigInt(1))).toEqual('"users"."id" > 1');
    });

    it('can generate a query condition (GTE)', () => {
      const queryPart = { Model: User };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GTE', null)).toEqual('"users"."id" >= NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GTE', true)).toEqual('"users"."id" >= TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GTE', false)).toEqual('"users"."id" >= FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GTE', 'derp')).toEqual('"users"."id" >= \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GTE', 1)).toEqual('"users"."id" >= 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'GTE', BigInt(1))).toEqual('"users"."id" >= 1');
    });

    it('can generate a query condition (LT)', () => {
      const queryPart = { Model: User };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LT', null)).toEqual('"users"."id" < NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LT', true)).toEqual('"users"."id" < TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LT', false)).toEqual('"users"."id" < FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LT', 'derp')).toEqual('"users"."id" < \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LT', 1)).toEqual('"users"."id" < 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LT', BigInt(1))).toEqual('"users"."id" < 1');
    });

    it('can generate a query condition (LTE)', () => {
      const queryPart = { Model: User };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LTE', null)).toEqual('"users"."id" <= NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LTE', true)).toEqual('"users"."id" <= TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LTE', false)).toEqual('"users"."id" <= FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LTE', 'derp')).toEqual('"users"."id" <= \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LTE', 1)).toEqual('"users"."id" <= 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, 'LTE', BigInt(1))).toEqual('"users"."id" <= 1');
    });

    it('can generate a query condition (using literals)', () => {
      const queryPart = { Model: User };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, User.fields.id, new SQLLiteral('EXISTS IN'), new SQLLiteral('(1,2,3,4)'))).toEqual('"users"."id" EXISTS IN (1,2,3,4)');
    });
  });

  describe('generateSelectJoinOnTableQueryCondition', () => {
    it('can generate a query condition (as reference)', () => {
      const leftQueryPart = { Model: User };
      const rightQueryPart = { Model: Role };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(leftQueryPart, rightQueryPart, User.fields.primaryRoleID, Role.fields.id, 'EQ')).toEqual('"users"."primaryRoleID" = "roles"."id"');
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(leftQueryPart, rightQueryPart, User.fields.primaryRoleID, Role.fields.id, 'NEQ')).toEqual('"users"."primaryRoleID" != "roles"."id"');
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(leftQueryPart, rightQueryPart, User.fields.primaryRoleID, Role.fields.id, 'GT')).toEqual('"users"."primaryRoleID" > "roles"."id"');
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(leftQueryPart, rightQueryPart, User.fields.primaryRoleID, Role.fields.id, 'GTE')).toEqual('"users"."primaryRoleID" >= "roles"."id"');
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(leftQueryPart, rightQueryPart, User.fields.primaryRoleID, Role.fields.id, 'LT')).toEqual('"users"."primaryRoleID" < "roles"."id"');
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(leftQueryPart, rightQueryPart, User.fields.primaryRoleID, Role.fields.id, 'LTE')).toEqual('"users"."primaryRoleID" <= "roles"."id"');
    });
  });

  describe('generateSelectQueryJoinTables', () => {
    it('can generate table join statements from query', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryJoinTables(
        User.where.primaryRoleID.EQ(Role.where.id),
      )).toEqual('LEFT INNER JOIN "roles" ON "users"."primaryRoleID" = "roles"."id"');
    });

    it('can generate table join statements from query between multiple tables', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryJoinTables(
        User.where
          .id
            .EQ(UserThing.where.userID)
        .AND
        .UserThing
          .roleThingID
            .EQ(RoleThing.where.id)
        .AND
        .RoleThing
          .roleID
            .EQ(Role.where.id),
      )).toEqual('LEFT INNER JOIN "user_things" ON "users"."id" = "user_things"."userID" LEFT INNER JOIN "role_things" ON "user_things"."roleThingID" = "role_things"."id" LEFT INNER JOIN "roles" ON "role_things"."roleID" = "roles"."id"');
    });

    it('should assume PK if a model is provided', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryJoinTables(User.where.primaryRoleID.EQ(Role))).toEqual('LEFT INNER JOIN "roles" ON "users"."primaryRoleID" = "roles"."id"');
    });

    it('should not generate anything if no tables are being joined', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryJoinTables(User.where.primaryRoleID.EQ('derp').AND.id.EQ('hello'))).toEqual('');
    });

    it('should throw an error if the join point has a condition', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(() => queryGenerator.generateSelectQueryJoinTables(User.where.primaryRoleID.EQ(Role.where.id.EQ('derp')))).toThrow(new Error('SQLiteQueryGenerator::generateSelectJoinOnTableQueryConditions: Invalid operation: Expected a field to join on, but instead received a query.'));
    });
  });

  describe('generateSelectWhereConditions', () => {
    it('can generate where statements for query', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectWhereConditions(User.where.primaryRoleID.EQ(Role.where.id).AND.firstName.EQ('Joe').OR.firstName.EQ('Mary'))).toEqual('"users"."firstName" = \'Joe\' OR "users"."firstName" = \'Mary\'');
    });

    it('can generate where statements for query, grouping conditions', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectWhereConditions(
        User.where
          .primaryRoleID
            .EQ(Role.where.id)
          .AND(
            User.where
              .firstName
                .EQ('Joe')
              .OR
              .firstName
                .EQ('Mary'),
          )
          .AND(
            User.where
              .lastName
                .EQ('Derp')
              .OR
              .lastName
                .EQ('Burp'),
          ),
      )).toEqual('("users"."firstName" = \'Joe\' OR "users"."firstName" = \'Mary\') AND ("users"."lastName" = \'Derp\' OR "users"."lastName" = \'Burp\')');
    });
  });

  describe('generateSelectQuery', () => {
    it('can generate a select statement with a table join', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectQuery(
        User.where
          .primaryRoleID
            .EQ(Role.where.id)
          .AND(
            User.where
              .firstName
                .EQ('Joe')
              .OR
              .firstName
                .EQ('Mary'),
          )
          .AND(
            User.where
              .lastName
                .EQ('Derp')
              .OR
              .lastName
                .EQ('Burp'),
          ),
      );
      expect(queryString).toEqual('SELECT "roles"."id" AS "Role"."id","roles"."name" AS "Role"."name","users"."firstName" AS "User"."firstName","users"."id" AS "User"."id","users"."lastName" AS "User"."lastName","users"."primaryRoleID" AS "User"."primaryRoleID" LEFT INNER JOIN "roles" ON "users"."primaryRoleID" = "roles"."id" WHERE ("users"."firstName" = \'Joe\' OR "users"."firstName" = \'Mary\') AND ("users"."lastName" = \'Derp\' OR "users"."lastName" = \'Burp\')');
    });

    it('can generate a select statement with an order, limit, and offset', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectQuery(
        User.where
          .primaryRoleID
            .EQ(1)
          .ORDER('+id')
          .LIMIT(100)
          .OFFSET(500),
      );
      expect(queryString).toEqual('SELECT "users"."firstName" AS "User"."firstName","users"."id" AS "User"."id","users"."lastName" AS "User"."lastName","users"."primaryRoleID" AS "User"."primaryRoleID" WHERE "users"."primaryRoleID" = 1 ORDER BY "users"."id" ASC LIMIT 100 OFFSET 500');
    });

    it('can generate a select statement using literals', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectQuery(
        User.where
          .primaryRoleID
            .EQ(new SQLLiteral('NOW'))
          .ORDER('+id')
          .LIMIT(100)
          .OFFSET(500),
      );
      expect(queryString).toEqual('SELECT "users"."firstName" AS "User"."firstName","users"."id" AS "User"."id","users"."lastName" AS "User"."lastName","users"."primaryRoleID" AS "User"."primaryRoleID" WHERE "users"."primaryRoleID" = NOW ORDER BY "users"."id" ASC LIMIT 100 OFFSET 500');
    });

    it('can generate a select statement with a complex join statement', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectQuery(
        User.where
          .id
            .EQ(UserThing.where.userID)
        .AND
        .UserThing
          .roleThingID
            .EQ(RoleThing.where.id)
        .AND
        .RoleThing
          .roleID
            .EQ(Role.where.id)
        .AND
        .User
        .firstName
          .EQ('Jonny')
        .AND
        .lastName
          .EQ('Bob'),
      );
      expect(queryString).toEqual('SELECT "role_things"."id" AS "RoleThing"."id","role_things"."roleID" AS "RoleThing"."roleID","roles"."id" AS "Role"."id","roles"."name" AS "Role"."name","user_things"."id" AS "UserThing"."id","user_things"."roleThingID" AS "UserThing"."roleThingID","user_things"."userID" AS "UserThing"."userID","users"."firstName" AS "User"."firstName","users"."id" AS "User"."id","users"."lastName" AS "User"."lastName","users"."primaryRoleID" AS "User"."primaryRoleID" LEFT INNER JOIN "user_things" ON "users"."id" = "user_things"."userID" LEFT INNER JOIN "role_things" ON "user_things"."roleThingID" = "role_things"."id" LEFT INNER JOIN "roles" ON "role_things"."roleID" = "roles"."id" WHERE "users"."firstName" = \'Jonny\' AND "users"."lastName" = \'Bob\'');
    });

    it('can generate a select statement with a complex join statement and an order, limit, and offset', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectQuery(
        User.where
          .id
            .EQ(UserThing.where.userID)
        .AND
        .UserThing
          .roleThingID
            .EQ(RoleThing.where.id)
        .AND
        .RoleThing
          .roleID
            .EQ(Role.where.id)
        .AND
        .User
        .firstName
          .EQ('Jonny')
        .AND
        .lastName
          .EQ('Bob')
        .ORDER('User:firstName')
        .LIMIT(100)
        .OFFSET(500),
      );
      expect(queryString).toEqual('SELECT "role_things"."id" AS "RoleThing"."id","role_things"."roleID" AS "RoleThing"."roleID","roles"."id" AS "Role"."id","roles"."name" AS "Role"."name","user_things"."id" AS "UserThing"."id","user_things"."roleThingID" AS "UserThing"."roleThingID","user_things"."userID" AS "UserThing"."userID","users"."firstName" AS "User"."firstName","users"."id" AS "User"."id","users"."lastName" AS "User"."lastName","users"."primaryRoleID" AS "User"."primaryRoleID" LEFT INNER JOIN "user_things" ON "users"."id" = "user_things"."userID" LEFT INNER JOIN "role_things" ON "user_things"."roleThingID" = "role_things"."id" LEFT INNER JOIN "roles" ON "role_things"."roleID" = "roles"."id" WHERE "users"."firstName" = \'Jonny\' AND "users"."lastName" = \'Bob\' ORDER BY "users"."firstName" ASC LIMIT 100 OFFSET 500');
    });
  });
});
