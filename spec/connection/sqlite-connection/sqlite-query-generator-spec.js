/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const Types                 = require('../../../src/types');
const { SQLiteConnection }  = require('../../../src/connection/sqlite-connection');
const {
  Role,
  User,
  UserThing,
  RoleThing,
} = require('../../support/models');

class ExtendedUser extends User {
  static fields = User.cloneFields({
    'id': {
      type:         Types.INTEGER,
      defaultValue: Types.INTEGER.Default.AUTO_INCREMENT,
      primaryKey:   true,
      allowNull:    false,
      unique:       true, // should be ignored when create table query is generated
    },
    'createdAt': {
      type:         Types.DATETIME,
      defaultValue: Types.DATETIME.Default.NOW,
      allowNull:    false,
    },
    'email': {
      type:         Types.STRING(256),
      allowNull:    false,
      unique:       true,
    },
    'primaryRole': {
      type:         Types.STRING(256),
      defaultValue: () => {
        return 'user';
      },
      allowNull:    false,
    },
    'playerType': {
      type:         Types.STRING(256),
      defaultValue: 'wizard',
      allowNull:    false,
    },
  });
}

fdescribe('SQLiteQueryGenerator', () => {
  let connection;

  beforeEach(() => {
    connection = new SQLiteConnection({
      models: [
        ExtendedUser,
        Role,
        RoleThing,
        User,
        UserThing,
      ],
    });
  });

  describe('generatorCreateTableStatement', () => {
    it('can generate a create table statement #1', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generatorCreateTableStatement(User)).toEqual('CREATE TABLE IF NOT EXISTS "users" (  "id" VARCHAR(36) PRIMARY KEY,\n  "firstName" VARCHAR(64),\n  "lastName" VARCHAR(64),\n  "primaryRoleID" VARCHAR(36) NOT NULL\n);');
    });

    it('can generate a create table statement #2', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generatorCreateTableStatement(ExtendedUser)).toEqual('CREATE TABLE IF NOT EXISTS "extended_users" (  "id" INTEGER PRIMARY KEY AUTOINCREMENT,\n  "createdAt" DATETIME NOT NULL DEFAULT (datetime(\'now\')),\n  "email" VARCHAR(256) UNIQUE NOT NULL,\n  "firstName" VARCHAR(64),\n  "lastName" VARCHAR(64),\n  "playerType" VARCHAR(256) NOT NULL DEFAULT \'wizard\',\n  "primaryRole" VARCHAR(256) NOT NULL DEFAULT \'user\',\n  "primaryRoleID" VARCHAR(36) NOT NULL\n);');
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
      expect(fieldList).toEqual({
        'User:id':            '"users"."id" AS "User"."id"',
        'User:firstName':     '"users"."firstName" AS "User"."firstName"',
        'User:lastName':      '"users"."lastName" AS "User"."lastName"',
        'User:primaryRoleID': '"users"."primaryRoleID" AS "User"."primaryRoleID"',
        'Role:id':            '"roles"."id" AS "Role"."id"',
        'Role:name':          '"roles"."name" AS "Role"."name"',
      });
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

    it('should throw an error on unknown operator', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(() => queryGenerator.generateSelectQueryOperatorFromQueryEngineOperator('UNKNOWN', null)).toThrow(new Error('SQLiteQueryGenerator::generateSelectQueryOperatorFromQueryEngineOperator: Unknown operator "UNKNOWN".'));
    });
  });

  describe('generateSelectQueryCondition', () => {
    it('can generate a query condition (EQ)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', null)).toEqual('"users"."id" IS NULL');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', true)).toEqual('"users"."id" IS TRUE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', false)).toEqual('"users"."id" IS FALSE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', 'derp')).toEqual('"users"."id" = \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', 1)).toEqual('"users"."id" = 1');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', BigInt(1))).toEqual('"users"."id" = 1');
    });

    it('can generate a query condition for array of items (EQ)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', [ 'stuff', 'derp' ])).toEqual('"users"."id" IN (\'stuff\',\'derp\')');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', [ 'stuff', {} ])).toEqual('"users"."id" IN (\'stuff\')');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', [ 'stuff', 1 ])).toEqual('"users"."id" IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', [ 'stuff', BigInt(1) ])).toEqual('"users"."id" IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', [ 'stuff', 1, undefined ])).toEqual('"users"."id" IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', [ 'stuff', 1, null ])).toEqual('("users"."id" IS NULL OR "users"."id" IN (\'stuff\',1))');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', [ true, false, null ])).toEqual('("users"."id" IS TRUE OR "users"."id" IS FALSE OR "users"."id" IS NULL)');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', [])).toEqual('');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'EQ', [ undefined, {} ])).toEqual('');
    });

    it('can generate a query condition (NEQ)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', null)).toEqual('"users"."id" IS NOT NULL');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', true)).toEqual('"users"."id" IS NOT TRUE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', false)).toEqual('"users"."id" IS NOT FALSE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', 'derp')).toEqual('"users"."id" != \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', 1)).toEqual('"users"."id" != 1');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', BigInt(1))).toEqual('"users"."id" != 1');
    });

    it('can generate a query condition for array of items (NEQ)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', [ 'stuff', 'derp' ])).toEqual('"users"."id" NOT IN (\'stuff\',\'derp\')');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', [ 'stuff', {} ])).toEqual('"users"."id" NOT IN (\'stuff\')');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', [ 'stuff', 1 ])).toEqual('"users"."id" NOT IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', [ 'stuff', BigInt(1) ])).toEqual('"users"."id" NOT IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', [ 'stuff', 1, undefined ])).toEqual('"users"."id" NOT IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', [ 'stuff', 1, null ])).toEqual('("users"."id" IS NOT NULL OR "users"."id" NOT IN (\'stuff\',1))');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', [ true, false, null ])).toEqual('("users"."id" IS NOT TRUE OR "users"."id" IS NOT FALSE OR "users"."id" IS NOT NULL)');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', [])).toEqual('');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'NEQ', [ undefined, {} ])).toEqual('');
    });

    it('can generate a query condition (GT)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GT', null)).toEqual('"users"."id" > NULL');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GT', true)).toEqual('"users"."id" > TRUE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GT', false)).toEqual('"users"."id" > FALSE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GT', 'derp')).toEqual('"users"."id" > \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GT', 1)).toEqual('"users"."id" > 1');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GT', BigInt(1))).toEqual('"users"."id" > 1');
    });

    it('can generate a query condition (GTE)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GTE', null)).toEqual('"users"."id" >= NULL');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GTE', true)).toEqual('"users"."id" >= TRUE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GTE', false)).toEqual('"users"."id" >= FALSE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GTE', 'derp')).toEqual('"users"."id" >= \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GTE', 1)).toEqual('"users"."id" >= 1');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'GTE', BigInt(1))).toEqual('"users"."id" >= 1');
    });

    it('can generate a query condition (LT)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LT', null)).toEqual('"users"."id" < NULL');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LT', true)).toEqual('"users"."id" < TRUE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LT', false)).toEqual('"users"."id" < FALSE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LT', 'derp')).toEqual('"users"."id" < \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LT', 1)).toEqual('"users"."id" < 1');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LT', BigInt(1))).toEqual('"users"."id" < 1');
    });

    it('can generate a query condition (LTE)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LTE', null)).toEqual('"users"."id" <= NULL');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LTE', true)).toEqual('"users"."id" <= TRUE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LTE', false)).toEqual('"users"."id" <= FALSE');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LTE', 'derp')).toEqual('"users"."id" <= \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LTE', 1)).toEqual('"users"."id" <= 1');
      expect(queryGenerator.generateSelectQueryCondition(User.fields.id, 'LTE', BigInt(1))).toEqual('"users"."id" <= 1');
    });
  });

  describe('generateSelectJoinOnTableQueryCondition', () => {
    it('can generate a query condition (as reference)', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(User.fields.primaryRoleID, Role.fields.id, 'EQ')).toEqual('"users"."primaryRoleID" = "roles"."id"');
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(User.fields.primaryRoleID, Role.fields.id, 'NEQ')).toEqual('"users"."primaryRoleID" != "roles"."id"');
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(User.fields.primaryRoleID, Role.fields.id, 'GT')).toEqual('"users"."primaryRoleID" > "roles"."id"');
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(User.fields.primaryRoleID, Role.fields.id, 'GTE')).toEqual('"users"."primaryRoleID" >= "roles"."id"');
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(User.fields.primaryRoleID, Role.fields.id, 'LT')).toEqual('"users"."primaryRoleID" < "roles"."id"');
      expect(queryGenerator.generateSelectJoinOnTableQueryCondition(User.fields.primaryRoleID, Role.fields.id, 'LTE')).toEqual('"users"."primaryRoleID" <= "roles"."id"');
    });
  });

  describe('generateSelectQueryJoinTables', () => {
    it('can generate table join statements from query', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryJoinTables(User.where.primaryRoleID.EQ(Role.where.id))).toEqual('LEFT INNER JOIN "roles" ON "users"."primaryRoleID" = "roles"."id"');
    });

    it('can generate table join statements from query between multiple tables', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryJoinTables(User.where.debug.id.EQ(UserThing.where.userID).AND.UserThing.where.roleThingID.EQ(RoleThing.where.id).AND.RoleThing.where.roleID.EQ(Role.where.id))).toEqual('LEFT INNER JOIN "user_things" ON "users"."id" = "user_things"."userID" LEFT INNER JOIN "role_things" ON "user_things"."roleThingID" = "role_things"."id" LEFT INNER JOIN "roles" ON "role_things"."roleID" = "roles"."id"');
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

  // describe('generateSelectQuery', () => {
  //   it('can generate a select statement #1', () => {
  //     let queryGenerator  = connection.getQueryGenerator();
  //     let fieldList       = Array.from(Object.values(queryGenerator.getEscapedModelFields(User)));
  //     let queryString     = queryGenerator.generateSelectQuery(User.where.id.EQ('derp'));
  //     expect(queryString).toEqual(`SELECT ${fieldList.join(',')} `);
  //   });
  // });
});
