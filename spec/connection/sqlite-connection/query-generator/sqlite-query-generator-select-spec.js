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
  let UserThing;
  let RoleThing;

  beforeEach(() => {
    connection = new SQLiteConnection({
      models: require('../../../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
    Role = models.Role;
    UserThing = models.UserThing;
    RoleThing = models.RoleThing;
  });

  describe('generateFromTableOrTableJoin', () => {
    it('can generate FROM and JOIN statements', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateFromTableOrTableJoin(User)).toEqual('FROM "users"');
      expect(queryGenerator.generateFromTableOrTableJoin(User, 'LEFT JOIN')).toEqual('LEFT JOIN "users"');
    });
  });

  describe('generateSelectQueryOperatorFromQueryEngineOperator', () => {
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
      const queryPart = { Model: User, Field: User.fields.id, not: false, operator: 'EQ', inverseOperator: 'NEQ' };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, null)).toEqual('"users"."id" IS NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, true)).toEqual('"users"."id" IS TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, false)).toEqual('"users"."id" IS FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 'derp')).toEqual('"users"."id" = \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 1)).toEqual('"users"."id" = 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, BigInt(1))).toEqual('"users"."id" = 1');
    });

    it('can generate a query condition for array of items (EQ)', () => {
      const queryPart = { Model: User, Field: User.fields.id, not: false, operator: 'EQ', inverseOperator: 'NEQ' };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', 'derp' ])).toEqual('"users"."id" IN (\'stuff\',\'derp\')');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', {} ])).toEqual('"users"."id" IN (\'stuff\')');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', 1 ])).toEqual('"users"."id" IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', BigInt(1) ])).toEqual('"users"."id" IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', 1, undefined ])).toEqual('"users"."id" IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', 1, null ])).toEqual('("users"."id" IS NULL OR "users"."id" IN (\'stuff\',1))');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ true, false, null ])).toEqual('("users"."id" IS TRUE OR "users"."id" IS FALSE OR "users"."id" IS NULL)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [])).toEqual('');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ undefined, {} ])).toEqual('');
    });

    it('can generate a query condition (NEQ)', () => {
      const queryPart = { Model: User, Field: User.fields.id, not: true, operator: 'EQ', inverseOperator: 'NEQ' };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, null)).toEqual('"users"."id" IS NOT NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, true)).toEqual('"users"."id" IS NOT TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, false)).toEqual('"users"."id" IS NOT FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 'derp')).toEqual('"users"."id" != \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 1)).toEqual('"users"."id" != 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, BigInt(1))).toEqual('"users"."id" != 1');
    });

    it('can generate a query condition for array of items (NEQ)', () => {
      const queryPart = { Model: User, Field: User.fields.id, not: true, operator: 'EQ', inverseOperator: 'NEQ' };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', 'derp' ])).toEqual('"users"."id" NOT IN (\'stuff\',\'derp\')');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', {} ])).toEqual('"users"."id" NOT IN (\'stuff\')');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', 1 ])).toEqual('"users"."id" NOT IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', BigInt(1) ])).toEqual('"users"."id" NOT IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', 1, undefined ])).toEqual('"users"."id" NOT IN (\'stuff\',1)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ 'stuff', 1, null ])).toEqual('("users"."id" IS NOT NULL OR "users"."id" NOT IN (\'stuff\',1))');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ true, false, null ])).toEqual('("users"."id" IS NOT TRUE OR "users"."id" IS NOT FALSE OR "users"."id" IS NOT NULL)');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [])).toEqual('');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, [ undefined, {} ])).toEqual('');
    });

    it('can generate a query condition (GT)', () => {
      const queryPart = { Model: User, Field: User.fields.id, not: false, operator: 'GT', inverseOperator: 'LTE' };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, null)).toEqual('"users"."id" > NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, true)).toEqual('"users"."id" > TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, false)).toEqual('"users"."id" > FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 'derp')).toEqual('"users"."id" > \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 1)).toEqual('"users"."id" > 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, BigInt(1))).toEqual('"users"."id" > 1');
    });

    it('can generate a query condition (GTE)', () => {
      const queryPart = { Model: User, Field: User.fields.id, not: false, operator: 'GTE', inverseOperator: 'LT' };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, null)).toEqual('"users"."id" >= NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, true)).toEqual('"users"."id" >= TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, false)).toEqual('"users"."id" >= FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 'derp')).toEqual('"users"."id" >= \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 1)).toEqual('"users"."id" >= 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, BigInt(1))).toEqual('"users"."id" >= 1');
    });

    it('can generate a query condition (LT)', () => {
      const queryPart = { Model: User, Field: User.fields.id, not: false, operator: 'LT', inverseOperator: 'GTE' };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, null)).toEqual('"users"."id" < NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, true)).toEqual('"users"."id" < TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, false)).toEqual('"users"."id" < FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 'derp')).toEqual('"users"."id" < \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 1)).toEqual('"users"."id" < 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, BigInt(1))).toEqual('"users"."id" < 1');
    });

    it('can generate a query condition (LTE)', () => {
      const queryPart = { Model: User, Field: User.fields.id, not: false, operator: 'LTE', inverseOperator: 'GT' };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, null)).toEqual('"users"."id" <= NULL');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, true)).toEqual('"users"."id" <= TRUE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, false)).toEqual('"users"."id" <= FALSE');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 'derp')).toEqual('"users"."id" <= \'derp\'');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, 1)).toEqual('"users"."id" <= 1');
      expect(queryGenerator.generateSelectQueryCondition(queryPart, BigInt(1))).toEqual('"users"."id" <= 1');
    });

    it('can generate a query condition (using literals)', () => {
      const queryPart = { Model: User, Field: User.fields.id, not: false, operator: new SQLLiteral('EXISTS IN') };

      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryCondition(queryPart, new SQLLiteral('(1,2,3,4)'))).toEqual('"users"."id" EXISTS IN (1,2,3,4)');
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
    it('can generate a complex join', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectQueryJoinTables(
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
        // This is silly... but it is just for testing :)
        .RoleThing
          .id
            .EQ(Role.where.name)
        .AND
        .User
        .firstName
          .EQ('Jonny')
        .AND
        .lastName
          .EQ('Bob'),
      );

      expect(queryString).toEqual('INNER JOIN "user_things" ON "user_things"."userID" = "users"."id" INNER JOIN "role_things" ON "role_things"."id" = "user_things"."roleThingID" INNER JOIN "roles" ON "roles"."id" = "role_things"."roleID" AND "roles"."name" = "role_things"."id"');
    });

    it('can generate table join statements from query', () => {
      let queryGenerator = connection.getQueryGenerator();
      let queryString = queryGenerator.generateSelectQueryJoinTables(
        User.where.primaryRoleID.EQ(Role.where.id),
      );

      expect(queryString).toEqual('INNER JOIN "roles" ON "roles"."id" = "users"."primaryRoleID"');
    });

    it('can generate table join statements from query between multiple tables', () => {
      let queryGenerator = connection.getQueryGenerator();
      let queryString = queryGenerator.generateSelectQueryJoinTables(
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
      );

      expect(queryString).toEqual('INNER JOIN "user_things" ON "user_things"."userID" = "users"."id" INNER JOIN "role_things" ON "role_things"."id" = "user_things"."roleThingID" INNER JOIN "roles" ON "roles"."id" = "role_things"."roleID"');
    });

    it('should assume PK if a model is provided', () => {
      let queryGenerator = connection.getQueryGenerator();
      let queryString = queryGenerator.generateSelectQueryJoinTables(User.where.primaryRoleID.EQ(Role));
      expect(queryString).toEqual('INNER JOIN "roles" ON "roles"."id" = "users"."primaryRoleID"');
    });

    it('should not generate anything if no tables are being joined', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(queryGenerator.generateSelectQueryJoinTables(User.where.primaryRoleID.EQ('derp').AND.id.EQ('hello'))).toEqual('');
    });

    it('should throw an error if the join point has a condition', () => {
      let queryGenerator = connection.getQueryGenerator();
      expect(() => queryGenerator.generateSelectQueryJoinTables(User.where.primaryRoleID.EQ(Role.where.id.EQ('derp')))).toThrow(new Error('SQLiteQueryGenerator::getJoinTableInfoFromQueryEngine: Invalid operation: Expected a field to join on, but instead received a query.'));
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

  describe('generateSelectStatement', () => {
    it('can generate a select statement with a table join', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectStatement(
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

      expect(queryString).toEqual('SELECT "roles"."id" AS "Role:id","roles"."name" AS "Role:name","users"."firstName" AS "User:firstName","users"."id" AS "User:id","users"."lastName" AS "User:lastName","users"."primaryRoleID" AS "User:primaryRoleID" FROM "users" INNER JOIN "roles" ON "roles"."id" = "users"."primaryRoleID" WHERE ("users"."firstName" = \'Joe\' OR "users"."firstName" = \'Mary\') AND ("users"."lastName" = \'Derp\' OR "users"."lastName" = \'Burp\') ORDER BY "users"."rowid" ASC');
    });

    it('can generate a select statement with an order, limit, and offset', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectStatement(
        User.where
          .primaryRoleID
            .EQ(1)
          .ORDER('+id')
          .LIMIT(100)
          .OFFSET(500),
      );
      expect(queryString).toEqual('SELECT "users"."firstName" AS "User:firstName","users"."id" AS "User:id","users"."lastName" AS "User:lastName","users"."primaryRoleID" AS "User:primaryRoleID" FROM "users" WHERE "users"."primaryRoleID" = 1 ORDER BY "users"."id" ASC LIMIT 100 OFFSET 500');
    });

    it('can generate a select statement with a DISTINCT clause', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectStatement(
        User.where
          .DISTINCT
          .primaryRoleID
            .EQ(1)
          .LIMIT(100)
          .OFFSET(500),
      );
      expect(queryString).toEqual('SELECT "users"."firstName" AS "User:firstName",DISTINCT "users"."id" AS "User:id","users"."lastName" AS "User:lastName","users"."primaryRoleID" AS "User:primaryRoleID" FROM "users" WHERE "users"."primaryRoleID" = 1 ORDER BY "users"."rowid" ASC LIMIT 100 OFFSET 500');
    });

    it('can generate a select statement using literals', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectStatement(
        User.where
          .primaryRoleID
            .EQ(new SQLLiteral('NOW'))
          .ORDER('+id')
          .LIMIT(100)
          .OFFSET(500),
      );
      expect(queryString).toEqual('SELECT "users"."firstName" AS "User:firstName","users"."id" AS "User:id","users"."lastName" AS "User:lastName","users"."primaryRoleID" AS "User:primaryRoleID" FROM "users" WHERE "users"."primaryRoleID" = NOW ORDER BY "users"."id" ASC LIMIT 100 OFFSET 500');
    });

    it('should return projection fields', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let result          = queryGenerator.generateSelectStatement(
        User.where
          .primaryRoleID
            .EQ(1),
        { returnFieldProjection: true },
      );

      expect(typeof result).toEqual('object');
      expect(result.sql).toEqual('SELECT "users"."firstName" AS "User:firstName","users"."id" AS "User:id","users"."lastName" AS "User:lastName","users"."primaryRoleID" AS "User:primaryRoleID" FROM "users" WHERE "users"."primaryRoleID" = 1 ORDER BY "users"."rowid" ASC');
      expect(Array.from(result.projectionFields.keys())).toEqual([
        'User:firstName',
        'User:id',
        'User:lastName',
        'User:primaryRoleID',
      ]);
      expect(Array.from(result.projectionFields.values())).toEqual([
        '"users"."firstName" AS "User:firstName"',
        '"users"."id" AS "User:id"',
        '"users"."lastName" AS "User:lastName"',
        '"users"."primaryRoleID" AS "User:primaryRoleID"',
      ]);
    });

    it('can generate a select statement with a complex join statement', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectStatement(
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

      expect(queryString).toEqual('SELECT "roles"."id" AS "Role:id","roles"."name" AS "Role:name","role_things"."id" AS "RoleThing:id","role_things"."roleID" AS "RoleThing:roleID","users"."firstName" AS "User:firstName","users"."id" AS "User:id","users"."lastName" AS "User:lastName","users"."primaryRoleID" AS "User:primaryRoleID","user_things"."id" AS "UserThing:id","user_things"."roleThingID" AS "UserThing:roleThingID","user_things"."userID" AS "UserThing:userID" FROM "users" INNER JOIN "user_things" ON "user_things"."userID" = "users"."id" INNER JOIN "role_things" ON "role_things"."id" = "user_things"."roleThingID" INNER JOIN "roles" ON "roles"."id" = "role_things"."roleID" WHERE "users"."firstName" = \'Jonny\' AND "users"."lastName" = \'Bob\' ORDER BY "users"."rowid" ASC');
    });

    it('can generate a select statement with a complex join statement and an order, limit, and offset', () => {
      let queryGenerator  = connection.getQueryGenerator();
      let queryString     = queryGenerator.generateSelectStatement(
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

      expect(queryString).toEqual('SELECT "roles"."id" AS "Role:id","roles"."name" AS "Role:name","role_things"."id" AS "RoleThing:id","role_things"."roleID" AS "RoleThing:roleID","users"."firstName" AS "User:firstName","users"."id" AS "User:id","users"."lastName" AS "User:lastName","users"."primaryRoleID" AS "User:primaryRoleID","user_things"."id" AS "UserThing:id","user_things"."roleThingID" AS "UserThing:roleThingID","user_things"."userID" AS "UserThing:userID" FROM "users" INNER JOIN "user_things" ON "user_things"."userID" = "users"."id" INNER JOIN "role_things" ON "role_things"."id" = "user_things"."roleThingID" INNER JOIN "roles" ON "roles"."id" = "role_things"."roleID" WHERE "users"."firstName" = \'Jonny\' AND "users"."lastName" = \'Bob\' ORDER BY "users"."firstName" ASC LIMIT 100 OFFSET 500');
    });
  });
});
