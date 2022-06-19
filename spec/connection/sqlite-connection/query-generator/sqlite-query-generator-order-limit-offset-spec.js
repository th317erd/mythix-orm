/* eslint-disable indent */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../../src/connection/sqlite-connection');

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
});
