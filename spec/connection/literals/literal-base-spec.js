/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { ConnectionBase, Literals } = require('../../../lib');
const { LiteralBase, Literal, CountLiteral, DistinctLiteral } = Literals;

describe('LiteralBase', () => {
  let connection;
  let User;

  beforeAll(() => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });

    let models = connection.getModels();
    User = models.User;
  });

  describe('static isLiteralClass', () => {
    it('returns true for LiteralBase class', () => {
      expect(LiteralBase.isLiteralClass(LiteralBase)).toBe(true);
    });

    it('returns true for Literal class', () => {
      expect(LiteralBase.isLiteralClass(Literal)).toBe(true);
    });

    it('returns true for CountLiteral class', () => {
      expect(LiteralBase.isLiteralClass(CountLiteral)).toBe(true);
    });

    it('returns true for class with _isMythixLiteral', () => {
      class CustomLiteral {
        static _isMythixLiteral = true;
      }
      expect(LiteralBase.isLiteralClass(CustomLiteral)).toBe(true);
    });

    it('returns false for null', () => {
      expect(LiteralBase.isLiteralClass(null)).toBe(false);
    });

    it('returns false for undefined', () => {
      expect(LiteralBase.isLiteralClass(undefined)).toBe(false);
    });

    it('returns false for regular class', () => {
      class NotALiteral {}
      expect(LiteralBase.isLiteralClass(NotALiteral)).toBe(false);
    });
  });

  describe('static isLiteral', () => {
    it('returns true for LiteralBase instance', () => {
      let literal = new Literal('test');
      expect(LiteralBase.isLiteral(literal)).toBe(true);
    });

    it('returns true for CountLiteral instance', () => {
      let literal = new CountLiteral('*');
      expect(LiteralBase.isLiteral(literal)).toBe(true);
    });

    it('returns true for object with _isMythixLiteral constructor', () => {
      class CustomLiteral {
        static _isMythixLiteral = true;
      }
      let instance = new CustomLiteral();
      expect(LiteralBase.isLiteral(instance)).toBe(true);
    });

    it('returns false for null', () => {
      expect(LiteralBase.isLiteral(null)).toBe(false);
    });

    it('returns false for undefined', () => {
      expect(LiteralBase.isLiteral(undefined)).toBe(false);
    });

    it('returns false for plain object', () => {
      expect(LiteralBase.isLiteral({})).toBe(false);
    });

    it('returns false for string', () => {
      expect(LiteralBase.isLiteral('test')).toBe(false);
    });
  });

  describe('instance isLiteral', () => {
    it('returns true with no arguments', () => {
      let literal = new Literal('test');
      expect(literal.isLiteral()).toBe(true);
    });

    it('checks provided value', () => {
      let literal = new Literal('test');
      let other = new CountLiteral('*');
      expect(literal.isLiteral(other)).toBe(true);
    });

    it('returns false for non-literal', () => {
      let literal = new Literal('test');
      expect(literal.isLiteral('string')).toBe(false);
    });
  });

  describe('static isLiteralType', () => {
    it('returns true for matching type', () => {
      let literal = new CountLiteral('*');
      expect(CountLiteral.isLiteralType(literal)).toBe(true);
    });

    it('returns false for different literal type', () => {
      let literal = new Literal('test');
      expect(CountLiteral.isLiteralType(literal)).toBe(false);
    });

    it('returns false for non-literal', () => {
      expect(CountLiteral.isLiteralType('string')).toBe(false);
    });
  });

  describe('static isAggregate', () => {
    it('returns false for LiteralBase', () => {
      expect(LiteralBase.isAggregate()).toBe(false);
    });

    it('returns false for Literal', () => {
      expect(Literal.isAggregate()).toBe(false);
    });
  });

  describe('instance isAggregate', () => {
    it('returns false for basic Literal', () => {
      let literal = new Literal('test');
      expect(literal.isAggregate()).toBe(false);
    });
  });

  describe('constructor', () => {
    it('stores literal value', () => {
      let literal = new Literal('SELECT 1');
      expect(literal.literal).toEqual('SELECT 1');
    });

    it('stores options', () => {
      let literal = new Literal('test', { as: 'alias' });
      expect(literal.options.as).toEqual('alias');
    });

    it('defaults options to empty object', () => {
      let literal = new Literal('test');
      expect(literal.options).toEqual({});
    });

    it('literal and options are non-enumerable', () => {
      let literal = new Literal('test', { key: 'value' });
      let keys = Object.keys(literal);
      expect(keys).not.toContain('literal');
      expect(keys).not.toContain('options');
    });
  });

  describe('fullyQualifiedNameToDefinition', () => {
    it('returns literal unchanged', () => {
      let literal = new Literal('test');
      let other = new CountLiteral('*');
      let result = literal.fullyQualifiedNameToDefinition(other);
      expect(result).toBe(other);
    });

    it('parses fully qualified field name', () => {
      let literal = new Literal('test');
      let result = literal.fullyQualifiedNameToDefinition('User:firstName');
      expect(result.modelName).toEqual('User');
      expect(result.fieldNames).toEqual([ 'firstName' ]);
    });

    it('parses Field instance', () => {
      let literal = new Literal('test');
      let result = literal.fullyQualifiedNameToDefinition(User.fields.firstName);
      expect(result.modelName).toEqual('User');
      expect(result.fieldNames).toEqual([ 'firstName' ]);
    });

    it('throws for undefined', () => {
      let literal = new Literal('test');
      expect(() => {
        literal.fullyQualifiedNameToDefinition(undefined);
      }).toThrow();
    });

    it('throws for invalid field name', () => {
      let literal = new Literal('test');
      expect(() => {
        literal.fullyQualifiedNameToDefinition('');
      }).toThrow();
    });
  });

  describe('definitionToField', () => {
    it('returns literal unchanged', () => {
      let literal = new Literal('test');
      let other = new CountLiteral('*');
      let result = literal.definitionToField(connection, other);
      expect(result).toBe(other);
    });

    it('returns non-definition unchanged', () => {
      let literal = new Literal('test');
      let result = literal.definitionToField(connection, 'string');
      expect(result).toEqual('string');
    });

    it('returns undefined unchanged', () => {
      let literal = new Literal('test');
      let result = literal.definitionToField(connection, undefined);
      expect(result).toBeUndefined();
    });

    it('converts definition to field', () => {
      let literal = new Literal('test');
      let definition = { modelName: 'User', fieldNames: [ 'firstName' ] };
      let result = literal.definitionToField(connection, definition);
      expect(result).toBeDefined();
      expect(result.fieldName).toEqual('firstName');
    });

    it('creates ephemeral field for non-existent field', () => {
      let literal = new Literal('test');
      let definition = { modelName: 'User', fieldNames: [ 'nonexistent' ] };
      let result = literal.definitionToField(connection, definition);
      // The method creates an ephemeral field if the field doesn't exist but model does
      expect(result).toBeDefined();
      expect(result.ephemeral).toBe(true);
      expect(result.fieldName).toEqual('nonexistent');
    });
  });

  describe('toString', () => {
    it('returns literal value as string', () => {
      let literal = new Literal('SELECT 1');
      expect(literal.toString()).toEqual('SELECT 1');
    });

    it('handles null literal', () => {
      let literal = new Literal(null);
      expect(literal.toString()).toEqual('null');
    });

    it('handles undefined literal', () => {
      let literal = new Literal(undefined);
      expect(literal.toString()).toEqual('undefined');
    });

    it('calls toString on object literals', () => {
      let obj = {
        toString() { return 'custom'; },
      };
      let literal = new Literal(obj);
      expect(literal.toString()).toEqual('custom');
    });
  });

  describe('valueOf', () => {
    it('returns raw literal value', () => {
      let literal = new Literal('test');
      expect(literal.valueOf()).toEqual('test');
    });

    it('returns object literal unchanged', () => {
      let obj = { key: 'value' };
      let literal = new Literal(obj);
      expect(literal.valueOf()).toBe(obj);
    });
  });
});
