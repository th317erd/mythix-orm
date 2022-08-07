/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const ConnectionBase              = require('../../../lib/connection/connection-base');
const { AverageLiteral, Literal } = require('../../../lib/connection/literals');

describe('AverageLiteral', () => {
  let connection;
  let User;

  beforeAll(async () => {
    try {
      connection = new ConnectionBase({
        bindModels: false,
        models:     require('../../support/models'),
      });

      let models = connection.getModels();
      User = models.User;
    } catch (error) {
      console.error('Error in beforeAll: ', error);
    }
  });

  describe('toString', () => {
    it('can turn a fully qualified name into a min projection', () => {
      expect((new AverageLiteral('User:id')).toString(connection)).toEqual('AVG("users"."id")');
    });

    it('will throw an exception if no field is present', () => {
      expect(() => (new AverageLiteral()).toString(connection)).toThrow(new TypeError('AverageLiteral::fullyQualifiedNameToDefinition: Unable to find field for fully qualified name "undefined".'));
    });

    it('can turn a raw field into a projection field', () => {
      expect((new AverageLiteral(User.fields.firstName)).toString(connection)).toEqual('AVG("users"."firstName")');
    });

    it('can provide a SQL literal', () => {
      expect((new AverageLiteral(new Literal('test'))).toString(connection)).toEqual('AVG(test)');
    });
  });
});
