/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const { SQLiteConnection } = require('../../../lib/connection/sqlite-connection');
const { SQLLiteral } = require('../../../lib/connection/sql-literals');

describe('SQLLiteral', () => {
  let connection;

  beforeEach(async () => {
    connection = new SQLiteConnection({
      models: require('../../support/models'),
    });
  });

  describe('toString', () => {
    it('can return anything as a literal', () => {
      expect((new SQLLiteral('test')).toString(connection)).toEqual('test');
      expect((new SQLLiteral('DERP(stuff)')).toString(connection)).toEqual('DERP(stuff)');
    });
  });
});
