/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const ConnectionBase  = require('../../../lib/connection/connection-base');
const { Literal }     = require('../../../lib/connection/literals');

describe('Literal', () => {
  let connection;

  beforeAll(async () => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     require('../../support/models'),
    });
  });

  describe('toString', () => {
    it('can return anything as a literal', () => {
      expect((new Literal('test')).toString(connection)).toEqual('test');
      expect((new Literal('DERP(stuff)')).toString(connection)).toEqual('DERP(stuff)');
    });
  });
});
