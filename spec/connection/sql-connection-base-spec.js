/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach */

const SQLConnectionBase = require('../../src/connection/sql-connection-base');
const {
  Role,
  User,
} = require('../support/models');

describe('SQLConnectionBase', () => {
  let connection;

  beforeEach(() => {
    connection = new SQLConnectionBase({
      models: [
        User,
        Role,
      ],
    });
  });

  describe('thing', () => {
    it('does', () => {

    });
  });
});
