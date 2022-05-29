/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, beforeEach */

const SQLConnectionBase = require('../../src/connection/sql-connection-base');
const {
  Role,
  User,
} = require('../support/models');

describe('SQLConnectionBase', () => {
  beforeEach(() => {
    new SQLConnectionBase({
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
