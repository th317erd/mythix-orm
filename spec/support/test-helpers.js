'use strict';

const UUID_REGEXP         = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/;
const XID_REGEXP          = /^[0-9abcdefghjkmnpqrstvwxyz]{20}$/;
const ISO8601_TIME_REGEXP = /\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.\d{3}Z/;
const ISO8601_DATE_REGEXP = /\d{4}-\d{2}-\d{2}T00:00:00.000Z/;

/* global it, fit */
const _it = it;
const _fit = fit;

function createIT(func, getConnection) {
  return function it(desc, runner) {
    return func.call(this, desc, async () => {
      await getConnection().createContext(runner);
    });
  };
}

function createFIT(func, getConnection) {
  return function fit(desc, runner) {
    return func.call(this, desc, async () => {
      await getConnection().createContext(runner);
    });
  };
}

function createRunners(getConnection) {
  return {
    it:   createIT(_it, getConnection),
    fit:  createFIT(_fit, getConnection),
  };
}

module.exports = {
  UUID_REGEXP,
  XID_REGEXP,
  ISO8601_TIME_REGEXP,
  ISO8601_DATE_REGEXP,
  createRunners,
};
