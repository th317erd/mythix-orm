/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeAll */

const { ConnectionBase, Utils } = require('../../lib');
const AllModels = require('../support/models');

describe('Connection Context', () => {
  let connection;
  let User;
  let Role;

  beforeAll(() => {
    connection = new ConnectionBase({
      bindModels: false,
      models:     AllModels,
    });

    let models = connection.getModels();
    User = models.User;
    Role = models.Role;
  });

  describe('buildConnectionContext', () => {
    it('creates a Map instance', () => {
      let context = connection.buildConnectionContext();
      expect(context instanceof Map).toBe(true);
    });

    it('sets connection in context', () => {
      let context = connection.buildConnectionContext();
      expect(context.get(Utils.CONNECTION_KEY)).toBe(connection);
    });

    it('sets context for each model', () => {
      let context = connection.buildConnectionContext();
      expect(context.has('User')).toBe(true);
      expect(context.has('Role')).toBe(true);
    });

    it('each model context contains connection', () => {
      let context = connection.buildConnectionContext();
      let userContext = context.get('User');
      expect(userContext.connection).toBe(connection);
    });

    it('accepts custom connection argument', () => {
      let otherConnection = new ConnectionBase({
        bindModels: false,
        models:     AllModels,
      });
      let context = connection.buildConnectionContext(otherConnection);
      expect(context.get(Utils.CONNECTION_KEY)).toBe(otherConnection);
    });
  });

  describe('createContext', () => {
    it('executes callback', async () => {
      let executed = false;
      await connection.createContext(() => {
        executed = true;
      });
      expect(executed).toBe(true);
    });

    it('returns callback result', async () => {
      const result = await connection.createContext(() => {
        return 'test result';
      });
      expect(result).toEqual('test result');
    });

    it('provides connection to callback', async () => {
      await connection.createContext((conn) => {
        expect(conn).toBe(connection);
      });
    });

    it('makes connection available via getContextValue', async () => {
      await connection.createContext(() => {
        let conn = Utils.getContextValue(Utils.CONNECTION_KEY);
        expect(conn).toBe(connection);
      });
    });

    it('makes model contexts available', async () => {
      await connection.createContext(() => {
        let userContext = Utils.getContextValue('User');
        expect(userContext).toBeDefined();
        expect(userContext.connection).toBe(connection);
      });
    });

    it('supports async callbacks', async () => {
      const result = await connection.createContext(async () => {
        await Promise.resolve();
        return 'async result';
      });
      expect(result).toEqual('async result');
    });

    it('preserves context through async calls', async () => {
      await connection.createContext(async () => {
        Utils.setContextValue('testKey', 'testValue');

        await Promise.resolve();

        const value = Utils.getContextValue('testKey');
        expect(value).toEqual('testValue');
      });
    });
  });

  describe('context isolation', () => {
    it('isolates contexts between createContext calls', async () => {
      await connection.createContext(() => {
        Utils.setContextValue('isolatedKey', 'value1');
      });

      await connection.createContext(() => {
        const value = Utils.getContextValue('isolatedKey');
        expect(value).toBeUndefined();
      });
    });

    it('supports nested contexts', async () => {
      await connection.createContext(async () => {
        Utils.setContextValue('outer', 'outerValue');

        await connection.createContext(async () => {
          expect(Utils.getContextValue('outer')).toEqual('outerValue');
          Utils.setContextValue('inner', 'innerValue');
        });

        expect(Utils.getContextValue('inner')).toBeUndefined();
      });
    });
  });

  describe('context value access', () => {
    it('getContextValue returns undefined outside context', () => {
      const value = connection.getContextValue('anyKey');
      expect(value).toBeUndefined();
    });

    it('getContextValue returns value inside context', async () => {
      await connection.createContext(() => {
        connection.setContextValue('myKey', 'myValue');
        const value = connection.getContextValue('myKey');
        expect(value).toEqual('myValue');
      });
    });

    it('getContextValue returns default value when key not found', async () => {
      await connection.createContext(() => {
        const value = connection.getContextValue('missingKey', 'defaultVal');
        expect(value).toEqual('defaultVal');
      });
    });

    it('setContextValue does nothing outside context', () => {
      connection.setContextValue('key', 'value');
      const value = connection.getContextValue('key');
      expect(value).toBeUndefined();
    });
  });

  describe('concurrent context isolation', () => {
    it('isolates concurrent contexts', async () => {
      const results = await Promise.all([
        connection.createContext(async () => {
          Utils.setContextValue('concurrent', 'first');
          await new Promise((r) => setTimeout(r, 10));
          return Utils.getContextValue('concurrent');
        }),
        connection.createContext(async () => {
          Utils.setContextValue('concurrent', 'second');
          await new Promise((r) => setTimeout(r, 5));
          return Utils.getContextValue('concurrent');
        }),
      ]);

      expect(results[0]).toEqual('first');
      expect(results[1]).toEqual('second');
    });
  });

  describe('custom thisArg', () => {
    it('uses custom thisArg for callback', async () => {
      const customThis = { custom: true };
      await connection.createContext(function() {
        expect(this).toBe(customThis);
      }, null, customThis);
    });

    it('defaults to connection as thisArg', async () => {
      await connection.createContext(function() {
        expect(this).toBe(connection);
      });
    });
  });

  describe('custom connection parameter', () => {
    it('uses custom connection in context', async () => {
      const otherConnection = new ConnectionBase({
        bindModels: false,
        models:     AllModels,
      });

      await connection.createContext((conn) => {
        expect(conn).toBe(otherConnection);
        const contextConn = Utils.getContextValue(Utils.CONNECTION_KEY);
        expect(contextConn).toBe(otherConnection);
      }, otherConnection);
    });
  });

  describe('model-per-connection binding', () => {
    it('each model gets correct connection in context', async () => {
      await connection.createContext(() => {
        let userContext = Utils.getContextValue('User');
        let roleContext = Utils.getContextValue('Role');

        expect(userContext.connection).toBe(connection);
        expect(roleContext.connection).toBe(connection);
      });
    });

    it('preserves model context properties', async () => {
      await connection.createContext(() => {
        let userContext = Utils.getContextValue('User');
        expect(userContext.connection).toBeDefined();
      });
    });
  });

  describe('async propagation', () => {
    it('propagates through setImmediate', async () => {
      await connection.createContext(async () => {
        Utils.setContextValue('immediateKey', 'immediateValue');

        await new Promise((resolve) => {
          setImmediate(() => {
            const value = Utils.getContextValue('immediateKey');
            expect(value).toEqual('immediateValue');
            resolve();
          });
        });
      });
    });

    it('propagates through setTimeout', async () => {
      await connection.createContext(async () => {
        Utils.setContextValue('timeoutKey', 'timeoutValue');

        await new Promise((resolve) => {
          setTimeout(() => {
            const value = Utils.getContextValue('timeoutKey');
            expect(value).toEqual('timeoutValue');
            resolve();
          }, 1);
        });
      });
    });

    it('propagates through Promise chains', async () => {
      await connection.createContext(async () => {
        Utils.setContextValue('promiseKey', 'promiseValue');

        const value = await Promise.resolve()
          .then(() => Promise.resolve())
          .then(() => Utils.getContextValue('promiseKey'));

        expect(value).toEqual('promiseValue');
      });
    });
  });
});
