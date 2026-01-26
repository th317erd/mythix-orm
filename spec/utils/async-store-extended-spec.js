/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect, beforeEach, afterEach */

const { Utils, ConnectionBase } = require('../../lib');
const AllModels = require('../support/models');

describe('AsyncStore Extended', () => {
  let connection;

  beforeEach(() => {
    // Reset debug mode before each test
    Utils.setDebug(false);

    connection = new ConnectionBase({
      bindModels: false,
      models:     AllModels,
    });
  });

  describe('CONNECTION_KEY', () => {
    it('is a Symbol', () => {
      expect(typeof Utils.CONNECTION_KEY).toBe('symbol');
    });

    it('has consistent identity', () => {
      const key1 = Utils.CONNECTION_KEY;
      const key2 = Symbol.for('mythix-orm:connection');
      expect(key1).toBe(key2);
    });

    it('is accessible via AsyncStore namespace', () => {
      expect(Utils.AsyncStore.CONNECTION_KEY).toBe(Utils.CONNECTION_KEY);
    });
  });

  describe('hasContext', () => {
    it('returns false outside context', () => {
      expect(Utils.hasContext()).toBe(false);
    });

    it('returns true inside context', async () => {
      await connection.createContext(() => {
        expect(Utils.hasContext()).toBe(true);
      });
    });

    it('returns false after context exits', async () => {
      await connection.createContext(() => {
        expect(Utils.hasContext()).toBe(true);
      });
      expect(Utils.hasContext()).toBe(false);
    });
  });

  describe('hasConnection', () => {
    it('returns false outside context', () => {
      expect(Utils.hasConnection()).toBe(false);
    });

    it('returns true inside createContext', async () => {
      await connection.createContext(() => {
        expect(Utils.hasConnection()).toBe(true);
      });
    });

    it('returns false in context without connection', () => {
      Utils.runInContext(new Map(), () => {
        expect(Utils.hasContext()).toBe(true);
        expect(Utils.hasConnection()).toBe(false);
      });
    });
  });

  describe('captureContext', () => {
    it('returns a function', () => {
      const captured = Utils.captureContext();
      expect(typeof captured).toBe('function');
    });

    it('captures current context for later use', async () => {
      let runInCaptured;

      await connection.createContext(() => {
        Utils.setContextValue('captureTest', 'capturedValue');
        runInCaptured = Utils.captureContext();
      });

      // Outside original context, run captured context
      expect(Utils.hasContext()).toBe(false);

      const result = runInCaptured(() => {
        return Utils.getContextValue('captureTest');
      });

      expect(result).toEqual('capturedValue');
    });

    it('captures connection for model operations', async () => {
      let runInCaptured;

      await connection.createContext(() => {
        runInCaptured = Utils.captureContext();
      });

      // Verify connection is available in captured context
      const result = runInCaptured(() => {
        return Utils.hasConnection();
      });

      expect(result).toBe(true);
    });

    it('works with async callbacks', async () => {
      let runInCaptured;

      await connection.createContext(() => {
        Utils.setContextValue('asyncTest', 'asyncValue');
        runInCaptured = Utils.captureContext();
      });

      const result = await runInCaptured(async () => {
        await Promise.resolve();
        return Utils.getContextValue('asyncTest');
      });

      expect(result).toEqual('asyncValue');
    });

    it('runs callback without context when captured outside context', () => {
      const runInCaptured = Utils.captureContext();

      let executed = false;
      runInCaptured(() => {
        executed = true;
      });

      expect(executed).toBe(true);
    });
  });

  describe('bindCallback', () => {
    it('returns a function', () => {
      const bound = Utils.bindCallback(() => {});
      expect(typeof bound).toBe('function');
    });

    it('preserves context when callback is invoked later', async () => {
      let boundCallback;

      await connection.createContext(() => {
        Utils.setContextValue('boundTest', 'boundValue');
        boundCallback = Utils.bindCallback(() => {
          return Utils.getContextValue('boundTest');
        });
      });

      // Outside original context
      expect(Utils.hasContext()).toBe(false);
      const result = boundCallback();
      expect(result).toEqual('boundValue');
    });

    it('passes arguments to wrapped callback', async () => {
      let boundCallback;

      await connection.createContext(() => {
        boundCallback = Utils.bindCallback((a, b, c) => {
          return [ a, b, c ];
        });
      });

      const result = boundCallback(1, 'two', { three: 3 });
      expect(result).toEqual([ 1, 'two', { three: 3 } ]);
    });

    it('preserves this context', async () => {
      let boundCallback;
      const customThis = { value: 'custom' };

      await connection.createContext(() => {
        boundCallback = Utils.bindCallback(function() {
          return this.value;
        });
      });

      const result = boundCallback.call(customThis);
      expect(result).toEqual('custom');
    });

    it('works with async callbacks', async () => {
      let boundCallback;

      await connection.createContext(() => {
        Utils.setContextValue('asyncBoundTest', 'asyncBoundValue');
        boundCallback = Utils.bindCallback(async () => {
          await Promise.resolve();
          return Utils.getContextValue('asyncBoundTest');
        });
      });

      const result = await boundCallback();
      expect(result).toEqual('asyncBoundValue');
    });

    it('preserves connection for model access', async () => {
      let boundCallback;

      await connection.createContext(() => {
        boundCallback = Utils.bindCallback(() => {
          return Utils.hasConnection();
        });
      });

      expect(Utils.hasConnection()).toBe(false);
      const result = boundCallback();
      expect(result).toBe(true);
    });
  });

  describe('debug mode', () => {
    afterEach(() => {
      Utils.setDebug(false);
    });

    it('isDebugEnabled returns false by default', () => {
      expect(Utils.isDebugEnabled()).toBe(false);
    });

    it('setDebug enables debug mode', () => {
      Utils.setDebug(true);
      expect(Utils.isDebugEnabled()).toBe(true);
    });

    it('setDebug disables debug mode', () => {
      Utils.setDebug(true);
      Utils.setDebug(false);
      expect(Utils.isDebugEnabled()).toBe(false);
    });

    it('setDebug coerces to boolean', () => {
      Utils.setDebug(1);
      expect(Utils.isDebugEnabled()).toBe(true);

      Utils.setDebug(0);
      expect(Utils.isDebugEnabled()).toBe(false);

      Utils.setDebug('yes');
      expect(Utils.isDebugEnabled()).toBe(true);

      Utils.setDebug('');
      expect(Utils.isDebugEnabled()).toBe(false);
    });
  });

  describe('getContextDebugInfo', () => {
    it('returns object with expected properties', () => {
      const info = Utils.getContextDebugInfo();
      expect(info).toEqual(jasmine.objectContaining({
        hasContext:    jasmine.any(Boolean),
        hasConnection: jasmine.any(Boolean),
        contextKeys:   jasmine.any(Array),
        parentDepth:   jasmine.any(Number),
      }));
    });

    it('shows no context outside createContext', () => {
      const info = Utils.getContextDebugInfo();
      expect(info.hasContext).toBe(false);
      expect(info.hasConnection).toBe(false);
      expect(info.contextKeys).toEqual([]);
      expect(info.parentDepth).toBe(0);
    });

    it('shows context info inside createContext', async () => {
      await connection.createContext(() => {
        const info = Utils.getContextDebugInfo();
        expect(info.hasContext).toBe(true);
        expect(info.hasConnection).toBe(true);
        expect(info.contextKeys.length).toBeGreaterThan(0);
        // Should include CONNECTION_KEY and model names
        expect(info.contextKeys).toContain(Utils.CONNECTION_KEY);
      });
    });

    it('tracks nested context depth', async () => {
      await connection.createContext(async () => {
        const outerInfo = Utils.getContextDebugInfo();
        expect(outerInfo.parentDepth).toBe(0);

        await connection.createContext(() => {
          const innerInfo = Utils.getContextDebugInfo();
          expect(innerInfo.parentDepth).toBe(1);
        });
      });
    });

    it('includes model names in contextKeys', async () => {
      await connection.createContext(() => {
        const info = Utils.getContextDebugInfo();
        expect(info.contextKeys).toContain('User');
        expect(info.contextKeys).toContain('Role');
      });
    });
  });

  describe('Connection.captureContext', () => {
    it('is available on connection instance', () => {
      expect(typeof connection.captureContext).toBe('function');
    });

    it('delegates to Utils.captureContext', async () => {
      let connCaptured;
      let utilsCaptured;

      await connection.createContext(() => {
        Utils.setContextValue('delegateTest', 'delegateValue');
        connCaptured = connection.captureContext();
        utilsCaptured = Utils.captureContext();
      });

      const connResult = connCaptured(() => Utils.getContextValue('delegateTest'));
      const utilsResult = utilsCaptured(() => Utils.getContextValue('delegateTest'));

      expect(connResult).toEqual(utilsResult);
    });
  });

  describe('Connection.bindCallback', () => {
    it('is available on connection instance', () => {
      expect(typeof connection.bindCallback).toBe('function');
    });

    it('preserves context for event handlers', async () => {
      const EventEmitter = require('events');
      const emitter = new EventEmitter();
      let result;

      await connection.createContext(() => {
        Utils.setContextValue('eventTest', 'eventValue');

        emitter.on('test', connection.bindCallback(() => {
          result = Utils.getContextValue('eventTest');
        }));
      });

      // Emit outside context
      expect(Utils.hasContext()).toBe(false);
      emitter.emit('test');

      expect(result).toEqual('eventValue');
    });
  });
});
