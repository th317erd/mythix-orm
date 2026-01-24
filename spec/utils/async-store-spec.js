/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const { Utils } = require('../../lib');
const { AsyncStore } = Utils;

describe('AsyncStore', () => {
  describe('runInContext', () => {
    it('executes callback within context', () => {
      let executed = false;
      AsyncStore.runInContext(null, () => {
        executed = true;
      });
      expect(executed).toBe(true);
    });

    it('returns callback result', () => {
      const result = AsyncStore.runInContext(null, () => {
        return 'test result';
      });
      expect(result).toEqual('test result');
    });

    it('creates new context when null provided', () => {
      AsyncStore.runInContext(null, () => {
        const store = AsyncStore.getContextStore();
        expect(store).toBeDefined();
        expect(store.context).toBeDefined();
        expect(store.context instanceof Map).toBe(true);
      });
    });

    it('uses provided context Map', () => {
      const customContext = new Map([[ 'key', 'value' ]]);
      AsyncStore.runInContext(customContext, () => {
        const value = AsyncStore.getContextValue('key');
        expect(value).toEqual('value');
      });
    });

    it('establishes parent chain for nested contexts', () => {
      AsyncStore.runInContext(null, () => {
        AsyncStore.setContextValue('outer', 'outer-value');

        AsyncStore.runInContext(null, () => {
          const store = AsyncStore.getContextStore();
          expect(store.parent).toBeDefined();
          expect(AsyncStore.getContextValue('outer')).toEqual('outer-value');
        });
      });
    });
  });

  describe('getContextStore', () => {
    it('returns undefined outside of context', () => {
      const store = AsyncStore.getContextStore();
      expect(store).toBeUndefined();
    });

    it('returns store object inside context', () => {
      AsyncStore.runInContext(null, () => {
        const store = AsyncStore.getContextStore();
        expect(store).toBeDefined();
        expect(store.context).toBeDefined();
      });
    });
  });

  describe('getContextValue', () => {
    it('returns default value outside of context', () => {
      const value = AsyncStore.getContextValue('nonexistent', 'default');
      expect(value).toEqual('default');
    });

    it('returns value from current context', () => {
      AsyncStore.runInContext(null, () => {
        AsyncStore.setContextValue('testKey', 'testValue');
        const value = AsyncStore.getContextValue('testKey');
        expect(value).toEqual('testValue');
      });
    });

    it('returns default value for missing key inside context', () => {
      AsyncStore.runInContext(null, () => {
        const value = AsyncStore.getContextValue('missing', 'fallback');
        expect(value).toEqual('fallback');
      });
    });

    it('traverses parent chain to find value', () => {
      AsyncStore.runInContext(null, () => {
        AsyncStore.setContextValue('parentKey', 'parentValue');

        AsyncStore.runInContext(null, () => {
          const value = AsyncStore.getContextValue('parentKey');
          expect(value).toEqual('parentValue');
        });
      });
    });

    it('returns closest value when key exists in multiple levels', () => {
      AsyncStore.runInContext(null, () => {
        AsyncStore.setContextValue('sharedKey', 'outer');

        AsyncStore.runInContext(null, () => {
          AsyncStore.setContextValue('sharedKey', 'inner');
          const value = AsyncStore.getContextValue('sharedKey');
          expect(value).toEqual('inner');
        });
      });
    });
  });

  describe('setContextValue', () => {
    it('does nothing outside of context', () => {
      AsyncStore.setContextValue('key', 'value');
      const value = AsyncStore.getContextValue('key');
      expect(value).toBeUndefined();
    });

    it('sets value in current context', () => {
      AsyncStore.runInContext(null, () => {
        AsyncStore.setContextValue('myKey', 'myValue');
        const value = AsyncStore.getContextValue('myKey');
        expect(value).toEqual('myValue');
      });
    });

    it('does not affect parent context', () => {
      AsyncStore.runInContext(null, () => {
        AsyncStore.runInContext(null, () => {
          AsyncStore.setContextValue('childKey', 'childValue');
        });

        const value = AsyncStore.getContextValue('childKey');
        expect(value).toBeUndefined();
      });
    });
  });

  describe('nested contexts', () => {
    it('provides context inheritance', () => {
      AsyncStore.runInContext(null, () => {
        AsyncStore.setContextValue('level1', 'one');

        AsyncStore.runInContext(null, () => {
          AsyncStore.setContextValue('level2', 'two');

          AsyncStore.runInContext(null, () => {
            AsyncStore.setContextValue('level3', 'three');

            expect(AsyncStore.getContextValue('level1')).toEqual('one');
            expect(AsyncStore.getContextValue('level2')).toEqual('two');
            expect(AsyncStore.getContextValue('level3')).toEqual('three');
          });
        });
      });
    });

    it('provides context isolation', () => {
      AsyncStore.runInContext(null, () => {
        AsyncStore.setContextValue('isolated', 'first');
      });

      AsyncStore.runInContext(null, () => {
        const value = AsyncStore.getContextValue('isolated');
        expect(value).toBeUndefined();
      });
    });

    it('supports shadowing parent values', () => {
      AsyncStore.runInContext(null, () => {
        AsyncStore.setContextValue('key', 'parent');

        AsyncStore.runInContext(null, () => {
          AsyncStore.setContextValue('key', 'child');
          expect(AsyncStore.getContextValue('key')).toEqual('child');
        });

        expect(AsyncStore.getContextValue('key')).toEqual('parent');
      });
    });
  });

  describe('async propagation', () => {
    it('propagates context through callbacks', async () => {
      await AsyncStore.runInContext(null, async () => {
        AsyncStore.setContextValue('asyncKey', 'asyncValue');

        await new Promise((resolve) => {
          setImmediate(() => {
            const value = AsyncStore.getContextValue('asyncKey');
            expect(value).toEqual('asyncValue');
            resolve();
          });
        });
      });
    });

    it('propagates context through Promises', async () => {
      await AsyncStore.runInContext(null, async () => {
        AsyncStore.setContextValue('promiseKey', 'promiseValue');

        await Promise.resolve().then(() => {
          const value = AsyncStore.getContextValue('promiseKey');
          expect(value).toEqual('promiseValue');
        });
      });
    });

    it('propagates context through async/await', async () => {
      await AsyncStore.runInContext(null, async () => {
        AsyncStore.setContextValue('awaitKey', 'awaitValue');

        const asyncFn = async () => {
          await Promise.resolve();
          return AsyncStore.getContextValue('awaitKey');
        };

        const value = await asyncFn();
        expect(value).toEqual('awaitValue');
      });
    });
  });
});
