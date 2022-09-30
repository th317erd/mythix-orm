'use strict';

let globalAsyncStore;

try {
  const { AsyncLocalStorage } = require('async_hooks');
  globalAsyncStore = new AsyncLocalStorage();
} catch (error) {
  globalAsyncStore = {
    getStore: () => undefined,
    run:      (context, callback) => callback(),
  };
}

function getContextStore() {
  return globalAsyncStore.getStore();
}

function getContextValue(key, defaultValue) {
  let store = globalAsyncStore.getStore();
  while (store) {
    let { context } = store;
    if (!context.has(key)) {
      store = store.parent;
      continue;
    }

    return context.get(key);
  }

  return defaultValue;
}

function setContextValue(key, value) {
  let store = globalAsyncStore.getStore();
  if (!store || !store.context)
    return;

  return store.context.set(key, value);
}

function runInContext(context, callback) {
  return new Promise((resolve, reject) => {
    globalAsyncStore.run({ parent: globalAsyncStore.getStore(), context }, async () => {
      try {
        let result = await callback();
        resolve(result);
      } catch (error) {
        reject(error);
      }
    });
  });
}

module.exports = {
  getContextValue,
  setContextValue,
  runInContext,
  getContextStore,
};
