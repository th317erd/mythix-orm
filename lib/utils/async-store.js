'use strict';

let globalAsyncStore = global._mythixGlobalAsyncLocalStore;

if (!globalAsyncStore) {
  try {
    const { AsyncLocalStorage } = require('async_hooks');
    global._mythixGlobalAsyncLocalStore = globalAsyncStore = new AsyncLocalStorage();
  } catch (error) {
    global._mythixGlobalAsyncLocalStore = globalAsyncStore = {
      getStore: () => undefined,
      run:      (context, callback) => callback(),
    };
  }
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
  return globalAsyncStore.run(
    {
      parent: globalAsyncStore.getStore(),
      context,
    },
    callback,
  );
}

module.exports = {
  getContextValue,
  setContextValue,
  runInContext,
  getContextStore,
};
