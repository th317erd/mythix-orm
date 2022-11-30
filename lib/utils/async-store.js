///! import `var { Utils: { AsyncStore } } = require('mythix-orm');`
///!
///! AsyncStore utilities provide the only global
///! used in mythix-orm. The global used here is an
///! [AsyncLocalStorage](https://nodejs.org/docs/latest-v18.x/api/async_context.html#class-asynclocalstorage) instance used to track
///! connections (and transactions) through asynchronous
///! calls in the engine.
///!
///! The `node:async_hooks` module is imported inside a
///! `try/catch` block, so if your Javascript engine doesn't
///! support `AsyncLocalStorage`, this will fail, and silently
///! fallback to running the engine with no `AsyncLocalStorage`
///! support... which simply means that connection instances need
///! to be manually passed around everywhere.
///!
///! **!WARNING!: Never set the `'connection'` key, or a string key
///! that matches one of your model names to this `AsyncLocalStorage` context
///! unless you know exactly what you are doing. These keys are reserved
///! by Mythix ORM to pass connections and transactions through calls.** Any
///! and all other custom keys are available for use, though it would be
///! wise for you to prefix your key names, so as to avoid future name collisions
///! that might occur due to newer versions of Mythix ORM, or name collisions with
///! other 3rd party plugins or code that might set keys on the context as well.
///!
///! DocScope: AsyncStore

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

/// Fetch the AsyncLocalStorage store.
/// This calls `.getStore()` on the global
/// `AsyncLocalStorage` instance.
///
/// Return: any
///   The value from a `.getStore()` call on the global `AsyncLocalStorage` instance.
///   This will be `undefined` if no `AsyncLocalStorage` context is in scope.
function getContextStore() {
  return globalAsyncStore.getStore();
}

/// Get a specific value from the global `AsyncLocalStorage` context
/// by name.
///
/// Arguments:
///   key: any
///     The name of the property to return. The `AsyncLocalStorage`
///     context internally uses a `Map` instance, so the `key` provided
///     can be of any type.
///   defaultValue: any
///     The default value to return if the key specified is not found.
///
/// Return:
///   Return the property named by `key` if one is found, otherwise
///   return the `defaultValue` that was provided. If the global `AsyncLocalStorage` context is not
///   in scope when this is called, then the `defaultValue` will always be
///   returned.
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

/// Set a specific value on the global `AsyncLocalStorage` context
/// by name. A `Map` instance is used internally, so the `key` can
/// be of any type.
///
/// Note:
///   The global `AsyncLocalStorage` context must be in scope for this method to work.
///
/// Arguments:
///   key: any
///     The key you wish to set on the global `AsyncLocalStorage` context.
///   value: any
///     The value you wish to set on the global `AsyncLocalStorage` context.
///
/// Return: undefined
///   This method returns nothing.
function setContextValue(key, value) {
  let store = globalAsyncStore.getStore();
  if (!store || !store.context)
    return;

  store.context.set(key, value);
}

/// Run an asynchronous method in the global `AsyncLocalStorage` context.
///
/// Running a method this way will provide the method, and all calls within
/// its scope the global `AsyncLocalStorage` context.
///
/// Arguments:
///   context: Map | null
///     The context `Map` to use for the operation.
///   callback: Function
///     The asynchronous method to call and provide the context to.
///
/// Return: any
///   The return value from the callback.
function runInContext(context, callback) {
  return globalAsyncStore.run(
    {
      parent:  globalAsyncStore.getStore(),
      context: context || new Map(),
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
