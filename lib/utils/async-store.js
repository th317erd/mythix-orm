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
///! **!WARNING!: Never set the <see>CONNECTION_KEY</see> Symbol, or a string key
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

/// The Symbol key used to store the connection in AsyncLocalStorage context.
/// This is exported so users can access the context directly if needed.
///
/// Example:
///   const { CONNECTION_KEY } = require('mythix-orm').Utils.AsyncStore;
///   const connection = AsyncStore.getContextValue(CONNECTION_KEY);
const CONNECTION_KEY = Symbol.for('mythix-orm:connection');

let globalAsyncStore = global._mythixGlobalAsyncLocalStore;
let AsyncLocalStorageClass = null;
let debugEnabled = process.env.MYTHIX_ORM_DEBUG === '1' || process.env.MYTHIX_ORM_DEBUG === 'true';

if (!globalAsyncStore) {
  try {
    const asyncHooks = require('async_hooks');
    AsyncLocalStorageClass = asyncHooks.AsyncLocalStorage;
    global._mythixGlobalAsyncLocalStore = globalAsyncStore = new AsyncLocalStorageClass();
  } catch (error) {
    global._mythixGlobalAsyncLocalStore = globalAsyncStore = {
      getStore: () => undefined,
      run:      (context, callback) => callback(),
    };
  }
}

/// Enable or disable debug mode for AsyncStore operations.
/// When enabled, context operations will be logged to console.
///
/// Debug mode can also be enabled via the `MYTHIX_ORM_DEBUG=1` environment variable.
///
/// Arguments:
///   enabled: boolean
///     Whether to enable debug logging.
///
/// Return: undefined
function setDebug(enabled) {
  debugEnabled = !!enabled;
}

/// Check if debug mode is currently enabled.
///
/// Return: boolean
///   True if debug mode is enabled.
function isDebugEnabled() {
  return debugEnabled;
}

function debugLog(...args) {
  if (debugEnabled)
    console.log('[mythix-orm:AsyncStore]', ...args);
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

/// Check if code is currently running inside an AsyncLocalStorage context.
///
/// Return: boolean
///   True if a context exists, false otherwise.
function hasContext() {
  return globalAsyncStore.getStore() !== undefined;
}

/// Check if a connection is available in the current AsyncLocalStorage context.
///
/// Return: boolean
///   True if a connection is available in context, false otherwise.
function hasConnection() {
  return !!getContextValue(CONNECTION_KEY);
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
  debugLog('Entering context with keys:', context ? [...context.keys()] : []);

  return globalAsyncStore.run(
    {
      parent:  globalAsyncStore.getStore(),
      context: context || new Map(),
    },
    callback,
  );
}

/// Capture the current AsyncLocalStorage context and return a function
/// that will execute callbacks within that captured context.
///
/// This is useful for preserving context across event emitters, setTimeout,
/// or other callbacks where context might otherwise be lost.
///
/// Note:
///   This uses AsyncLocalStorage.snapshot() when available (Node.js 20+),
///   with a fallback implementation for older versions.
///
/// Return: Function
///   A function that takes a callback and executes it in the captured context.
///   If no context exists when captured, callbacks will run without context.
///
/// Example:
///   const runInCapturedContext = captureContext();
///
///   eventEmitter.on('someEvent', () => {
///     runInCapturedContext(() => {
///       // Models work here - context is preserved
///       const user = await User.where.id.EQ('123').first();
///     });
///   });
function captureContext() {
  // Use native snapshot() if available (Node.js 20+)
  if (AsyncLocalStorageClass && typeof AsyncLocalStorageClass.snapshot === 'function') {
    debugLog('Capturing context via snapshot()');
    return AsyncLocalStorageClass.snapshot();
  }

  // Fallback: manually capture current store
  const capturedStore = globalAsyncStore.getStore();
  debugLog('Capturing context manually, has context:', !!capturedStore);

  return function runInCapturedContext(callback) {
    if (!capturedStore)
      return callback();

    return globalAsyncStore.run(capturedStore, callback);
  };
}

/// Wrap a callback function to preserve the current AsyncLocalStorage context.
///
/// This is a convenience wrapper around <see>captureContext</see> for use with
/// event handlers and other callbacks.
///
/// Arguments:
///   callback: Function
///     The callback function to wrap.
///
/// Return: Function
///   A wrapped version of the callback that will execute in the captured context.
///
/// Example:
///   eventEmitter.on('data', bindCallback(async (data) => {
///     // Context preserved - models work here
///     await processData(data);
///   }));
function bindCallback(callback) {
  const runInCapturedContext = captureContext();

  return function boundCallback(...args) {
    return runInCapturedContext(() => callback.apply(this, args));
  };
}

/// Get debugging information about the current context state.
/// Useful for troubleshooting context-related issues.
///
/// Return: object
///   An object containing:
///   - hasContext: boolean - whether a context exists
///   - hasConnection: boolean - whether a connection is in context
///   - contextKeys: Array - keys present in the current context
///   - parentDepth: number - how many parent contexts exist
function getContextDebugInfo() {
  let store = globalAsyncStore.getStore();
  let parentDepth = 0;
  let contextKeys = [];

  if (store && store.context)
    contextKeys = [...store.context.keys()];

  let tempStore = store;
  while (tempStore && tempStore.parent) {
    parentDepth++;
    tempStore = tempStore.parent;
  }

  return {
    hasContext:    !!store,
    hasConnection: hasConnection(),
    contextKeys,
    parentDepth,
  };
}

module.exports = {
  CONNECTION_KEY,
  getContextValue,
  setContextValue,
  runInContext,
  getContextStore,
  hasContext,
  hasConnection,
  captureContext,
  bindCallback,
  setDebug,
  isDebugEnabled,
  getContextDebugInfo,
};
