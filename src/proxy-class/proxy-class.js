/* eslint-disable no-unused-vars */
/* eslint-disable max-classes-per-file */
/* eslint-disable lines-between-class-members */

'use strict';

const APPLY                       = Symbol.for('@_apply');
const CALLABLE                    = Symbol.for('@_callable');
const CONSTRUCT                   = Symbol.for('@_construct');
const DEFINE_PROPERTY             = Symbol.for('@_defineProperty');
const DELETE_PROPERTY             = Symbol.for('@_deleteProperty');
const GET                         = Symbol.for('@_get');
const GET_OWN_PROPERTY_DESCRIPTOR = Symbol.for('@_getOwnPropertyDescriptor');
const GET_PROTOTYPEOF             = Symbol.for('@_getPrototypeOf');
const HAS                         = Symbol.for('@_has');
const IS_EXTENSIBLE               = Symbol.for('@_isExtensible');
const MISSING                     = Symbol.for('@_missing');
const OWN_KEYS                    = Symbol.for('@_ownKeys');
const PREVENT_EXTENSIONS          = Symbol.for('@_preventExtensions');
const SET                         = Symbol.for('@_set');
const SET_PROTOTYPEOF             = Symbol.for('@_setPrototypeOf');
const PROXY                       = Symbol.for('@__proxy');
const TARGET                      = Symbol.for('@__target');
const SELF                        = Symbol.for('@__rootInstance');
const AUTO_CALL_CALLER            = Symbol.for('@__autoCallCaller');
const AUTO_CALL_CALLED            = Symbol.for('@__autoCallCalled');
const AUTO_CALL                   = Symbol.for('@__autoCall');

function shouldSkipProxy(prop) {
  if (prop === PROXY || prop === TARGET || prop === SELF || prop === AUTO_CALL_CALLER || prop === AUTO_CALL_CALLED || prop === AUTO_CALL)
    return true;

  if (prop in Object.prototype)
    return true;

  return false;
}

class ProxyClass {
  static APPLY = APPLY;
  static CALLABLE = CALLABLE;
  static CONSTRUCT = CONSTRUCT;
  static DEFINE_PROPERTY = DEFINE_PROPERTY;
  static DELETE_PROPERTY = DELETE_PROPERTY;
  static GET = GET;
  static GET_OWN_PROPERTY_DESCRIPTOR = GET_OWN_PROPERTY_DESCRIPTOR;
  static GET_PROTOTYPEOF = GET_PROTOTYPEOF;
  static HAS = HAS;
  static IS_EXTENSIBLE = IS_EXTENSIBLE;
  static MISSING = MISSING;
  static OWN_KEYS = OWN_KEYS;
  static PREVENT_EXTENSIONS = PREVENT_EXTENSIONS;
  static SET = SET;
  static SET_PROTOTYPEOF = SET_PROTOTYPEOF;
  static PROXY = PROXY;
  static TARGET = TARGET;
  static SELF = SELF;
  static AUTO_CALL_CALLER = AUTO_CALL_CALLER;
  static AUTO_CALL_CALLED = AUTO_CALL_CALLED;
  static AUTO_CALL = AUTO_CALL;

  static shouldSkipProxy = shouldSkipProxy;

  static autoCall(func) {
    Object.defineProperties(func, {
      [AUTO_CALL]: {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        true,
      },
    });

    return func;
  }

  static createProxy(_proxyTarget, forceSpecifiedContext) {
    let me = (forceSpecifiedContext) ? this : (this[SELF] || this);
    let proxyTarget = _proxyTarget || me[CALLABLE] || me;

    let proxy = new Proxy(proxyTarget, {
      apply: function(target, thisArg, argumentsList) {
        return me[APPLY].call(me, target, thisArg, argumentsList);
      },
      construct: function(target, argumentsList, newTarget) {
        return me[CONSTRUCT].call(me, target, argumentsList, newTarget);
      },
      defineProperty: function(target, key, descriptor) {
        return me[DEFINE_PROPERTY].call(me, target, key, descriptor);
      },
      deleteProperty: function(target, prop) {
        return me[DELETE_PROPERTY].call(me, target, prop);
      },
      get: function(target, prop, receiver) {
        if (shouldSkipProxy(prop))
          return me[prop];

        // console.log('PROXY KEY: ', prop, me);

        if (typeof prop !== 'symbol' && prop !== '__autoCall' && prop !== '__call') {
          let autoCall = me[AUTO_CALL_CALLER];
          if (typeof autoCall === 'function' && me[AUTO_CALL_CALLED] === false) {
            me[AUTO_CALL_CALLED] = true;
            autoCall.call(me);
          }
        }

        if (prop in me) {
          let value = me[GET].call(me, target, prop, receiver);

          if (typeof value === 'function' && value[AUTO_CALL] === true)
            return me.constructor.createProxy.call(Object.create(me), value, true).__autoCall(value);

          return value;
        }

        return me[MISSING].call(me, target, prop, receiver);
      },
      getOwnPropertyDescriptor: function(target, prop) {
        return me[GET_OWN_PROPERTY_DESCRIPTOR].call(me, target, prop);
      },
      getPrototypeOf: function(target) {
        return me[GET_PROTOTYPEOF].call(me, target);
      },
      has: function(target, prop) {
        return me[HAS].call(me, target, prop);
      },
      isExtensible: function(target) {
        return me[IS_EXTENSIBLE].call(me, target);
      },
      ownKeys: function(target) {
        return me[OWN_KEYS].call(me, target);
      },
      preventExtensions: function(target) {
        return me[PREVENT_EXTENSIONS].call(me, target);
      },
      set: function(target, prop, value, receiver) {
        if (typeof prop !== 'symbol' && prop !== '__autoCall' && prop !== '__call') {
          let autoCall = me[AUTO_CALL_CALLER];
          if (typeof autoCall === 'function' && me[AUTO_CALL_CALLED] === false) {
            me[AUTO_CALL_CALLED] = true;
            autoCall.call(me);
          }
        }

        return me[SET].call(me, target, prop, value, receiver);
      },
      setPrototypeOf: function(target, prototype) {
        return me[SET_PROTOTYPEOF].call(me, target, prototype);
      },
    });

    Object.defineProperties(me, {
      [PROXY]: {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        proxy,
      },
      [TARGET]: {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        proxyTarget,
      },
      [SELF]: {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        me,
      },
    });

    return proxy;
  }

  constructor() {
    Object.defineProperties(this, {
      [AUTO_CALL_CALLER]: {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        null,
      },
      [AUTO_CALL_CALLED]: {
        writable:     true,
        enumberable:  false,
        configurable: true,
        value:        false,
      },
    });

    let proxy = ProxyClass.createProxy.call(this);
    return proxy;
  }

  __autoCall(caller) {
    this[AUTO_CALL_CALLER] = caller;
    this[AUTO_CALL_CALLED] = false;

    return this;
  }

  __call(caller) {
    return ProxyClass.createProxy.call(this, caller.bind(this[PROXY]));
  }

  [APPLY](target, thisArg, argumentsList) {
    return target.apply(thisArg, argumentsList);
  }

  [CONSTRUCT](target, argumentsList, newTarget) {
    let self = Object.create(target.prototype);
    let result = newTarget.apply(self, argumentsList);

    if (!result || typeof result !== 'object')
      return self;

    return result;
  }

  [DEFINE_PROPERTY](target, key, descriptor) {
    return Object.defineProperty(this, key, descriptor);
  }

  [DELETE_PROPERTY](target, prop) {
    return delete this[prop];
  }

  [GET](target, prop) {
    return this[prop];
  }

  [GET_OWN_PROPERTY_DESCRIPTOR](target, prop) {
    return Object.getOwnPropertyDescriptor(target, prop);
  }

  [GET_PROTOTYPEOF](target) {
    return Object.getPrototypeOf(this);
  }

  [HAS](target, prop) {
    return (prop in this);
  }

  [IS_EXTENSIBLE](target) {
    return Object.isExtensible(this);
  }

  [MISSING]() {
  }

  [OWN_KEYS](target) {
    return Reflect.ownKeys(this);
  }

  [PREVENT_EXTENSIONS](target) {
    return Object.preventExtensions(this);
  }

  [SET](target, prop, value) {
    this[prop] = value;
    return true;
  }

  [SET_PROTOTYPEOF](target, prototype) {
    return Object.setPrototypeOf(this, prototype);
  }
}

module.exports = ProxyClass;
