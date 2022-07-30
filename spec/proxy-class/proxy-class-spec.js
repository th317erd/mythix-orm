/* eslint-disable max-classes-per-file */
/* eslint-disable new-cap */
/* eslint-disable no-magic-numbers */

'use strict';

/* global describe, it, expect */

const ProxyClass = require('../../lib/proxy-class');

describe('ProxyClass', () => {
  it('can overload getter', () => {
    let props = [];
    let derpCalled = false;
    let helloCalled = false;

    class ProxyTest extends ProxyClass {
      [ProxyClass.GET](target, prop) {
        props.push(prop);
        return super[ProxyClass.GET].apply(this, arguments);
      }

      derp() {
        derpCalled = true;
      }

      hello() {
        helloCalled = true;
      }
    }

    let test = new ProxyTest();
    test.derp();
    test.hello();

    expect(props).toEqual([ 'derp', 'hello' ]);
    expect(derpCalled).toBe(true);
    expect(helloCalled).toBe(true);
  });

  it('can overload setter', () => {
    let props = [];

    class ProxyTest extends ProxyClass {
      [ProxyClass.SET](target, prop, value, receiver) {
        props.push(prop);
        return super[ProxyClass.SET].apply(this, [ target, prop, `${value}_modified`, receiver ]);
      }
    }

    let test = new ProxyTest();
    test.derp = 'stuff';
    test.hello = 'world';

    expect(props).toEqual([ 'derp', 'hello' ]);
    expect(test.derp).toEqual('stuff_modified');
    expect(test.hello).toEqual('world_modified');
  });

  it('can be callable', () => {
    let givenArgs;

    class ProxyTest extends ProxyClass {
      [ProxyClass.CALLABLE](...args) {
        givenArgs = args;
      }
    }

    let test = new ProxyTest();
    test('hello', 'world');

    expect(givenArgs).toEqual([ 'hello', 'world' ]);
  });

  it('can modify it to be callable', () => {
    let givenArgs;

    class ProxyTest extends ProxyClass {
    }

    let test = new ProxyTest();
    expect(() => test('hello', 'world')).toThrow(new TypeError('test is not a function'));

    test = test.__call((...args) => {
      givenArgs = args;
    });

    test('hello', 'world');
    expect(givenArgs).toEqual([ 'hello', 'world' ]);
  });

  it('can setup an autocaller', () => {
    let autoCallArgs;
    let givenArgs;

    class ProxyTestResult extends ProxyClass {
      hello() {
        return 'world';
      }
    }

    class ProxyTest extends ProxyClass {
      getHello() {
        return new ProxyTestResult().__call((...args) => {
          givenArgs = args;
        }).__autoCall((...args) => {
          autoCallArgs = args;
        });
      }
    }

    // Should not call autocaller
    let test = new ProxyTest();
    test.getHello()('hello', 'world');

    expect(givenArgs).toEqual([ 'hello', 'world' ]);
    expect(autoCallArgs).toBe(undefined);

    // Should call autocaller
    givenArgs = undefined;
    autoCallArgs = undefined;

    test = new ProxyTest();

    // .stuff should trigger autocall
    let magic = test.getHello();
    magic.stuff;

    expect(givenArgs).toBe(undefined);
    expect(autoCallArgs).toEqual([]);

    // Should not call autocaller a second time
    givenArgs = undefined;
    autoCallArgs = undefined;
    magic.derp;

    expect(givenArgs).toBe(undefined);
    expect(autoCallArgs).toBe(undefined);
  });

  it('can specify a method as autocall', () => {
    let givenArgs;
    let callCount = 0;

    class ProxyTest extends ProxyClass {
      NOT = ProxyClass.autoCall(function(...args) {
        callCount++;
        givenArgs = args;
      });
    }

    // Should not be called just by fetching the method name
    let test = new ProxyTest();
    test.NOT;

    expect(callCount).toEqual(0);

    // Should be called once when called directly
    callCount = 0;

    test.NOT('hello', 'world');
    expect(callCount).toEqual(1);
    expect(givenArgs).toEqual([ 'hello', 'world' ]);

    // Should be called once when called indirectly
    callCount = 0;
    givenArgs = undefined;

    test.NOT.derp;
    expect(callCount).toEqual(1);
    expect(givenArgs).toEqual([]);
  });
});
