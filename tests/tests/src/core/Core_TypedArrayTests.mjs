// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Test from "./Test.mjs";
import * as Stdlib_Option from "rescript/lib/es6/Stdlib_Option.js";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";

let eq = Primitive_object.equal;

Test.run([
  [
    "Core_TypedArrayTests.res",
    5,
    20,
    44
  ],
  "bytes per element is 8"
], BigInt64Array.BYTES_PER_ELEMENT, eq, 8);

let num1 = BigInt("123456789");

let num2 = BigInt("987654321");

let num3 = BigInt("555555555");

function assertTrue(message, predicate) {
  try {
    if (predicate()) {
      return;
    }
    throw new Error(message);
  } catch (exn) {
    throw new Error(message);
  }
}

function assertWillThrow(message, f) {
  let didThrow = false;
  try {
    f();
  } catch (exn) {
    didThrow = true;
  }
  if (didThrow !== false) {
    return;
  }
  throw new Error(message);
}

function areSame(x, y) {
  return x.toString() === y.toString();
}

assertTrue("fromArray", () => areSame(Stdlib_Option.getExn(new BigInt64Array([
    num1,
    num2
  ])[1], undefined), num2));

assertTrue("fromBuffer", () => {
  let x = new BigInt64Array(new ArrayBuffer(16));
  x[1] = num2;
  return areSame(Stdlib_Option.getExn(x[1], undefined), num2);
});

assertWillThrow("fromBuffer when too short can throw when used", () => {
  let x = new BigInt64Array(new ArrayBuffer(1));
  x[0] = num1;
  areSame(Stdlib_Option.getExn(x[0], undefined), num1);
});

assertTrue("fromBufferWithRange", () => {
  let x = new BigInt64Array(new ArrayBuffer(16), 0, 1);
  x[0] = num1;
  return areSame(Stdlib_Option.getExn(x[0], undefined), num1);
});

assertWillThrow("fromBufferWithRange is unsafe, out of range", () => {
  let x = new BigInt64Array(new ArrayBuffer(16), 13, 1);
  x[0] = num1;
  areSame(Stdlib_Option.getExn(x[0], undefined), num1);
});

assertTrue("fromLength is NOT in bytes", () => {
  let x = new BigInt64Array(1);
  return x.byteLength === 8;
});

function o(prim0, prim1) {
  return BigInt64Array.from(prim0, prim1);
}

export {
  eq,
  num1,
  num2,
  num3,
  assertTrue,
  assertWillThrow,
  areSame,
  o,
}
/*  Not a pure module */
