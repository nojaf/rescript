// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Test_common from "./test_common.mjs";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";

let Local = /* @__PURE__ */Primitive_exceptions.create("Test_exception.Local");

function f() {
  throw {
    RE_EXN_ID: Local,
    _1: 3,
    Error: new Error()
  };
}

function g() {
  throw {
    RE_EXN_ID: "Not_found",
    Error: new Error()
  };
}

function h() {
  throw {
    RE_EXN_ID: Test_common.U,
    _1: 3,
    Error: new Error()
  };
}

function x() {
  throw {
    RE_EXN_ID: Test_common.H,
    Error: new Error()
  };
}

function xx() {
  throw {
    RE_EXN_ID: "Invalid_argument",
    _1: "x",
    Error: new Error()
  };
}

let Nullary = /* @__PURE__ */Primitive_exceptions.create("Test_exception.Nullary");

let a = {
  RE_EXN_ID: Nullary
};

export {
  Local,
  f,
  g,
  h,
  x,
  xx,
  Nullary,
  a,
}
/* No side effect */