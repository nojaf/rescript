// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_undefined from "rescript/lib/es6/Js_undefined.js";

function Make(S) {
  let opt_get = (f, i) => Js_undefined.toOption(f[i]);
  return {
    opt_get: opt_get
  };
}

function opt_get(f, i) {
  return Js_undefined.toOption(f[i]);
}

let Int_arr = {
  opt_get: opt_get
};

function f(v) {
  return [
    v[0],
    Js_undefined.toOption(v[1])
  ];
}

export {
  Make,
  Int_arr,
  f,
}
/* No side effect */
