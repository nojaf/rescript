// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Mt from "./mt.mjs";

function tst() {
  for (let i = (console.log("hi"), 0), i_finish = (console.log("hello"), 3); i <= i_finish; ++i) {
    
  }
}

function test2() {
  let v = 0;
  v = 3;
  v = 10;
  for (let i = 0; i <= 1; ++i) {
    
  }
  return v;
}

let suites_0 = [
  "for_order",
  param => ({
    TAG: "Eq",
    _0: 10,
    _1: test2()
  })
];

let suites = {
  hd: suites_0,
  tl: /* [] */0
};

Mt.from_pair_suites("For_side_effect_test", suites);

export {
  tst,
  test2,
  suites,
}
/*  Not a pure module */
