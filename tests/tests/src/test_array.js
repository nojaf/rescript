// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Belt_Array = require("rescript/lib/js/Belt_Array.js");

let v = Belt_Array.make(6, 5);

let h = Belt_Array.slice(v, 0, 2);

let hhh = Belt_Array.concat([
  1,
  2,
  3,
  4
], [
  1,
  2,
  3,
  5
]);

let u = Belt_Array.concatMany([
  [
    1,
    2
  ],
  [
    2,
    3
  ],
  [
    3,
    4
  ]
]);

let hh = Belt_Array.blit;

exports.v = v;
exports.h = h;
exports.hh = hh;
exports.hhh = hhh;
exports.u = u;
/* v Not a pure module */