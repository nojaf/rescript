// Generated by ReScript, PLEASE EDIT WITH CARE

import * as React from "react";
import * as ReactDom from "react-dom";

function fib(x) {
  if (x !== 2 && x !== 1) {
    return fib(x - 1 | 0) + fib(x - 2 | 0) | 0;
  } else {
    return 1;
  }
}

function sum(n) {
  let v = 0;
  for (let i = 0; i <= n; ++i) {
    v = v + i | 0;
  }
  return v;
}

function map(f, x) {
  if (typeof x !== "object") {
    return "Nil";
  } else {
    return {
      TAG: "Cons",
      _0: f(x._0),
      _1: map(f, x._1)
    };
  }
}

function test_curry(x, y) {
  return x + y | 0;
}

function f(extra) {
  return 32 + extra | 0;
}

ReactDom.render(React.createClass({
  render: () => React.DOM.div({
    alt: "pic"
  }, React.DOM.h1(undefined, "hello react"), React.DOM.h2(undefined, "type safe!"))
}), document.getElementById("hi"));

export {
  fib,
  sum,
  map,
  test_curry,
  f,
}
/*  Not a pure module */
