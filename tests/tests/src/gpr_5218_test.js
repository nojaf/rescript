// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

let test_id = {
  contents: 0
};

let suites = {
  contents: /* [] */0
};

function test(x) {
  if (x.NAME === 2) {
    return {
      NAME: 2,
      VAL: x.VAL
    };
  } else {
    return {
      NAME: 1,
      VAL: x.VAL
    };
  }
}

Mt.eq_suites(test_id, suites, "File \"gpr_5218_test.res\", line 11, characters 29-36", test({
  NAME: 1,
  VAL: 3
}), {
  NAME: 1,
  VAL: 3
});

Mt.eq_suites(test_id, suites, "File \"gpr_5218_test.res\", line 13, characters 29-36", test({
  NAME: 2,
  VAL: 3
}), {
  NAME: 2,
  VAL: 3
});

Mt.from_pair_suites("gpr_5218_test.res", suites.contents);

let eq_suites = Mt.eq_suites;

exports.eq_suites = eq_suites;
exports.test_id = test_id;
exports.suites = suites;
exports.test = test;
/*  Not a pure module */