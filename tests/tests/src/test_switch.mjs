// Generated by ReScript, PLEASE EDIT WITH CARE


function f(x) {
  if (typeof x !== "object") {
    if (x === "G") {
      return 4;
    } else {
      return 5;
    }
  }
  switch (x.TAG) {
    case "A" :
      return 0;
    case "B" :
      return 1;
    case "C" :
      return 2;
    case "F" :
      return 3;
  }
}

function bind(x, f) {
  if (x.TAG === "Left") {
    return {
      TAG: "Left",
      _0: f(x._0)
    };
  } else {
    return x;
  }
}

export {
  f,
  bind,
}
/* No side effect */