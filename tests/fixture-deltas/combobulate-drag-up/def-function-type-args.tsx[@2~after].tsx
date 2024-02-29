// -*- combobulate-test-point-overlays: ((1 outline 173) (2 outline 184) (3 outline 196)); eval: (combobulate-test-fixture-mode t); -*-
function doStuff({a, b = 1, ...c}: {b?: number, a: number, c: number} = {a: 1, c: 2}) {
  console.log(a, b, c);
  a = 2;
  b = 3;
  c = 4;
  {
    let a = 5;
    let b = 6;
    let c = 7;
    console.log(a, b, c);
  }
  // comment
  return {a, b, c};


