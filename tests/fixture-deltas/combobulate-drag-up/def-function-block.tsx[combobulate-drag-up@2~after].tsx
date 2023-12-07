// -*- combobulate-test-point-overlays: ((1 outline 275) (2 outline 299) (3 outline 308) (4 outline 317) (5 outline 326) (6 outline 418)); eval: (combobulate-test-fixture-mode t); -*-
function doStuff({a, b = 1, ...c}: {a: number, b?: number, c: number} = {a: 1, c: 2}) {
  a = 2;
  console.log(a, b, c);
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


