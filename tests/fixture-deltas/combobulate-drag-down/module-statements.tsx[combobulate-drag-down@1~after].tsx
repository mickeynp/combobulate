// -*- combobulate-test-point-overlays: ((1 outline 169) (2 outline 209) (3 outline 241) (4 outline 263) (5 outline 318)); eval: (combobulate-test-fixture-mode t); -*-
function foo() {
  return 1;
}
// comment

import { foo } from 'chai';

const bar = () => 2;

class Foo {
  constructor() {
    this.foo = 1;
  }
}

export default Foo;
