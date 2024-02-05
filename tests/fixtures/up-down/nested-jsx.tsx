// -*- combobulate-test-point-overlays: ((8 outline 1) (7 outline 215) (6 outline 230) (5 outline 234) (4 outline 241) (3 outline 259) (2 outline 308) (1 outline 313)); eval: (combobulate-test-fixture-mode t); -*-
function foo() {
  return <span>
           <div>
             something before
             <div>bar</div>
             something after
           </div>
         </span>;
}
export default Foo;
