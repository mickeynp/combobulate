// -*- combobulate-test-point-overlays: ((1 outline 199) (2 outline 310)); eval: (combobulate-test-fixture-mode t); -*-

function Foo({ a, b }: { a: number, b: number }) {
  return <div>
           <div>
             <h1>This is a heading</h1>
             The number 'a' is: {a}
           </div>
           <div>{b}</div>
         </div>
}

