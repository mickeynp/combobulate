// -*- combobulate-test-point-overlays: ((1 outline 250) (2 outline 256) (3 outline 262) (4 outline 268) (5 outline 272) (6 outline 279) (7 outline 285) (8 outline 297) (9 outline 318) (10 outline 319)); eval: (combobulate-test-fixture-mode t); -*-
const Foo = () => {
  return (
    <foo>
      <div id={1}>
        {1 ? <br/> : null}
        <h1>hello</h1>
      </div>
    </foo>
  )
}
