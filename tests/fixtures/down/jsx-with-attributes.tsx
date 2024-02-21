// -*- combobulate-test-point-overlays: ((1 outline 220) (2 outline 232) (3 outline 253) (4 outline 254) (5 outline 258) (6 outline 266)); eval: (combobulate-test-fixture-mode t); -*-
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
