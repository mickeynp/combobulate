// -*- combobulate-test-point-overlays: ((1 outline 171)); eval: (combobulate-test-fixture-mode t); -*-

function Foo({ a, b }: { a: number, b: number }) {
  return <div>{null
    ? <div>Some jsx element</div>
    : <mytag/>
  }</div>
}

