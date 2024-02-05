# -*- combobulate-test-point-overlays: ((1 outline 211) (2 outline 233) (3 outline 252) (4 outline 272) (5 outline 294) (6 outline 317) (7 outline 361)); eval: (combobulate-test-fixture-mode t); -*-

d = {
    'key1': 'value1',
    123: 'value2',
    True: 'value3',
    Foo.bar: 'value5',
    (4, 5): 'value4',
    {'inner_key': 'inner_value'}: 'value6',
    None: 'value7'
}
