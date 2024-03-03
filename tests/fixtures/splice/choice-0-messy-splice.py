# -*- coding: utf-8; eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 155)); -*-
result = (lambda x: x if x < 0 else x * 2)(sum([i for i in range(10) if i % 2 == 0]))

