%% -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 167) (2 outline 182) (3 outline 197)); -*-
-module(demo).

run(X) ->
    A = X + 1,
    log(B),
    B = A * 2.
