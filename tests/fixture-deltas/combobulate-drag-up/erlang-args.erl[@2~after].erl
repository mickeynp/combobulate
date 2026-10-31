%% -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 191) (2 outline 195) (3 outline 201)); -*-
-module(demo).

run(Id, Opts, Timeout) ->
    request(Opts, Id, Timeout).
