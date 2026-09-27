%% -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 169) (2 outline 184) (3 outline 202) (4 outline 207)); -*-
-module(demo).

pick(X) ->
    case X of
        a -> 1;
        b -> 2
    end.
