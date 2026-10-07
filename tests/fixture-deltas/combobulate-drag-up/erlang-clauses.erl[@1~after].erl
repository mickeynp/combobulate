%% -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 186) (2 outline 202) (3 outline 218)); -*-
-module(demo).

pick(X) ->
    case X of
        a -> 1;
        b -> 2;
        _ -> 3
    end.
