# -*- combobulate-test-point-overlays: ((1 outline 247) (2 outline 257) (3 outline 267) (4 outline 274) (5 outline 284) (6 outline 294) (7 outline 301) (8 outline 343) (9 outline 355)); eval: (combobulate-test-fixture-mode t); -*-


def foo(
    arg1,
    /,
    arg2,
    arg3,
    arg4,
    *,
    a={
        1,
        (2, 3),
    },
    b=None,
    c=...,
):
    pass
