# -*- combobulate-test-point-overlays: ((1 outline 201) (2 outline 222) (3 outline 246) (4 outline 264) (5 outline 311) (6 outline 349)); eval: (combobulate-test-fixture-mode t); -*-


def foo():
    a = {1: 2, 3: 4}
    # some comment here
    b = (1, 2, 3)
    """
    This is a multiline string
    """
    return a, b
    some_func(1, 2, {"a": 1, "b": 2})
