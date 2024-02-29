# -*- combobulate-test-point-overlays: ((1 outline 121)); eval: (combobulate-test-fixture-mode t); -*-


def foo():
    a = {1: 2, 3: 4}
    # some comment here
    b = (1, 2, 3)
    """
    This is a multiline string
    """
    some_func(1, 2, {"a": 1, "b": 2})
    return a, b
