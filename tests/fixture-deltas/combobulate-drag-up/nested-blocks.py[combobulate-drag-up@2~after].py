# -*- combobulate-test-point-overlays: ((1 outline 187) (2 outline 285) (3 outline 390)); eval: (combobulate-test-fixture-mode t); -*-


def foo():
    with some_manager() as x:
        try:
            print(x)
        except:
            pass
        finally:
            print(x)
        if x:
            for y in x:
                print(y)
        else:
            return x
        async with some_async_manager() as y:
            print(y)
