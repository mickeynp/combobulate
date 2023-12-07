# -*- combobulate-test-point-overlays: ((1 outline 187) (2 outline 285) (3 outline 390)); eval: (combobulate-test-fixture-mode t); -*-


def foo():
    if x:
        for y in x:
            print(y)
    else:
        return x
    try:
        print(x)
    except:
        pass
    finally:
        print(x)
    async with some_async_manager() as y:
        print(y)

