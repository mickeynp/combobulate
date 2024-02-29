# -*- coding: utf-8; combobulate-test-point-overlays: ((1 outline 140)); eval: (combobulate-test-fixture-mode t); -*-
class Test:

    test: str = "test"
    number: int = 1

    def __init__(self):
        self.a = 1
        self.b = 2
        self.c = 3

    def __iter__(self):
        return iter([self.a, self.b, self.c])

    def __next__(self):
        return next(self.__iter__())
