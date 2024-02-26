# -*- coding: utf-8; combobulate-test-point-overlays: ((5 outline 306) (4 outline 394) (3 outline 419) (6 outline 434) (2 outline 463) (1 outline 506) (7 outline 579)); eval: (combobulate-test-fixture-mode t); -*-

class DeeplyNestedClass:
    class InnerClass:
        class InnerInnerClass:
            try:
                for i in range(10):
                    if i % 2 == 0:
                        print(i)
                    else:
                        print(i + 1)
            except:
                pass
            finally:
                pass
