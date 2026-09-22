// -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 1) (2 outline 235) (3 outline 255) (4 outline 261) (5 outline 272) (6 outline 282) (7 outline 310) (8 outline 324)); -*-
#include <stdio.h>

void process(int n) {
    if (n > 0) {
        for (int i = 0; i < n; i++) {
            printf("%d\n", i);
        }
    }
}
