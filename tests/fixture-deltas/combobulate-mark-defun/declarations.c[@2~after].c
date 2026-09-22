// -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 212) (2 outline 246) (3 outline 277) (4 outline 351) (5 outline 389)); -*-
#include <stdio.h>

struct Point {
    int x;
    int y;
};

;

typedef unsigned long ulong_t;

static int counter = 0;

static int helper(int a) {
    return a + 1;
}

int main(void) {
    return helper(counter);
}
