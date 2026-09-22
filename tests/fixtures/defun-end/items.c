// -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 188) (2 outline 216) (3 outline 234) (4 outline 281) (5 outline 325)); -*-
#include <stdio.h>

struct Point {
    int x;
};

#define MAX 10

static int helper(int a) {
    return a + 1;
}

int main(void) {
    return helper(MAX);
}
