// -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 185) (2 outline 205) (3 outline 235) (4 outline 251) (5 outline 299) (6 outline 341)); -*-
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
