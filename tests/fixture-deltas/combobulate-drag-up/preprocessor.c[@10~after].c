// -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 284) (2 outline 303) (3 outline 324) (4 outline 345) (5 outline 376) (6 outline 408) (7 outline 457) (8 outline 494) (9 outline 527) (10 outline 569) (11 outline 596) (12 outline 621)); -*-
#include <stdio.h>
#include <stdlib.h>

#define MAX_SIZE 256
#define SQUARE(x) ((x) * (x))

typedef unsigned long ulong_t;

typedef struct {
    int x;
    int y;
} Point;

typedef enum { RED, GREEN } Colour;

struct Node {
    int value;
};

enum Status { OK, FAIL };

union Value {
    int i;
    float f;
};

static int counter = 0;

static void helper(void) {
    return;
}
