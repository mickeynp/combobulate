// -*- eval: (combobulate-test-fixture-mode t); combobulate-test-point-overlays: ((1 outline 233) (2 outline 250) (3 outline 271) (4 outline 300) (5 outline 341) (6 outline 378) (7 outline 410) (8 outline 463)); -*-
#ifndef POINT_H
#include <stddef.h>

#define POINT_H

typedef struct Point Point;

struct Point {
    int x;
    int y;
};

enum Colour {
    RED,
    GREEN
};

extern const char *point_name;

int point_distance(const Point *a, const Point *b);

void point_free(Point *p);

#endif
