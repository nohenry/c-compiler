// #include <stdio.h>
// #include <Availability.h>
// #include "inc.cp"
// #include <sys/cdefs.h>
// #include "sys/_types/_useconds_t.h"
// #include <assert.h>

// #define align(x) __attribute__((aligned(x)))

// #define BAR 4

// int fopen() __DARWIN_ALIAS_STARTING(__MAC_10_6, __IPHONE_2_0, __DARWIN_ALIAS(fopen));

// #include <stdint.h>
// #include <stdbool.h>
// #include "stdlib.h"

// #define BAR(e, a, b) (e, a, b)
// #define FOO(e) e ? BAR(#e, 1, 2) : false
// #define F(f, x, y) (f, x, y)

typedef struct node {
    struct node n;
} Node;

int main(int a, int) {
    char *c = "foobar";
    // int i[2] = {1, 2};
    return 1 + 2u + 3uL + 4uLL + 5.0;
// __DARWIN_ALIAS(fopen);
    // char *c = A(F((y + 2)));

//   assert(f > e);
    // FOO(f > e);

    // int i = (int) {.bar = 3};
    // char *c = B(F(1));
}

// int a();
// int a, align(2) b align(4) = 1.2f;
