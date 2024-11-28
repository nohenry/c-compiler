// #include <stdio.h>
// #include <Availability.h>
// #include "inc.cp"
// #include <sys/cdefs.h>
// #include "sys/_types/_useconds_t.h"

// #define align(x) __attribute__((aligned(x)))

#define F(x) x
#define A(f, x) f (x)
#define B(x) x ## AR
#define BAR 4

// int fopen() __DARWIN_ALIAS_STARTING(__MAC_10_6, __IPHONE_2_0, __DARWIN_ALIAS(fopen));

// #include <stdint.h>
// #include <stdbool.h>
// #include "stdlib.h"

int main() {
// __DARWIN_ALIAS(fopen);
    // char *c = A(F((y + 2)));

    char *c = B(F(1));
}

// int a();
// int a, align(2) b align(4) = 1.2f;
