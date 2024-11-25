// #include <stdio.h>
// #include "inc.cp"
// #include <sys/cdefs.h>
// #include "sys/_types/_useconds_t.h"

// #define align(x) __attribute__((aligned(x)))

#define F(x) # x
#define A(x) x

int main() {
    char *c = A(F((y + 2)));
    // char *c = F(y);
    // char *c = A(45);
}

// int a();
// int a, align(2) b align(4) = 1.2f;
