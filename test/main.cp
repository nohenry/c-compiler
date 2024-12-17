// #include <iconv.h>
// #include <time.h>
#include <stdarg.h>
// #include <sys/cdefs.h>

// extern long timezone __asm("foobar");
int printf(const char *c, ...);

int args(int count, ...) {
    va_list v;
    va_start(v, count);
    int i1 = va_arg(v, int);
    printf("%d ", i1);
    int i2 = va_arg(v, int);
    printf("%d ", i2);
    va_end(v);
    return 34;
}


// typedef struct foo *bar;
// bar dobar() {
// }

// int *bar[2];

int main() {
    args(1, 2, 69);
    // printf("%c %d\n", 'c', 3);
    // fpos_t f;
    // iconv_t cd;
    // cd = iconv_open ("IBM1047", "UTF-8");
    // if (cd == (iconv_t) -1)
    //     return 1;
    return 0;
}
