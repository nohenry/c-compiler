#include <iconv.h>
// #include <time.h>
// #include <stdarg.h>
// #include <sys/cdefs.h>

// extern long timezone __asm("foobar");
// int args(const char *c, ...) {
//     va_list v;
//     va_start(v, c);
//     va_arg(v, int);
//     va_end(v);
//     return 34;
// }

// int printf(const char *c, ...);

// typedef struct foo *bar;
// bar dobar() {
// }

// int *bar[2];

int main() {
    // fpos_t f;
    iconv_t cd;
    cd = iconv_open ("IBM1047", "UTF-8");
    if (cd == (iconv_t) -1)
        return 1;
    return 0;
}
