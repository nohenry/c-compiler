// #include <iconv.h>
#include <stdarg.h>

int args(const char *c, ...) {
    va_list v;
    va_start(v, c);
    va_arg(v, int);
    return 34;
}

int main()
{
    return args("fd", 23);
    // cd = iconv_open ("IBM1047", "UTF-8");
    // if (cd == (iconv_t) -1)
    //     return 1;
    // return 0;
}