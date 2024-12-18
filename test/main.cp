// #include <string.h>
// #include <stdlib.h>
// #include <signal.h>

// #define STR(x) #x

// char *a = STR(@foo), *b = "@foo";

// typedef int foo;

// void (*signal(int, void (*)(int)))(int);
// void (*signal(int, void (*)(int)))(int);
// signal(int, void (*)(int))

// sighandler_t;
typedef void (*sighandler_t)(int);
sighandler_t (*signal1)(int, sighandler_t);
// void (*signal)(int);

void (* (*signal) (int, void (*)(int)) ) (int);
void (* signa2 (int, void (*)(int)) ) (int);
void *foo1;
void *foo2();
void (*foo3)();

int main(void)
{

    // signal();
    // foo bar;
    // malloc_type_posix_memalign
    // malloc(0);
    // malloc_zone_t *f;
    //   if (strcmp (a, b))
    // abort ();
    return 0;
}
// char array[(a2(f)) == 10 ? 1 : -1];
