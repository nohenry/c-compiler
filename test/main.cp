
int foobar(int, int, ...);

// long hello(char c) {
//     return (short)c;
// }

long foo(signed int f) {
    return (unsigned char)f;
}

long foo1(char f) {
    return f;
}

long foo2(signed char f) {
    long s = (signed int)f;
    return (unsigned short)s;
}

int main(int argc, char **argv) {
    return (argv[0][0]);
}
