
// #define __QOS_ENUM(name) typedef name foo;

// __QOS_ENUM(foobar);
// #define __QOS_ENUM(name) typedef name  foo
// #define F(...) __VA_ARG__
enum { a = 4, f = 3 };

#define a1(y) (y+2)
#define a2(y) a1(y)+1
#define f a+f

int i;

int main() {
    i = 234;
    return i;
}