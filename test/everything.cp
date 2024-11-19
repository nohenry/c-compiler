#define BAR(x) (x)
#define FOO BAR
#define MY_CONST const

MY_CONST int a = FOO(345);

typedef enum foo {
    A,B,C = 2
} Foo;

struct data {
    int *d;
    union {
        int i;
        float f;
    };
};

extern int result(struct data*);
int foo_something(Foo) {
    struct data d = {
        .d = 0,
        .i = 34,
    };
    struct data dd = {
        .d = 0,
        .i = 2048,
    };

    int sum = 0;
    for (int i = 0; i < 23; ++i) {
        if (i % 2 == 0) {
            sum += result(&d);
        } else {
            sum += result(&dd);
        }
    }

    switch (sum) {
        case 0:
            goto END;
            break;
        case 2:
            sum *= 4;
            break;
        default: break;
    }

END:
    return sum;
}