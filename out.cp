typedef int __uint128_t;
typedef int __int128_t;
typedef int __builtin_va_list;
extern float _Complex cacosf(float _Complex);
extern double _Complex cacos(double _Complex);
extern long double _Complex cacosl(long double _Complex);
extern float _Complex casinf(float _Complex);
extern double _Complex casin(double _Complex);
extern long double _Complex casinl(long double _Complex);
extern float _Complex catanf(float _Complex);
extern double _Complex catan(double _Complex);
extern long double _Complex catanl(long double _Complex);
extern float _Complex ccosf(float _Complex);
extern double _Complex ccos(double _Complex);
extern long double _Complex ccosl(long double _Complex);
extern float _Complex csinf(float _Complex);
extern double _Complex csin(double _Complex);
extern long double _Complex csinl(long double _Complex);
extern float _Complex ctanf(float _Complex);
extern double _Complex ctan(double _Complex);
extern long double _Complex ctanl(long double _Complex);
extern float _Complex cacoshf(float _Complex);
extern double _Complex cacosh(double _Complex);
extern long double _Complex cacoshl(long double _Complex);
extern float _Complex casinhf(float _Complex);
extern double _Complex casinh(double _Complex);
extern long double _Complex casinhl(long double _Complex);
extern float _Complex catanhf(float _Complex);
extern double _Complex catanh(double _Complex);
extern long double _Complex catanhl(long double _Complex);
extern float _Complex ccoshf(float _Complex);
extern double _Complex ccosh(double _Complex);
extern long double _Complex ccoshl(long double _Complex);
extern float _Complex csinhf(float _Complex);
extern double _Complex csinh(double _Complex);
extern long double _Complex csinhl(long double _Complex);
extern float _Complex ctanhf(float _Complex);
extern double _Complex ctanh(double _Complex);
extern long double _Complex ctanhl(long double _Complex);
extern float _Complex cexpf(float _Complex);
extern double _Complex cexp(double _Complex);
extern long double _Complex cexpl(long double _Complex);
extern float _Complex clogf(float _Complex);
extern double _Complex clog(double _Complex);
extern long double _Complex clogl(long double _Complex);
extern float cabsf(float _Complex);
extern double cabs(double _Complex);
extern long double cabsl(long double _Complex);
extern float _Complex cpowf(float _Complex, float _Complex);
extern double _Complex cpow(double _Complex, double _Complex);
extern long double _Complex cpowl(long double _Complex, long double _Complex);
extern float _Complex csqrtf(float _Complex);
extern double _Complex csqrt(double _Complex);
extern long double _Complex csqrtl(long double _Complex);
extern float cargf(float _Complex);
extern double carg(double _Complex);
extern long double cargl(long double _Complex);
extern float cimagf(float _Complex);
extern double cimag(double _Complex);
extern long double cimagl(long double _Complex);
extern float _Complex conjf(float _Complex);
extern double _Complex conj(double _Complex);
extern long double _Complex conjl(long double _Complex);
extern float _Complex cprojf(float _Complex);
extern double _Complex cproj(double _Complex);
extern long double _Complex cprojl(long double _Complex);
extern float crealf(float _Complex);
extern double creal(double _Complex);
extern long double creall(long double _Complex);
extern void abort(void);
extern int fails;
typedef struct
{
    float f;
    double d;
} Sfd;
typedef struct
{
    float f;
    long double l;
} Sfl;
typedef struct
{
    double d;
    float f;
} Sdf;
typedef struct
{
    double d;
    long double l;
} Sdl;
typedef struct
{
    long double l;
    float f;
} Slf;
typedef struct
{
    long double l;
    double d;
} Sld;
typedef struct
{
    float f;
    double d;
    long double l;
} Sfdl;
typedef struct
{
    float f;
    long double l;
    double d;
} Sfld;
typedef struct
{
    double d;
    float f;
    long double l;
} Sdfl;
typedef struct
{
    double d;
    long double l;
    float f;
} Sdlf;
typedef struct
{
    long double l;
    float f;
    double d;
} Slfd;
typedef struct
{
    long double l;
    double d;
    float f;
} Sldf;
void checkSfd(Sfd x, double y)
{
    if (x.f != y || x.d != y + 1)
        abort();
}
void checkSfl(Sfl x, double y)
{
    if (x.f != y || x.l != y + 1)
        abort();
}
void checkSdf(Sdf x, double y)
{
    if (x.d != y || x.f != y + 1)
        abort();
}
void checkSdl(Sdl x, double y)
{
    if (x.d != y || x.l != y + 1)
        abort();
}
void checkSlf(Slf x, double y)
{
    if (x.l != y || x.f != y + 1)
        abort();
}
void checkSld(Sld x, double y)
{
    if (x.l != y || x.d != y + 1)
        abort();
}
void checkSfdl(Sfdl x, double y)
{
    if (x.f != y || x.d != y + 1 || x.l != y + 2)
        abort();
}
void checkSfld(Sfld x, double y)
{
    if (x.f != y || x.l != y + 1 || x.d != y + 2)
        abort();
}
void checkSdfl(Sdfl x, double y)
{
    if (x.d != y || x.f != y + 1 || x.l != y + 2)
        abort();
}
void checkSdlf(Sdlf x, double y)
{
    if (x.d != y || x.l != y + 1 || x.f != y + 2)
        abort();
}
void checkSlfd(Slfd x, double y)
{
    if (x.l != y || x.f != y + 1 || x.d != y + 2)
        abort();
}
void checkSldf(Sldf x, double y)
{
    if (x.l != y || x.d != y + 1 || x.f != y + 2)
        abort();
}
Sfd g1sSfd, g2sSfd, g3sSfd, g4sSfd;
Sfd g5sSfd, g6sSfd, g7sSfd, g8sSfd;
Sfd g9sSfd, g10sSfd, g11sSfd, g12sSfd;
Sfd g13sSfd, g14sSfd, g15sSfd, g16sSfd;
extern void initSfd(Sfd *p, double x);
extern void checkgSfd(void);
extern void testSfd(Sfd s1, Sfd s2, Sfd s3, Sfd s4, Sfd s5, Sfd s6, Sfd s7, Sfd s8, Sfd s9, Sfd s10, Sfd s11, Sfd s12, Sfd s13, Sfd s14, Sfd s15, Sfd s16);
extern void testvaSfd(int n, ...);
void test2_Sfd(Sfd s1, Sfd s2, Sfd s3, Sfd s4, Sfd s5, Sfd s6, Sfd s7, Sfd s8) { testSfd(s1, g2sSfd, s2, g4sSfd, s3, g6sSfd, s4, g8sSfd, s5, g10sSfd, s6, g12sSfd, s7, g14sSfd, s8, g16sSfd); }
void testitSfd(void)
{
    ;
    ;
    initSfd(&g1sSfd, (double)1);
    initSfd(&g2sSfd, (double)2);
    initSfd(&g3sSfd, (double)3);
    initSfd(&g4sSfd, (double)4);
    initSfd(&g5sSfd, (double)5);
    initSfd(&g6sSfd, (double)6);
    initSfd(&g7sSfd, (double)7);
    initSfd(&g8sSfd, (double)8);
    initSfd(&g9sSfd, (double)9);
    initSfd(&g10sSfd, (double)10);
    initSfd(&g11sSfd, (double)11);
    initSfd(&g12sSfd, (double)12);
    initSfd(&g13sSfd, (double)13);
    initSfd(&g14sSfd, (double)14);
    initSfd(&g15sSfd, (double)15);
    initSfd(&g16sSfd, (double)16);
    checkgSfd();
    ;
    ;
    ;
    testSfd(g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd, g7sSfd, g8sSfd, g9sSfd, g10sSfd, g11sSfd, g12sSfd, g13sSfd, g14sSfd, g15sSfd, g16sSfd);
    ;
    ;
    ;
    ;
    testvaSfd(1, g1sSfd);
    ;
    testvaSfd(2, g1sSfd, g2sSfd);
    ;
    testvaSfd(3, g1sSfd, g2sSfd, g3sSfd);
    ;
    testvaSfd(4, g1sSfd, g2sSfd, g3sSfd, g4sSfd);
    ;
    testvaSfd(5, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd);
    ;
    testvaSfd(6, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd);
    ;
    testvaSfd(7, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd, g7sSfd);
    ;
    testvaSfd(8, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd, g7sSfd, g8sSfd);
    ;
    testvaSfd(9, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd, g7sSfd, g8sSfd, g9sSfd);
    ;
    testvaSfd(10, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd, g7sSfd, g8sSfd, g9sSfd, g10sSfd);
    ;
    testvaSfd(11, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd, g7sSfd, g8sSfd, g9sSfd, g10sSfd, g11sSfd);
    ;
    testvaSfd(12, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd, g7sSfd, g8sSfd, g9sSfd, g10sSfd, g11sSfd, g12sSfd);
    ;
    testvaSfd(13, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd, g7sSfd, g8sSfd, g9sSfd, g10sSfd, g11sSfd, g12sSfd, g13sSfd);
    ;
    testvaSfd(14, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd, g7sSfd, g8sSfd, g9sSfd, g10sSfd, g11sSfd, g12sSfd, g13sSfd, g14sSfd);
    ;
    testvaSfd(15, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd, g7sSfd, g8sSfd, g9sSfd, g10sSfd, g11sSfd, g12sSfd, g13sSfd, g14sSfd, g15sSfd);
    ;
    testvaSfd(16, g1sSfd, g2sSfd, g3sSfd, g4sSfd, g5sSfd, g6sSfd, g7sSfd, g8sSfd, g9sSfd, g10sSfd, g11sSfd, g12sSfd, g13sSfd, g14sSfd, g15sSfd, g16sSfd);
    ;
    ;
    ;
    test2_Sfd(g1sSfd, g3sSfd, g5sSfd, g7sSfd, g9sSfd, g11sSfd, g13sSfd, g15sSfd);
    ;
}
Sfl g1sSfl, g2sSfl, g3sSfl, g4sSfl;
Sfl g5sSfl, g6sSfl, g7sSfl, g8sSfl;
Sfl g9sSfl, g10sSfl, g11sSfl, g12sSfl;
Sfl g13sSfl, g14sSfl, g15sSfl, g16sSfl;
extern void initSfl(Sfl *p, double x);
extern void checkgSfl(void);
extern void testSfl(Sfl s1, Sfl s2, Sfl s3, Sfl s4, Sfl s5, Sfl s6, Sfl s7, Sfl s8, Sfl s9, Sfl s10, Sfl s11, Sfl s12, Sfl s13, Sfl s14, Sfl s15, Sfl s16);
extern void testvaSfl(int n, ...);
void test2_Sfl(Sfl s1, Sfl s2, Sfl s3, Sfl s4, Sfl s5, Sfl s6, Sfl s7, Sfl s8) { testSfl(s1, g2sSfl, s2, g4sSfl, s3, g6sSfl, s4, g8sSfl, s5, g10sSfl, s6, g12sSfl, s7, g14sSfl, s8, g16sSfl); }
void testitSfl(void)
{
    ;
    ;
    initSfl(&g1sSfl, (double)1);
    initSfl(&g2sSfl, (double)2);
    initSfl(&g3sSfl, (double)3);
    initSfl(&g4sSfl, (double)4);
    initSfl(&g5sSfl, (double)5);
    initSfl(&g6sSfl, (double)6);
    initSfl(&g7sSfl, (double)7);
    initSfl(&g8sSfl, (double)8);
    initSfl(&g9sSfl, (double)9);
    initSfl(&g10sSfl, (double)10);
    initSfl(&g11sSfl, (double)11);
    initSfl(&g12sSfl, (double)12);
    initSfl(&g13sSfl, (double)13);
    initSfl(&g14sSfl, (double)14);
    initSfl(&g15sSfl, (double)15);
    initSfl(&g16sSfl, (double)16);
    checkgSfl();
    ;
    ;
    ;
    testSfl(g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl, g7sSfl, g8sSfl, g9sSfl, g10sSfl, g11sSfl, g12sSfl, g13sSfl, g14sSfl, g15sSfl, g16sSfl);
    ;
    ;
    ;
    ;
    testvaSfl(1, g1sSfl);
    ;
    testvaSfl(2, g1sSfl, g2sSfl);
    ;
    testvaSfl(3, g1sSfl, g2sSfl, g3sSfl);
    ;
    testvaSfl(4, g1sSfl, g2sSfl, g3sSfl, g4sSfl);
    ;
    testvaSfl(5, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl);
    ;
    testvaSfl(6, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl);
    ;
    testvaSfl(7, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl, g7sSfl);
    ;
    testvaSfl(8, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl, g7sSfl, g8sSfl);
    ;
    testvaSfl(9, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl, g7sSfl, g8sSfl, g9sSfl);
    ;
    testvaSfl(10, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl, g7sSfl, g8sSfl, g9sSfl, g10sSfl);
    ;
    testvaSfl(11, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl, g7sSfl, g8sSfl, g9sSfl, g10sSfl, g11sSfl);
    ;
    testvaSfl(12, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl, g7sSfl, g8sSfl, g9sSfl, g10sSfl, g11sSfl, g12sSfl);
    ;
    testvaSfl(13, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl, g7sSfl, g8sSfl, g9sSfl, g10sSfl, g11sSfl, g12sSfl, g13sSfl);
    ;
    testvaSfl(14, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl, g7sSfl, g8sSfl, g9sSfl, g10sSfl, g11sSfl, g12sSfl, g13sSfl, g14sSfl);
    ;
    testvaSfl(15, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl, g7sSfl, g8sSfl, g9sSfl, g10sSfl, g11sSfl, g12sSfl, g13sSfl, g14sSfl, g15sSfl);
    ;
    testvaSfl(16, g1sSfl, g2sSfl, g3sSfl, g4sSfl, g5sSfl, g6sSfl, g7sSfl, g8sSfl, g9sSfl, g10sSfl, g11sSfl, g12sSfl, g13sSfl, g14sSfl, g15sSfl, g16sSfl);
    ;
    ;
    ;
    test2_Sfl(g1sSfl, g3sSfl, g5sSfl, g7sSfl, g9sSfl, g11sSfl, g13sSfl, g15sSfl);
    ;
}
Sdf g1sSdf, g2sSdf, g3sSdf, g4sSdf;
Sdf g5sSdf, g6sSdf, g7sSdf, g8sSdf;
Sdf g9sSdf, g10sSdf, g11sSdf, g12sSdf;
Sdf g13sSdf, g14sSdf, g15sSdf, g16sSdf;
extern void initSdf(Sdf *p, double x);
extern void checkgSdf(void);
extern void testSdf(Sdf s1, Sdf s2, Sdf s3, Sdf s4, Sdf s5, Sdf s6, Sdf s7, Sdf s8, Sdf s9, Sdf s10, Sdf s11, Sdf s12, Sdf s13, Sdf s14, Sdf s15, Sdf s16);
extern void testvaSdf(int n, ...);
void test2_Sdf(Sdf s1, Sdf s2, Sdf s3, Sdf s4, Sdf s5, Sdf s6, Sdf s7, Sdf s8) { testSdf(s1, g2sSdf, s2, g4sSdf, s3, g6sSdf, s4, g8sSdf, s5, g10sSdf, s6, g12sSdf, s7, g14sSdf, s8, g16sSdf); }
void testitSdf(void)
{
    ;
    ;
    initSdf(&g1sSdf, (double)1);
    initSdf(&g2sSdf, (double)2);
    initSdf(&g3sSdf, (double)3);
    initSdf(&g4sSdf, (double)4);
    initSdf(&g5sSdf, (double)5);
    initSdf(&g6sSdf, (double)6);
    initSdf(&g7sSdf, (double)7);
    initSdf(&g8sSdf, (double)8);
    initSdf(&g9sSdf, (double)9);
    initSdf(&g10sSdf, (double)10);
    initSdf(&g11sSdf, (double)11);
    initSdf(&g12sSdf, (double)12);
    initSdf(&g13sSdf, (double)13);
    initSdf(&g14sSdf, (double)14);
    initSdf(&g15sSdf, (double)15);
    initSdf(&g16sSdf, (double)16);
    checkgSdf();
    ;
    ;
    ;
    testSdf(g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf, g7sSdf, g8sSdf, g9sSdf, g10sSdf, g11sSdf, g12sSdf, g13sSdf, g14sSdf, g15sSdf, g16sSdf);
    ;
    ;
    ;
    ;
    testvaSdf(1, g1sSdf);
    ;
    testvaSdf(2, g1sSdf, g2sSdf);
    ;
    testvaSdf(3, g1sSdf, g2sSdf, g3sSdf);
    ;
    testvaSdf(4, g1sSdf, g2sSdf, g3sSdf, g4sSdf);
    ;
    testvaSdf(5, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf);
    ;
    testvaSdf(6, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf);
    ;
    testvaSdf(7, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf, g7sSdf);
    ;
    testvaSdf(8, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf, g7sSdf, g8sSdf);
    ;
    testvaSdf(9, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf, g7sSdf, g8sSdf, g9sSdf);
    ;
    testvaSdf(10, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf, g7sSdf, g8sSdf, g9sSdf, g10sSdf);
    ;
    testvaSdf(11, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf, g7sSdf, g8sSdf, g9sSdf, g10sSdf, g11sSdf);
    ;
    testvaSdf(12, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf, g7sSdf, g8sSdf, g9sSdf, g10sSdf, g11sSdf, g12sSdf);
    ;
    testvaSdf(13, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf, g7sSdf, g8sSdf, g9sSdf, g10sSdf, g11sSdf, g12sSdf, g13sSdf);
    ;
    testvaSdf(14, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf, g7sSdf, g8sSdf, g9sSdf, g10sSdf, g11sSdf, g12sSdf, g13sSdf, g14sSdf);
    ;
    testvaSdf(15, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf, g7sSdf, g8sSdf, g9sSdf, g10sSdf, g11sSdf, g12sSdf, g13sSdf, g14sSdf, g15sSdf);
    ;
    testvaSdf(16, g1sSdf, g2sSdf, g3sSdf, g4sSdf, g5sSdf, g6sSdf, g7sSdf, g8sSdf, g9sSdf, g10sSdf, g11sSdf, g12sSdf, g13sSdf, g14sSdf, g15sSdf, g16sSdf);
    ;
    ;
    ;
    test2_Sdf(g1sSdf, g3sSdf, g5sSdf, g7sSdf, g9sSdf, g11sSdf, g13sSdf, g15sSdf);
    ;
}
Sdl g1sSdl, g2sSdl, g3sSdl, g4sSdl;
Sdl g5sSdl, g6sSdl, g7sSdl, g8sSdl;
Sdl g9sSdl, g10sSdl, g11sSdl, g12sSdl;
Sdl g13sSdl, g14sSdl, g15sSdl, g16sSdl;
extern void initSdl(Sdl *p, double x);
extern void checkgSdl(void);
extern void testSdl(Sdl s1, Sdl s2, Sdl s3, Sdl s4, Sdl s5, Sdl s6, Sdl s7, Sdl s8, Sdl s9, Sdl s10, Sdl s11, Sdl s12, Sdl s13, Sdl s14, Sdl s15, Sdl s16);
extern void testvaSdl(int n, ...);
void test2_Sdl(Sdl s1, Sdl s2, Sdl s3, Sdl s4, Sdl s5, Sdl s6, Sdl s7, Sdl s8) { testSdl(s1, g2sSdl, s2, g4sSdl, s3, g6sSdl, s4, g8sSdl, s5, g10sSdl, s6, g12sSdl, s7, g14sSdl, s8, g16sSdl); }
void testitSdl(void)
{
    ;
    ;
    initSdl(&g1sSdl, (double)1);
    initSdl(&g2sSdl, (double)2);
    initSdl(&g3sSdl, (double)3);
    initSdl(&g4sSdl, (double)4);
    initSdl(&g5sSdl, (double)5);
    initSdl(&g6sSdl, (double)6);
    initSdl(&g7sSdl, (double)7);
    initSdl(&g8sSdl, (double)8);
    initSdl(&g9sSdl, (double)9);
    initSdl(&g10sSdl, (double)10);
    initSdl(&g11sSdl, (double)11);
    initSdl(&g12sSdl, (double)12);
    initSdl(&g13sSdl, (double)13);
    initSdl(&g14sSdl, (double)14);
    initSdl(&g15sSdl, (double)15);
    initSdl(&g16sSdl, (double)16);
    checkgSdl();
    ;
    ;
    ;
    testSdl(g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl, g7sSdl, g8sSdl, g9sSdl, g10sSdl, g11sSdl, g12sSdl, g13sSdl, g14sSdl, g15sSdl, g16sSdl);
    ;
    ;
    ;
    ;
    testvaSdl(1, g1sSdl);
    ;
    testvaSdl(2, g1sSdl, g2sSdl);
    ;
    testvaSdl(3, g1sSdl, g2sSdl, g3sSdl);
    ;
    testvaSdl(4, g1sSdl, g2sSdl, g3sSdl, g4sSdl);
    ;
    testvaSdl(5, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl);
    ;
    testvaSdl(6, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl);
    ;
    testvaSdl(7, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl, g7sSdl);
    ;
    testvaSdl(8, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl, g7sSdl, g8sSdl);
    ;
    testvaSdl(9, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl, g7sSdl, g8sSdl, g9sSdl);
    ;
    testvaSdl(10, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl, g7sSdl, g8sSdl, g9sSdl, g10sSdl);
    ;
    testvaSdl(11, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl, g7sSdl, g8sSdl, g9sSdl, g10sSdl, g11sSdl);
    ;
    testvaSdl(12, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl, g7sSdl, g8sSdl, g9sSdl, g10sSdl, g11sSdl, g12sSdl);
    ;
    testvaSdl(13, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl, g7sSdl, g8sSdl, g9sSdl, g10sSdl, g11sSdl, g12sSdl, g13sSdl);
    ;
    testvaSdl(14, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl, g7sSdl, g8sSdl, g9sSdl, g10sSdl, g11sSdl, g12sSdl, g13sSdl, g14sSdl);
    ;
    testvaSdl(15, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl, g7sSdl, g8sSdl, g9sSdl, g10sSdl, g11sSdl, g12sSdl, g13sSdl, g14sSdl, g15sSdl);
    ;
    testvaSdl(16, g1sSdl, g2sSdl, g3sSdl, g4sSdl, g5sSdl, g6sSdl, g7sSdl, g8sSdl, g9sSdl, g10sSdl, g11sSdl, g12sSdl, g13sSdl, g14sSdl, g15sSdl, g16sSdl);
    ;
    ;
    ;
    test2_Sdl(g1sSdl, g3sSdl, g5sSdl, g7sSdl, g9sSdl, g11sSdl, g13sSdl, g15sSdl);
    ;
}
Slf g1sSlf, g2sSlf, g3sSlf, g4sSlf;
Slf g5sSlf, g6sSlf, g7sSlf, g8sSlf;
Slf g9sSlf, g10sSlf, g11sSlf, g12sSlf;
Slf g13sSlf, g14sSlf, g15sSlf, g16sSlf;
extern void initSlf(Slf *p, double x);
extern void checkgSlf(void);
extern void testSlf(Slf s1, Slf s2, Slf s3, Slf s4, Slf s5, Slf s6, Slf s7, Slf s8, Slf s9, Slf s10, Slf s11, Slf s12, Slf s13, Slf s14, Slf s15, Slf s16);
extern void testvaSlf(int n, ...);
void test2_Slf(Slf s1, Slf s2, Slf s3, Slf s4, Slf s5, Slf s6, Slf s7, Slf s8) { testSlf(s1, g2sSlf, s2, g4sSlf, s3, g6sSlf, s4, g8sSlf, s5, g10sSlf, s6, g12sSlf, s7, g14sSlf, s8, g16sSlf); }
void testitSlf(void)
{
    ;
    ;
    initSlf(&g1sSlf, (double)1);
    initSlf(&g2sSlf, (double)2);
    initSlf(&g3sSlf, (double)3);
    initSlf(&g4sSlf, (double)4);
    initSlf(&g5sSlf, (double)5);
    initSlf(&g6sSlf, (double)6);
    initSlf(&g7sSlf, (double)7);
    initSlf(&g8sSlf, (double)8);
    initSlf(&g9sSlf, (double)9);
    initSlf(&g10sSlf, (double)10);
    initSlf(&g11sSlf, (double)11);
    initSlf(&g12sSlf, (double)12);
    initSlf(&g13sSlf, (double)13);
    initSlf(&g14sSlf, (double)14);
    initSlf(&g15sSlf, (double)15);
    initSlf(&g16sSlf, (double)16);
    checkgSlf();
    ;
    ;
    ;
    testSlf(g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf, g7sSlf, g8sSlf, g9sSlf, g10sSlf, g11sSlf, g12sSlf, g13sSlf, g14sSlf, g15sSlf, g16sSlf);
    ;
    ;
    ;
    ;
    testvaSlf(1, g1sSlf);
    ;
    testvaSlf(2, g1sSlf, g2sSlf);
    ;
    testvaSlf(3, g1sSlf, g2sSlf, g3sSlf);
    ;
    testvaSlf(4, g1sSlf, g2sSlf, g3sSlf, g4sSlf);
    ;
    testvaSlf(5, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf);
    ;
    testvaSlf(6, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf);
    ;
    testvaSlf(7, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf, g7sSlf);
    ;
    testvaSlf(8, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf, g7sSlf, g8sSlf);
    ;
    testvaSlf(9, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf, g7sSlf, g8sSlf, g9sSlf);
    ;
    testvaSlf(10, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf, g7sSlf, g8sSlf, g9sSlf, g10sSlf);
    ;
    testvaSlf(11, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf, g7sSlf, g8sSlf, g9sSlf, g10sSlf, g11sSlf);
    ;
    testvaSlf(12, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf, g7sSlf, g8sSlf, g9sSlf, g10sSlf, g11sSlf, g12sSlf);
    ;
    testvaSlf(13, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf, g7sSlf, g8sSlf, g9sSlf, g10sSlf, g11sSlf, g12sSlf, g13sSlf);
    ;
    testvaSlf(14, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf, g7sSlf, g8sSlf, g9sSlf, g10sSlf, g11sSlf, g12sSlf, g13sSlf, g14sSlf);
    ;
    testvaSlf(15, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf, g7sSlf, g8sSlf, g9sSlf, g10sSlf, g11sSlf, g12sSlf, g13sSlf, g14sSlf, g15sSlf);
    ;
    testvaSlf(16, g1sSlf, g2sSlf, g3sSlf, g4sSlf, g5sSlf, g6sSlf, g7sSlf, g8sSlf, g9sSlf, g10sSlf, g11sSlf, g12sSlf, g13sSlf, g14sSlf, g15sSlf, g16sSlf);
    ;
    ;
    ;
    test2_Slf(g1sSlf, g3sSlf, g5sSlf, g7sSlf, g9sSlf, g11sSlf, g13sSlf, g15sSlf);
    ;
}
Sld g1sSld, g2sSld, g3sSld, g4sSld;
Sld g5sSld, g6sSld, g7sSld, g8sSld;
Sld g9sSld, g10sSld, g11sSld, g12sSld;
Sld g13sSld, g14sSld, g15sSld, g16sSld;
extern void initSld(Sld *p, double x);
extern void checkgSld(void);
extern void testSld(Sld s1, Sld s2, Sld s3, Sld s4, Sld s5, Sld s6, Sld s7, Sld s8, Sld s9, Sld s10, Sld s11, Sld s12, Sld s13, Sld s14, Sld s15, Sld s16);
extern void testvaSld(int n, ...);
void test2_Sld(Sld s1, Sld s2, Sld s3, Sld s4, Sld s5, Sld s6, Sld s7, Sld s8) { testSld(s1, g2sSld, s2, g4sSld, s3, g6sSld, s4, g8sSld, s5, g10sSld, s6, g12sSld, s7, g14sSld, s8, g16sSld); }
void testitSld(void)
{
    ;
    ;
    initSld(&g1sSld, (double)1);
    initSld(&g2sSld, (double)2);
    initSld(&g3sSld, (double)3);
    initSld(&g4sSld, (double)4);
    initSld(&g5sSld, (double)5);
    initSld(&g6sSld, (double)6);
    initSld(&g7sSld, (double)7);
    initSld(&g8sSld, (double)8);
    initSld(&g9sSld, (double)9);
    initSld(&g10sSld, (double)10);
    initSld(&g11sSld, (double)11);
    initSld(&g12sSld, (double)12);
    initSld(&g13sSld, (double)13);
    initSld(&g14sSld, (double)14);
    initSld(&g15sSld, (double)15);
    initSld(&g16sSld, (double)16);
    checkgSld();
    ;
    ;
    ;
    testSld(g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld, g7sSld, g8sSld, g9sSld, g10sSld, g11sSld, g12sSld, g13sSld, g14sSld, g15sSld, g16sSld);
    ;
    ;
    ;
    ;
    testvaSld(1, g1sSld);
    ;
    testvaSld(2, g1sSld, g2sSld);
    ;
    testvaSld(3, g1sSld, g2sSld, g3sSld);
    ;
    testvaSld(4, g1sSld, g2sSld, g3sSld, g4sSld);
    ;
    testvaSld(5, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld);
    ;
    testvaSld(6, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld);
    ;
    testvaSld(7, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld, g7sSld);
    ;
    testvaSld(8, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld, g7sSld, g8sSld);
    ;
    testvaSld(9, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld, g7sSld, g8sSld, g9sSld);
    ;
    testvaSld(10, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld, g7sSld, g8sSld, g9sSld, g10sSld);
    ;
    testvaSld(11, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld, g7sSld, g8sSld, g9sSld, g10sSld, g11sSld);
    ;
    testvaSld(12, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld, g7sSld, g8sSld, g9sSld, g10sSld, g11sSld, g12sSld);
    ;
    testvaSld(13, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld, g7sSld, g8sSld, g9sSld, g10sSld, g11sSld, g12sSld, g13sSld);
    ;
    testvaSld(14, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld, g7sSld, g8sSld, g9sSld, g10sSld, g11sSld, g12sSld, g13sSld, g14sSld);
    ;
    testvaSld(15, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld, g7sSld, g8sSld, g9sSld, g10sSld, g11sSld, g12sSld, g13sSld, g14sSld, g15sSld);
    ;
    testvaSld(16, g1sSld, g2sSld, g3sSld, g4sSld, g5sSld, g6sSld, g7sSld, g8sSld, g9sSld, g10sSld, g11sSld, g12sSld, g13sSld, g14sSld, g15sSld, g16sSld);
    ;
    ;
    ;
    test2_Sld(g1sSld, g3sSld, g5sSld, g7sSld, g9sSld, g11sSld, g13sSld, g15sSld);
    ;
}
Sfdl g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl;
Sfdl g5sSfdl, g6sSfdl, g7sSfdl, g8sSfdl;
Sfdl g9sSfdl, g10sSfdl, g11sSfdl, g12sSfdl;
Sfdl g13sSfdl, g14sSfdl, g15sSfdl, g16sSfdl;
extern void initSfdl(Sfdl *p, double x);
extern void checkgSfdl(void);
extern void testSfdl(Sfdl s1, Sfdl s2, Sfdl s3, Sfdl s4, Sfdl s5, Sfdl s6, Sfdl s7, Sfdl s8, Sfdl s9, Sfdl s10, Sfdl s11, Sfdl s12, Sfdl s13, Sfdl s14, Sfdl s15, Sfdl s16);
extern void testvaSfdl(int n, ...);
void test2_Sfdl(Sfdl s1, Sfdl s2, Sfdl s3, Sfdl s4, Sfdl s5, Sfdl s6, Sfdl s7, Sfdl s8) { testSfdl(s1, g2sSfdl, s2, g4sSfdl, s3, g6sSfdl, s4, g8sSfdl, s5, g10sSfdl, s6, g12sSfdl, s7, g14sSfdl, s8, g16sSfdl); }
void testitSfdl(void)
{
    ;
    ;
    initSfdl(&g1sSfdl, (double)1);
    initSfdl(&g2sSfdl, (double)2);
    initSfdl(&g3sSfdl, (double)3);
    initSfdl(&g4sSfdl, (double)4);
    initSfdl(&g5sSfdl, (double)5);
    initSfdl(&g6sSfdl, (double)6);
    initSfdl(&g7sSfdl, (double)7);
    initSfdl(&g8sSfdl, (double)8);
    initSfdl(&g9sSfdl, (double)9);
    initSfdl(&g10sSfdl, (double)10);
    initSfdl(&g11sSfdl, (double)11);
    initSfdl(&g12sSfdl, (double)12);
    initSfdl(&g13sSfdl, (double)13);
    initSfdl(&g14sSfdl, (double)14);
    initSfdl(&g15sSfdl, (double)15);
    initSfdl(&g16sSfdl, (double)16);
    checkgSfdl();
    ;
    ;
    ;
    testSfdl(g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl, g7sSfdl, g8sSfdl, g9sSfdl, g10sSfdl, g11sSfdl, g12sSfdl, g13sSfdl, g14sSfdl, g15sSfdl, g16sSfdl);
    ;
    ;
    ;
    ;
    testvaSfdl(1, g1sSfdl);
    ;
    testvaSfdl(2, g1sSfdl, g2sSfdl);
    ;
    testvaSfdl(3, g1sSfdl, g2sSfdl, g3sSfdl);
    ;
    testvaSfdl(4, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl);
    ;
    testvaSfdl(5, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl);
    ;
    testvaSfdl(6, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl);
    ;
    testvaSfdl(7, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl, g7sSfdl);
    ;
    testvaSfdl(8, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl, g7sSfdl, g8sSfdl);
    ;
    testvaSfdl(9, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl, g7sSfdl, g8sSfdl, g9sSfdl);
    ;
    testvaSfdl(10, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl, g7sSfdl, g8sSfdl, g9sSfdl, g10sSfdl);
    ;
    testvaSfdl(11, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl, g7sSfdl, g8sSfdl, g9sSfdl, g10sSfdl, g11sSfdl);
    ;
    testvaSfdl(12, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl, g7sSfdl, g8sSfdl, g9sSfdl, g10sSfdl, g11sSfdl, g12sSfdl);
    ;
    testvaSfdl(13, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl, g7sSfdl, g8sSfdl, g9sSfdl, g10sSfdl, g11sSfdl, g12sSfdl, g13sSfdl);
    ;
    testvaSfdl(14, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl, g7sSfdl, g8sSfdl, g9sSfdl, g10sSfdl, g11sSfdl, g12sSfdl, g13sSfdl, g14sSfdl);
    ;
    testvaSfdl(15, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl, g7sSfdl, g8sSfdl, g9sSfdl, g10sSfdl, g11sSfdl, g12sSfdl, g13sSfdl, g14sSfdl, g15sSfdl);
    ;
    testvaSfdl(16, g1sSfdl, g2sSfdl, g3sSfdl, g4sSfdl, g5sSfdl, g6sSfdl, g7sSfdl, g8sSfdl, g9sSfdl, g10sSfdl, g11sSfdl, g12sSfdl, g13sSfdl, g14sSfdl, g15sSfdl, g16sSfdl);
    ;
    ;
    ;
    test2_Sfdl(g1sSfdl, g3sSfdl, g5sSfdl, g7sSfdl, g9sSfdl, g11sSfdl, g13sSfdl, g15sSfdl);
    ;
}
Sfld g1sSfld, g2sSfld, g3sSfld, g4sSfld;
Sfld g5sSfld, g6sSfld, g7sSfld, g8sSfld;
Sfld g9sSfld, g10sSfld, g11sSfld, g12sSfld;
Sfld g13sSfld, g14sSfld, g15sSfld, g16sSfld;
extern void initSfld(Sfld *p, double x);
extern void checkgSfld(void);
extern void testSfld(Sfld s1, Sfld s2, Sfld s3, Sfld s4, Sfld s5, Sfld s6, Sfld s7, Sfld s8, Sfld s9, Sfld s10, Sfld s11, Sfld s12, Sfld s13, Sfld s14, Sfld s15, Sfld s16);
extern void testvaSfld(int n, ...);
void test2_Sfld(Sfld s1, Sfld s2, Sfld s3, Sfld s4, Sfld s5, Sfld s6, Sfld s7, Sfld s8) { testSfld(s1, g2sSfld, s2, g4sSfld, s3, g6sSfld, s4, g8sSfld, s5, g10sSfld, s6, g12sSfld, s7, g14sSfld, s8, g16sSfld); }
void testitSfld(void)
{
    ;
    ;
    initSfld(&g1sSfld, (double)1);
    initSfld(&g2sSfld, (double)2);
    initSfld(&g3sSfld, (double)3);
    initSfld(&g4sSfld, (double)4);
    initSfld(&g5sSfld, (double)5);
    initSfld(&g6sSfld, (double)6);
    initSfld(&g7sSfld, (double)7);
    initSfld(&g8sSfld, (double)8);
    initSfld(&g9sSfld, (double)9);
    initSfld(&g10sSfld, (double)10);
    initSfld(&g11sSfld, (double)11);
    initSfld(&g12sSfld, (double)12);
    initSfld(&g13sSfld, (double)13);
    initSfld(&g14sSfld, (double)14);
    initSfld(&g15sSfld, (double)15);
    initSfld(&g16sSfld, (double)16);
    checkgSfld();
    ;
    ;
    ;
    testSfld(g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld, g7sSfld, g8sSfld, g9sSfld, g10sSfld, g11sSfld, g12sSfld, g13sSfld, g14sSfld, g15sSfld, g16sSfld);
    ;
    ;
    ;
    ;
    testvaSfld(1, g1sSfld);
    ;
    testvaSfld(2, g1sSfld, g2sSfld);
    ;
    testvaSfld(3, g1sSfld, g2sSfld, g3sSfld);
    ;
    testvaSfld(4, g1sSfld, g2sSfld, g3sSfld, g4sSfld);
    ;
    testvaSfld(5, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld);
    ;
    testvaSfld(6, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld);
    ;
    testvaSfld(7, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld, g7sSfld);
    ;
    testvaSfld(8, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld, g7sSfld, g8sSfld);
    ;
    testvaSfld(9, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld, g7sSfld, g8sSfld, g9sSfld);
    ;
    testvaSfld(10, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld, g7sSfld, g8sSfld, g9sSfld, g10sSfld);
    ;
    testvaSfld(11, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld, g7sSfld, g8sSfld, g9sSfld, g10sSfld, g11sSfld);
    ;
    testvaSfld(12, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld, g7sSfld, g8sSfld, g9sSfld, g10sSfld, g11sSfld, g12sSfld);
    ;
    testvaSfld(13, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld, g7sSfld, g8sSfld, g9sSfld, g10sSfld, g11sSfld, g12sSfld, g13sSfld);
    ;
    testvaSfld(14, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld, g7sSfld, g8sSfld, g9sSfld, g10sSfld, g11sSfld, g12sSfld, g13sSfld, g14sSfld);
    ;
    testvaSfld(15, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld, g7sSfld, g8sSfld, g9sSfld, g10sSfld, g11sSfld, g12sSfld, g13sSfld, g14sSfld, g15sSfld);
    ;
    testvaSfld(16, g1sSfld, g2sSfld, g3sSfld, g4sSfld, g5sSfld, g6sSfld, g7sSfld, g8sSfld, g9sSfld, g10sSfld, g11sSfld, g12sSfld, g13sSfld, g14sSfld, g15sSfld, g16sSfld);
    ;
    ;
    ;
    test2_Sfld(g1sSfld, g3sSfld, g5sSfld, g7sSfld, g9sSfld, g11sSfld, g13sSfld, g15sSfld);
    ;
}
Sdfl g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl;
Sdfl g5sSdfl, g6sSdfl, g7sSdfl, g8sSdfl;
Sdfl g9sSdfl, g10sSdfl, g11sSdfl, g12sSdfl;
Sdfl g13sSdfl, g14sSdfl, g15sSdfl, g16sSdfl;
extern void initSdfl(Sdfl *p, double x);
extern void checkgSdfl(void);
extern void testSdfl(Sdfl s1, Sdfl s2, Sdfl s3, Sdfl s4, Sdfl s5, Sdfl s6, Sdfl s7, Sdfl s8, Sdfl s9, Sdfl s10, Sdfl s11, Sdfl s12, Sdfl s13, Sdfl s14, Sdfl s15, Sdfl s16);
extern void testvaSdfl(int n, ...);
void test2_Sdfl(Sdfl s1, Sdfl s2, Sdfl s3, Sdfl s4, Sdfl s5, Sdfl s6, Sdfl s7, Sdfl s8) { testSdfl(s1, g2sSdfl, s2, g4sSdfl, s3, g6sSdfl, s4, g8sSdfl, s5, g10sSdfl, s6, g12sSdfl, s7, g14sSdfl, s8, g16sSdfl); }
void testitSdfl(void)
{
    ;
    ;
    initSdfl(&g1sSdfl, (double)1);
    initSdfl(&g2sSdfl, (double)2);
    initSdfl(&g3sSdfl, (double)3);
    initSdfl(&g4sSdfl, (double)4);
    initSdfl(&g5sSdfl, (double)5);
    initSdfl(&g6sSdfl, (double)6);
    initSdfl(&g7sSdfl, (double)7);
    initSdfl(&g8sSdfl, (double)8);
    initSdfl(&g9sSdfl, (double)9);
    initSdfl(&g10sSdfl, (double)10);
    initSdfl(&g11sSdfl, (double)11);
    initSdfl(&g12sSdfl, (double)12);
    initSdfl(&g13sSdfl, (double)13);
    initSdfl(&g14sSdfl, (double)14);
    initSdfl(&g15sSdfl, (double)15);
    initSdfl(&g16sSdfl, (double)16);
    checkgSdfl();
    ;
    ;
    ;
    testSdfl(g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl, g7sSdfl, g8sSdfl, g9sSdfl, g10sSdfl, g11sSdfl, g12sSdfl, g13sSdfl, g14sSdfl, g15sSdfl, g16sSdfl);
    ;
    ;
    ;
    ;
    testvaSdfl(1, g1sSdfl);
    ;
    testvaSdfl(2, g1sSdfl, g2sSdfl);
    ;
    testvaSdfl(3, g1sSdfl, g2sSdfl, g3sSdfl);
    ;
    testvaSdfl(4, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl);
    ;
    testvaSdfl(5, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl);
    ;
    testvaSdfl(6, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl);
    ;
    testvaSdfl(7, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl, g7sSdfl);
    ;
    testvaSdfl(8, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl, g7sSdfl, g8sSdfl);
    ;
    testvaSdfl(9, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl, g7sSdfl, g8sSdfl, g9sSdfl);
    ;
    testvaSdfl(10, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl, g7sSdfl, g8sSdfl, g9sSdfl, g10sSdfl);
    ;
    testvaSdfl(11, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl, g7sSdfl, g8sSdfl, g9sSdfl, g10sSdfl, g11sSdfl);
    ;
    testvaSdfl(12, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl, g7sSdfl, g8sSdfl, g9sSdfl, g10sSdfl, g11sSdfl, g12sSdfl);
    ;
    testvaSdfl(13, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl, g7sSdfl, g8sSdfl, g9sSdfl, g10sSdfl, g11sSdfl, g12sSdfl, g13sSdfl);
    ;
    testvaSdfl(14, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl, g7sSdfl, g8sSdfl, g9sSdfl, g10sSdfl, g11sSdfl, g12sSdfl, g13sSdfl, g14sSdfl);
    ;
    testvaSdfl(15, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl, g7sSdfl, g8sSdfl, g9sSdfl, g10sSdfl, g11sSdfl, g12sSdfl, g13sSdfl, g14sSdfl, g15sSdfl);
    ;
    testvaSdfl(16, g1sSdfl, g2sSdfl, g3sSdfl, g4sSdfl, g5sSdfl, g6sSdfl, g7sSdfl, g8sSdfl, g9sSdfl, g10sSdfl, g11sSdfl, g12sSdfl, g13sSdfl, g14sSdfl, g15sSdfl, g16sSdfl);
    ;
    ;
    ;
    test2_Sdfl(g1sSdfl, g3sSdfl, g5sSdfl, g7sSdfl, g9sSdfl, g11sSdfl, g13sSdfl, g15sSdfl);
    ;
}
Sdlf g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf;
Sdlf g5sSdlf, g6sSdlf, g7sSdlf, g8sSdlf;
Sdlf g9sSdlf, g10sSdlf, g11sSdlf, g12sSdlf;
Sdlf g13sSdlf, g14sSdlf, g15sSdlf, g16sSdlf;
extern void initSdlf(Sdlf *p, double x);
extern void checkgSdlf(void);
extern void testSdlf(Sdlf s1, Sdlf s2, Sdlf s3, Sdlf s4, Sdlf s5, Sdlf s6, Sdlf s7, Sdlf s8, Sdlf s9, Sdlf s10, Sdlf s11, Sdlf s12, Sdlf s13, Sdlf s14, Sdlf s15, Sdlf s16);
extern void testvaSdlf(int n, ...);
void test2_Sdlf(Sdlf s1, Sdlf s2, Sdlf s3, Sdlf s4, Sdlf s5, Sdlf s6, Sdlf s7, Sdlf s8) { testSdlf(s1, g2sSdlf, s2, g4sSdlf, s3, g6sSdlf, s4, g8sSdlf, s5, g10sSdlf, s6, g12sSdlf, s7, g14sSdlf, s8, g16sSdlf); }
void testitSdlf(void)
{
    ;
    ;
    initSdlf(&g1sSdlf, (double)1);
    initSdlf(&g2sSdlf, (double)2);
    initSdlf(&g3sSdlf, (double)3);
    initSdlf(&g4sSdlf, (double)4);
    initSdlf(&g5sSdlf, (double)5);
    initSdlf(&g6sSdlf, (double)6);
    initSdlf(&g7sSdlf, (double)7);
    initSdlf(&g8sSdlf, (double)8);
    initSdlf(&g9sSdlf, (double)9);
    initSdlf(&g10sSdlf, (double)10);
    initSdlf(&g11sSdlf, (double)11);
    initSdlf(&g12sSdlf, (double)12);
    initSdlf(&g13sSdlf, (double)13);
    initSdlf(&g14sSdlf, (double)14);
    initSdlf(&g15sSdlf, (double)15);
    initSdlf(&g16sSdlf, (double)16);
    checkgSdlf();
    ;
    ;
    ;
    testSdlf(g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf, g7sSdlf, g8sSdlf, g9sSdlf, g10sSdlf, g11sSdlf, g12sSdlf, g13sSdlf, g14sSdlf, g15sSdlf, g16sSdlf);
    ;
    ;
    ;
    ;
    testvaSdlf(1, g1sSdlf);
    ;
    testvaSdlf(2, g1sSdlf, g2sSdlf);
    ;
    testvaSdlf(3, g1sSdlf, g2sSdlf, g3sSdlf);
    ;
    testvaSdlf(4, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf);
    ;
    testvaSdlf(5, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf);
    ;
    testvaSdlf(6, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf);
    ;
    testvaSdlf(7, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf, g7sSdlf);
    ;
    testvaSdlf(8, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf, g7sSdlf, g8sSdlf);
    ;
    testvaSdlf(9, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf, g7sSdlf, g8sSdlf, g9sSdlf);
    ;
    testvaSdlf(10, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf, g7sSdlf, g8sSdlf, g9sSdlf, g10sSdlf);
    ;
    testvaSdlf(11, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf, g7sSdlf, g8sSdlf, g9sSdlf, g10sSdlf, g11sSdlf);
    ;
    testvaSdlf(12, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf, g7sSdlf, g8sSdlf, g9sSdlf, g10sSdlf, g11sSdlf, g12sSdlf);
    ;
    testvaSdlf(13, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf, g7sSdlf, g8sSdlf, g9sSdlf, g10sSdlf, g11sSdlf, g12sSdlf, g13sSdlf);
    ;
    testvaSdlf(14, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf, g7sSdlf, g8sSdlf, g9sSdlf, g10sSdlf, g11sSdlf, g12sSdlf, g13sSdlf, g14sSdlf);
    ;
    testvaSdlf(15, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf, g7sSdlf, g8sSdlf, g9sSdlf, g10sSdlf, g11sSdlf, g12sSdlf, g13sSdlf, g14sSdlf, g15sSdlf);
    ;
    testvaSdlf(16, g1sSdlf, g2sSdlf, g3sSdlf, g4sSdlf, g5sSdlf, g6sSdlf, g7sSdlf, g8sSdlf, g9sSdlf, g10sSdlf, g11sSdlf, g12sSdlf, g13sSdlf, g14sSdlf, g15sSdlf, g16sSdlf);
    ;
    ;
    ;
    test2_Sdlf(g1sSdlf, g3sSdlf, g5sSdlf, g7sSdlf, g9sSdlf, g11sSdlf, g13sSdlf, g15sSdlf);
    ;
}
Slfd g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd;
Slfd g5sSlfd, g6sSlfd, g7sSlfd, g8sSlfd;
Slfd g9sSlfd, g10sSlfd, g11sSlfd, g12sSlfd;
Slfd g13sSlfd, g14sSlfd, g15sSlfd, g16sSlfd;
extern void initSlfd(Slfd *p, double x);
extern void checkgSlfd(void);
extern void testSlfd(Slfd s1, Slfd s2, Slfd s3, Slfd s4, Slfd s5, Slfd s6, Slfd s7, Slfd s8, Slfd s9, Slfd s10, Slfd s11, Slfd s12, Slfd s13, Slfd s14, Slfd s15, Slfd s16);
extern void testvaSlfd(int n, ...);
void test2_Slfd(Slfd s1, Slfd s2, Slfd s3, Slfd s4, Slfd s5, Slfd s6, Slfd s7, Slfd s8) { testSlfd(s1, g2sSlfd, s2, g4sSlfd, s3, g6sSlfd, s4, g8sSlfd, s5, g10sSlfd, s6, g12sSlfd, s7, g14sSlfd, s8, g16sSlfd); }
void testitSlfd(void)
{
    ;
    ;
    initSlfd(&g1sSlfd, (double)1);
    initSlfd(&g2sSlfd, (double)2);
    initSlfd(&g3sSlfd, (double)3);
    initSlfd(&g4sSlfd, (double)4);
    initSlfd(&g5sSlfd, (double)5);
    initSlfd(&g6sSlfd, (double)6);
    initSlfd(&g7sSlfd, (double)7);
    initSlfd(&g8sSlfd, (double)8);
    initSlfd(&g9sSlfd, (double)9);
    initSlfd(&g10sSlfd, (double)10);
    initSlfd(&g11sSlfd, (double)11);
    initSlfd(&g12sSlfd, (double)12);
    initSlfd(&g13sSlfd, (double)13);
    initSlfd(&g14sSlfd, (double)14);
    initSlfd(&g15sSlfd, (double)15);
    initSlfd(&g16sSlfd, (double)16);
    checkgSlfd();
    ;
    ;
    ;
    testSlfd(g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd, g7sSlfd, g8sSlfd, g9sSlfd, g10sSlfd, g11sSlfd, g12sSlfd, g13sSlfd, g14sSlfd, g15sSlfd, g16sSlfd);
    ;
    ;
    ;
    ;
    testvaSlfd(1, g1sSlfd);
    ;
    testvaSlfd(2, g1sSlfd, g2sSlfd);
    ;
    testvaSlfd(3, g1sSlfd, g2sSlfd, g3sSlfd);
    ;
    testvaSlfd(4, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd);
    ;
    testvaSlfd(5, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd);
    ;
    testvaSlfd(6, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd);
    ;
    testvaSlfd(7, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd, g7sSlfd);
    ;
    testvaSlfd(8, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd, g7sSlfd, g8sSlfd);
    ;
    testvaSlfd(9, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd, g7sSlfd, g8sSlfd, g9sSlfd);
    ;
    testvaSlfd(10, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd, g7sSlfd, g8sSlfd, g9sSlfd, g10sSlfd);
    ;
    testvaSlfd(11, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd, g7sSlfd, g8sSlfd, g9sSlfd, g10sSlfd, g11sSlfd);
    ;
    testvaSlfd(12, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd, g7sSlfd, g8sSlfd, g9sSlfd, g10sSlfd, g11sSlfd, g12sSlfd);
    ;
    testvaSlfd(13, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd, g7sSlfd, g8sSlfd, g9sSlfd, g10sSlfd, g11sSlfd, g12sSlfd, g13sSlfd);
    ;
    testvaSlfd(14, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd, g7sSlfd, g8sSlfd, g9sSlfd, g10sSlfd, g11sSlfd, g12sSlfd, g13sSlfd, g14sSlfd);
    ;
    testvaSlfd(15, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd, g7sSlfd, g8sSlfd, g9sSlfd, g10sSlfd, g11sSlfd, g12sSlfd, g13sSlfd, g14sSlfd, g15sSlfd);
    ;
    testvaSlfd(16, g1sSlfd, g2sSlfd, g3sSlfd, g4sSlfd, g5sSlfd, g6sSlfd, g7sSlfd, g8sSlfd, g9sSlfd, g10sSlfd, g11sSlfd, g12sSlfd, g13sSlfd, g14sSlfd, g15sSlfd, g16sSlfd);
    ;
    ;
    ;
    test2_Slfd(g1sSlfd, g3sSlfd, g5sSlfd, g7sSlfd, g9sSlfd, g11sSlfd, g13sSlfd, g15sSlfd);
    ;
}
Sldf g1sSldf, g2sSldf, g3sSldf, g4sSldf;
Sldf g5sSldf, g6sSldf, g7sSldf, g8sSldf;
Sldf g9sSldf, g10sSldf, g11sSldf, g12sSldf;
Sldf g13sSldf, g14sSldf, g15sSldf, g16sSldf;
extern void initSldf(Sldf *p, double x);
extern void checkgSldf(void);
extern void testSldf(Sldf s1, Sldf s2, Sldf s3, Sldf s4, Sldf s5, Sldf s6, Sldf s7, Sldf s8, Sldf s9, Sldf s10, Sldf s11, Sldf s12, Sldf s13, Sldf s14, Sldf s15, Sldf s16);
extern void testvaSldf(int n, ...);
void test2_Sldf(Sldf s1, Sldf s2, Sldf s3, Sldf s4, Sldf s5, Sldf s6, Sldf s7, Sldf s8) { testSldf(s1, g2sSldf, s2, g4sSldf, s3, g6sSldf, s4, g8sSldf, s5, g10sSldf, s6, g12sSldf, s7, g14sSldf, s8, g16sSldf); }
void testitSldf(void)
{
    ;
    ;
    initSldf(&g1sSldf, (double)1);
    initSldf(&g2sSldf, (double)2);
    initSldf(&g3sSldf, (double)3);
    initSldf(&g4sSldf, (double)4);
    initSldf(&g5sSldf, (double)5);
    initSldf(&g6sSldf, (double)6);
    initSldf(&g7sSldf, (double)7);
    initSldf(&g8sSldf, (double)8);
    initSldf(&g9sSldf, (double)9);
    initSldf(&g10sSldf, (double)10);
    initSldf(&g11sSldf, (double)11);
    initSldf(&g12sSldf, (double)12);
    initSldf(&g13sSldf, (double)13);
    initSldf(&g14sSldf, (double)14);
    initSldf(&g15sSldf, (double)15);
    initSldf(&g16sSldf, (double)16);
    checkgSldf();
    ;
    ;
    ;
    testSldf(g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf, g7sSldf, g8sSldf, g9sSldf, g10sSldf, g11sSldf, g12sSldf, g13sSldf, g14sSldf, g15sSldf, g16sSldf);
    ;
    ;
    ;
    ;
    testvaSldf(1, g1sSldf);
    ;
    testvaSldf(2, g1sSldf, g2sSldf);
    ;
    testvaSldf(3, g1sSldf, g2sSldf, g3sSldf);
    ;
    testvaSldf(4, g1sSldf, g2sSldf, g3sSldf, g4sSldf);
    ;
    testvaSldf(5, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf);
    ;
    testvaSldf(6, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf);
    ;
    testvaSldf(7, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf, g7sSldf);
    ;
    testvaSldf(8, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf, g7sSldf, g8sSldf);
    ;
    testvaSldf(9, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf, g7sSldf, g8sSldf, g9sSldf);
    ;
    testvaSldf(10, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf, g7sSldf, g8sSldf, g9sSldf, g10sSldf);
    ;
    testvaSldf(11, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf, g7sSldf, g8sSldf, g9sSldf, g10sSldf, g11sSldf);
    ;
    testvaSldf(12, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf, g7sSldf, g8sSldf, g9sSldf, g10sSldf, g11sSldf, g12sSldf);
    ;
    testvaSldf(13, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf, g7sSldf, g8sSldf, g9sSldf, g10sSldf, g11sSldf, g12sSldf, g13sSldf);
    ;
    testvaSldf(14, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf, g7sSldf, g8sSldf, g9sSldf, g10sSldf, g11sSldf, g12sSldf, g13sSldf, g14sSldf);
    ;
    testvaSldf(15, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf, g7sSldf, g8sSldf, g9sSldf, g10sSldf, g11sSldf, g12sSldf, g13sSldf, g14sSldf, g15sSldf);
    ;
    testvaSldf(16, g1sSldf, g2sSldf, g3sSldf, g4sSldf, g5sSldf, g6sSldf, g7sSldf, g8sSldf, g9sSldf, g10sSldf, g11sSldf, g12sSldf, g13sSldf, g14sSldf, g15sSldf, g16sSldf);
    ;
    ;
    ;
    test2_Sldf(g1sSldf, g3sSldf, g5sSldf, g7sSldf, g9sSldf, g11sSldf, g13sSldf, g15sSldf);
    ;
}
void struct_by_value_10_x()
{
    testitSfd();
    testitSfl();
    testitSdf();
    testitSdl();
    testitSlf();
    testitSld();
    testitSfdl();
    testitSfld();
    testitSdfl();
    testitSdlf();
    testitSlfd();
    testitSldf();
    if (fails != 0)
        abort();
}