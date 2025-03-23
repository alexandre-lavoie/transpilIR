#include <math.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#define LINK(sec) __attribute__((section(sec)))
#define LINK_FLAGS(sec, flag) LINK(sec)
#define EXPORT
#define THREAD 

#define VA_START(ap, paramN) va_start(*(va_list*)(ap), paramN)
#define VA_ARG(T, ap) va_arg(*(va_list*)(ap), T)

#define ALIGN(n) __attribute__((aligned(n)))
#define ALIGN_DEFAULT ALIGN(1)
#define ALLOCATE(align, size) alloca(size)
#define BLIT(dest, src, n) memcpy(dest, src, n)

#define IS_NAN(v) isnan(v)
#define ANY_NAN(l, r) (IS_NAN(l) || IS_NAN(r))
#define ALL_NAN(l, r) (IS_NAN(l) && IS_NAN(r))

