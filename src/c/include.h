#include <stdarg.h>

#define SIZE_T unsigned long

#define LINK(sec) __attribute__((section(sec)))
#define LINK_FLAGS(sec, flag) LINK(sec)
#define EXPORT
#define THREAD

#define VA_START(ap, paramN) va_start(*(va_list*)(ap), paramN)
#define VA_ARG(T, ap) va_arg(*(va_list*)(ap), T)

#define ALIGN(n) __attribute__((aligned(n)))
#define ALIGN_DEFAULT ALIGN(1)

#if defined(__unix__) || defined(__APPLE__) || defined(__llvm__) || defined(__clang__)
extern void *alloca(SIZE_T n);
#define ALLOCATE(align, n) alloca(n)
#else
#define ALLOCATE(align, n) #error "Unsuppored"
#endif

#if defined(__unix__) || defined(__APPLE__)
extern void *memcpy(void *dest, const void *src, SIZE_T n);
#define BLIT(dest, src, n) memcpy(dest, src, n)
#else
#define BLIT(dest, src, n) for (int __i = 0; __i < (n); __i++) ((char *)(dest))[__i] = ((char *)(src))[__i]
#endif

#if defined(__unix__) || defined(__APPLE__)
extern int isnan(double v);
#define IS_NAN(v) isnan(v)
#else
#define IS_NAN(v) (v) != (v)
#endif
#define ANY_NAN(l, r) (IS_NAN(l) || IS_NAN(r))
#define ALL_NAN(l, r) (IS_NAN(l) && IS_NAN(r))

