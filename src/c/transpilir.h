#ifndef TRANSPILIR
#define TRANSPILIR

#include <stdarg.h>
#include <stdint.h>

// Types

#define I8 int8_t
#define U8 uint8_t
#define I16 int16_t
#define U16 uint16_t
#define I32 int32_t
#define U32 uint32_t
#define I64 int64_t
#define U64 uint64_t
#define F32 float
#define F64 double
#define SIZE_T U64

// Floating-point

#if defined(__GNUC__) || defined(__clang__)
#define NAN __builtin_nanf("")
#define INFINITY __builtin_inff()
#else
#define NAN (0.0f/0.0f)
#define INFINITY 1e5000f
#endif

#if defined(__unix__) || defined(__APPLE__)
extern int isnan(double v);
#define IS_NAN(v) isnan(v)
#else
#define IS_NAN(v) (v) != (v)
#endif

#define NOT_NAN(l, r) (!IS_NAN(l) && !IS_NAN(r))
#define ANY_NAN(l, r) (IS_NAN(l) || IS_NAN(r))

// Linkage

#if defined(__GNUC__) || defined(__clang__)
#define TAG(label) __asm__(label)
#else
#define TAG(label)
#endif

#define LOCAL static
#define EXPORT

#ifdef __STDC_NO_THREADS__
#define THREAD
#else
#define THREAD _Thread_local
#endif

#if defined(__GNUC__) || defined(__clang__)
#define LINK(sec) __attribute__((section(sec)))
#else
#define LINK(sec)
#endif

#define LINK_FLAGS(sec, flag) LINK(sec)

// Variable Arguments

#define VA_START(ap, paramN) va_start(*(va_list*)(ap), paramN)
#define VA_ARG(T, ap) va_arg(*(va_list*)(ap), T)

// Memory

#if defined(__GNUC__) || defined(__clang__)
#define ALIGN(n) __attribute__((aligned(n)))
#else
#define ALIGN(n)
#endif

#define ALIGN_DEFAULT ALIGN(1)

#if defined(__GNUC__) || defined(__clang__)
#define BLIT(src, dst, n) __builtin_memcpy(dst, src, n)
#else
#define BLIT(src, dst, n) for (int __i = 0; __i < (n); __i++) ((char *)(dst))[__i] = ((char *)(src))[__i];
#endif

// Flow

#if defined(__GNUC__) || defined(__clang__)
#define HALT() __builtin_unreachable()
#else
#define HALT()
#endif

#endif

