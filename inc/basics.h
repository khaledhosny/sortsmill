/* Copyright (C) 2000-2012 by George Williams */
/*
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.

 * The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#ifndef _BASICS_H
#define _BASICS_H

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>

// FIXME: Use the standard names in the code; get rid of these
// typedefs.
typedef int32_t		int32;
typedef uint32_t	uint32;
typedef int16_t		int16;
typedef uint16_t	uint16;
typedef int8_t		int8;
typedef uint8_t		uint8;

// FIXME: Use the standard names in the code; get rid of this
// typedef, too.
//
// An integral type which can hold a pointer.
typedef intptr_t	intpt;

typedef uint32_t unichar_t;

// FIXME: Consider using the unused-parameter snippet from Gnulib.
//
// A macro to mark unused function parameters with. We often
// have such parameters, because of extensive use of callbacks.
#ifdef UNUSED
#elif defined(__GNUC__)
# define UNUSED(x) UNUSED_ ## x __attribute__((unused))
#elif defined(__LCLINT__)
# define UNUSED(x) /*@unused@*/ x
#else
# define UNUSED(x) x
#endif

extern void *galloc(size_t size);
extern void *gcalloc(size_t cnt, size_t size);
extern void *grealloc(void *,size_t size);
extern void gfree(void *);
extern char *copy(const char *);
extern char *copyn(const char *, size_t);

static inline int imin(int a, int b)
{
    return (a < b) ? a : b;
}

static inline int imax(int a, int b)
{
    return (a < b) ? b : a;
}

#endif

