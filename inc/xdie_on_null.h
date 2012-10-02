/*
 * Copyright (C) 2000-2012 by Barry Schwartz
  
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

#ifndef _XDIE_ON_NULL_H
#define _XDIE_ON_NULL_H

#include <config.h>

#include <assert.h>
#include <stddef.h>
#include <errno.h>
#include <xalloc.h>

inline void *xdie_on_null (void *p);
inline void *xdie_on_enomem (void *p);

inline void *
xdie_on_null (void *p)
{
  if (p == NULL)
    xalloc_die ();
  return p;
}

inline void *
xdie_on_enomem (void *p)
{
  if (p == NULL && errno == ENOMEM)
    xalloc_die ();
  return p;
}

// The macro XDIE_ON_NULL tries to avoid implicit type-casting between
// the type of p and (void *). This works with gcc, in particular.
//
#ifdef HAVE_TYPEOF
#define XDIE_ON_NULL(p) ((typeof (p)) xdie_on_null ((void *) (p)))
#else
#define XDIE_ON_NULL xdie_on_null
#endif

// The macro XDIE_ON_ENOMEM tries to avoid implicit type-casting
// between the type of p and (void *). This works with gcc, in
// particular.
//
#ifdef HAVE_TYPEOF
#define XDIE_ON_ENOMEM(p) ((typeof (p)) xdie_on_enomem ((void *) (p)))
#else
#define XDIE_ON_ENOMEM(p) xdie_on_enomem
#endif

#endif // _XDIE_ON_NULL_H
