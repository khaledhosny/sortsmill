/*
 * Copyright (C) 2012 Barry Schwartz
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILLFF_ATTRIBUTES_H
#define _SORTSMILLFF_ATTRIBUTES_H

#define _FF_GNUC_VERSION_AT_LEAST(MAJOR, MINOR)				\
  (defined __GNUC__ && defined __GNUC_MINOR__ &&			\
   (MAJOR < __GNUC__ || (MAJOR == __GNUC__ && MINOR <= __GNUC_MINOR__)))

#if !defined _FF_ATTRIBUTE_PURE
#if _FF_GNUC_VERSION_AT_LEAST (2, 96)
#define _FF_ATTRIBUTE_PURE __attribute__ ((__pure__))
#else
#define _FF_ATTRIBUTE_PURE      /* empty */
#endif
#endif

#if !defined _FF_ATTRIBUTE_CONST
#if _FF_GNUC_VERSION_AT_LEAST (2, 5)
#define _FF_ATTRIBUTE_CONST __attribute__ ((__const__))
#else
#define _FF_ATTRIBUTE_CONST     /* empty */
#endif
#endif

#if !defined _FF_ATTRIBUTE_HOT
#if _FF_GNUC_VERSION_AT_LEAST (4, 3)
#define _FF_ATTRIBUTE_HOT __attribute__ ((__hot__))
#else
#define _FF_ATTRIBUTE_HOT       /* empty */
#endif
#endif

#if !defined _FF_ATTRIBUTE_COLD
#if _FF_GNUC_VERSION_AT_LEAST (4, 3)
#define _FF_ATTRIBUTE_COLD __attribute__ ((__cold__))
#else
#define _FF_ATTRIBUTE_COLD      /* empty */
#endif
#endif

#if !defined _FF_ATTRIBUTE_DEPRECATED
#if defined __GNUC__
#define _FF_ATTRIBUTE_DEPRECATED __attribute__ ((__deprecated__))
#else
#define _FF_ATTRIBUTE_DEPRECATED        /* empty */
#endif
#endif

#if !defined _FF_ATTRIBUTE_SENTINEL
#if defined __GNUC__
#define _FF_ATTRIBUTE_SENTINEL __attribute__ ((__sentinel__))
#else
#define _FF_ATTRIBUTE_SENTINEL  /* empty */
#endif
#endif

#if !defined _FF_ATTRIBUTE_WARN_UNUSED_RESULT
#if defined __GNUC__
#define _FF_ATTRIBUTE_WARN_UNUSED_RESULT __attribute__ ((__warn_unused_result__))
#else
#define _FF_ATTRIBUTE_WARN_UNUSED_RESULT        /* empty */
#endif
#endif

#if !defined _FF_ATTRIBUTE_NORETURN
#if _FF_GNUC_VERSION_AT_LEAST (2, 5)
#define _FF_ATTRIBUTE_NORETURN __attribute__ ((__noreturn__))
#else
#define _FF_ATTRIBUTE_NORETURN  /* empty */
#endif
#endif

#if !defined _FF_ATTRIBUTE_NOINLINE
#if defined __GNUC__
#define _FF_ATTRIBUTE_NOINLINE __attribute__ ((__noinline__))
#else
#define _FF_ATTRIBUTE_NOINLINE  /* empty */
#endif
#endif

#if !defined _FF_ATTRIBUTE_ALWAYS_INLINE
#if defined __GNUC__
#define _FF_ATTRIBUTE_ALWAYS_INLINE __attribute__ ((__always_inline__))
#else
#define _FF_ATTRIBUTE_ALWAYS_INLINE     /* empty */
#endif
#endif

#if !defined _FF_ATTRIBUTE_FLATTEN
#if defined __GNUC__
#define _FF_ATTRIBUTE_FLATTEN __attribute__ ((__flatten__))
#else
#define _FF_ATTRIBUTE_FLATTEN   /* empty */
#endif
#endif

#if !defined _FF_ATTRIBUTE_MALLOC
#if defined __GNUC__
#define _FF_ATTRIBUTE_MALLOC __attribute__ ((__malloc__))
#else
#define _FF_ATTRIBUTE_MALLOC    /* empty */
#endif
#endif

#if !defined _FF_UNUSED
#if _FF_GNUC_VERSION_AT_LEAST (2, 7)
#define _FF_UNUSED(x) x __attribute__ ((__unused__))
#elif defined __LCLINT__
#define _FF_UNUSED(x) /*@unused@*/ x
#else
#define _FF_UNUSED(x) x
#endif
#endif

#if _FF_GNUC_VERSION_AT_LEAST (2, 0) || 0x5110 <= __SUNPRO_C
#if !defined _FF_HAVE_TYPEOF
#define _FF_HAVE_TYPEOF 1
#endif
#if !defined _FF_TYPEOF
#define _FF_TYPEOF(x) __typeof__ (x)
#endif
#if !defined _FF_CAST_TYPEOF
#define _FF_CAST_TYPEOF(x) (__typeof__ (x))
#endif
#else /* typeof not supported */
#if !defined _FF_HAVE_TYPEOF
#define _FF_HAVE_TYPEOF 0
#endif
#if !defined _FF_TYPEOF
#define _FF_TYPEOF(x)           /* empty */
#endif
#if !defined _FF_CAST_TYPEOF
#define _FF_CAST_TYPEOF(x)      /* empty */
#endif
#endif

#endif /* _SORTSMILLFF_ATTRIBUTES_H */
