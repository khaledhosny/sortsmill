#include <config.h>

// Copyright (C) 2012 Barry Schwartz
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <libguile.h>

// // Export symbols that are very unlikely to conflict, taking advantage
// // of Guile’s dependence on ltdl. See "Using libltdl" in the libtool
// // manual to learn how this method works.
// //
// // We assume these very long symbol names can be handled.
// //
// #define init_guile_sortsmillff_iconv				\
//   libguile_sortsmillff_iconv_LTX_init_guile_sortsmillff_iconv
// #define scm_embedded_utf7_to_string				\
//   libguile_sortsmillff_iconv_LTX_scm_embedded_utf7_to_string
// 
// VISIBLE void init_guile_sortsmillff_iconv (void);
// VISIBLE SCM scm_embedded_utf7_to_string (SCM str);
// 
// SCM
// scm_embedded_utf7_to_string (SCM str)
// {
//   size_t utf7_length;
//   size_t utf8_length;
//   char *utf8;
// 
//   scm_dynwind_begin (0);
// 
//   // Treat the string as UTF-7 embedded in ASCII/UTF-8.
//   char *utf7 = scm_to_utf8_stringn (str, &utf7_length);
//   scm_dynwind_free (utf7);
// 
//   // I doubt the MY_ICONV_SUFFIX_TRANSLIT_STRING here has any effect,
//   // but it should do no harm, either, and demonstrates how to use it.
//   iconv_t descriptor =
//     iconv_open (MY_ICONV_UTF8_STRING MY_ICONV_SUFFIX_TRANSLIT_STRING,
//                 MY_ICONV_UTF7_STRING);
// 
//   if (descriptor != (iconv_t) (-1))
//     {
//       size_t utf8_bufsize = utf7_length * 6;    /* FIXME: What is a good
//                                                    size here? */
//       utf8 = scm_gc_malloc_pointerless (utf8_bufsize * sizeof (char),
//                                         "string");
// 
//       // iconv(3) modifies these pointers and integers.
//       char *utf7_p = utf7;
//       char *utf8_p = utf8;
//       size_t utf7_n = utf7_length;
//       size_t utf8_n = utf8_bufsize;
// 
//       (void) iconv (descriptor, &utf7_p, &utf7_n, &utf8_p, &utf8_n);
//       utf8_length = utf8_p - utf8;
// 
//       iconv_close (descriptor);
//     }
//   else
//     {
//       utf8 = scm_gc_malloc_pointerless (utf7_length * sizeof (char),
//                                         "string");
//       memcpy (utf8, utf7, utf7_length * sizeof (char));
//       utf8_length = utf7_length;
//     }
// 
//   SCM result = scm_from_utf8_stringn (utf8, utf8_length);
// 
//   scm_dynwind_end ();
// 
//   return result;
// }
// 
// void
// init_guile_sortsmillff_iconv (void)
// {
//   (void) scm_c_define_gsubr ("embedded-utf7->string", 1, 0, 0,
//                              scm_embedded_utf7_to_string);
// }
