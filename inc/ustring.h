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

#ifndef _FONTFORGE_AUX_USTRING_H
#define _FONTFORGE_AUX_USTRING_H

#include <config.h>

#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <memory.h>
#include <basics.h>
#include <xunistring.h>

VISIBLE extern long uc_strmatch (const uint32_t *, const char *);
VISIBLE extern long uc_strnmatch (const uint32_t *, const char *, int);
VISIBLE extern void cu_strncpy (char *to, const uint32_t *from, int len);
VISIBLE extern void uc_strncpy (uint32_t *to, const char *from, int len);
VISIBLE extern void uc_strncat (uint32_t *, const char *, int len);
VISIBLE extern void cu_strncat (char *, const uint32_t *, int len);
VISIBLE extern uint32_t *uc_strstr (const uint32_t *, const char *);
VISIBLE extern uint32_t *uc_strstrmatch (const uint32_t *, const char *);

VISIBLE extern uint32_t *u_strstartmatch (const uint32_t *initial,
                                   const uint32_t *full);
VISIBLE extern uint32_t *cu_strstartmatch (const char *initial,
                                    const uint32_t *full);

#define utf82u_strncpy utf82U_strncpy
VISIBLE extern int32_t utf8_ildb (const char **utf8_text);
VISIBLE extern char *utf8_idpb (char *utf8_text, uint32_t ch);
VISIBLE extern char *utf8_db (char *utf8_text);
VISIBLE extern char *utf8_ib (char *utf8_text);
VISIBLE extern char *latin1_2_utf8_strcpy (char *utf8buf, const char *lbuf);
VISIBLE extern char *latin1_2_utf8_copy (const char *lbuf);
VISIBLE extern char *utf8_2_latin1_copy (const char *utf8buf);
VISIBLE extern int utf82u_strlen (const char *utf8_str);        /* how many long would this be in shorts (UCS2) */
VISIBLE extern void utf8_strncpy (register char *to, const char *from, int len);        /* copy n characters NOT bytes */
VISIBLE extern char *def2utf8_copy (const char *from);
VISIBLE extern char *utf82def_copy (const char *ufrom);
VISIBLE extern char *utf8_strchr (const char *utf8_str, int search_char);

VISIBLE extern uint32_t *utf82u_strncpy (uint32_t *ubuf, const char *utf8buf,
                                  int len);
VISIBLE extern uint32_t *utf82u_strcpy (uint32_t *ubuf, const char *utf8buf);
VISIBLE extern void utf82u_strcat (uint32_t *ubuf, const char *utf8buf);
VISIBLE extern uint32_t *utf82u_copyn (const char *utf8buf, int len);
VISIBLE extern uint32_t *utf82u_copy (const char *utf8buf);
VISIBLE extern char *u2utf8_strcpy (char *utf8buf, const uint32_t *ubuf);
VISIBLE extern char *u2utf8_copy (const uint32_t *ubuf);
VISIBLE extern char *u2utf8_copyn (const uint32_t *ubuf, int len);
VISIBLE extern char *u2def_strncpy (char *to, const uint32_t *ufrom, int n);
VISIBLE extern char *u2def_copy (const uint32_t *ufrom);

VISIBLE extern int uAllAscii (const uint32_t *str);
VISIBLE extern int AllAscii (const char *);
VISIBLE extern char *StripToASCII (const char *utf8_str);

#endif
