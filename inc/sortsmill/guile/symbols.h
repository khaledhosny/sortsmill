/*
 * Copyright (C) 2013 Barry Schwartz
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

#ifndef _SORTSMILL_GUILE_FONTS_SYMBOLS_H
#define _SORTSMILL_GUILE_FONTS_SYMBOLS_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/*-----------------------------------------------------------------------*/

/* Symbols having widespread applications. */

SCM scm_symbol__coords (void);  /* coords */
SCM scm_symbol__name (void);    /* name */
SCM scm_symbol__type (void);    /* type */

SCM scm_symbol__left (void);    /* left */
SCM scm_symbol__right (void);   /* right */

SCM scm_symbol__all (void);     /* all */

/*-----------------------------------------------------------------------*/

/* Glyph layers. */

SCM scm_symbol__guide (void);   /* guide */

/*-----------------------------------------------------------------------*/

/* OpenType lookups. */

SCM scm_symbol__subtable_name (void);   /* subtable_name */
SCM scm_symbol__lookup_name (void);     /* lookup_name */
SCM scm_symbol__lookup_type (void);     /* lookup_type */

SCM scm_symbol__gsub_single (void);     /* gsub-single */
SCM scm_symbol__gsub_multiple (void);   /* gsub-multiple */
SCM scm_symbol__gsub_alternate (void);  /* gsub-alternate */
SCM scm_symbol__gsub_ligature (void);   /* gsub-ligature */
SCM scm_symbol__gsub_context (void);    /* gsub-context */
SCM scm_symbol__gsub_chaining_contextual (void);        /* gsub-chaining-contextual */
SCM scm_symbol__gsub_extension (void);  /* gsub-extension */
SCM scm_symbol__gsub_reverse_chaining_contextual (void);        /* gsub-reverse-chaining-contextual */
SCM scm_symbol__gpos_single (void);     /* gpos-single */
SCM scm_symbol__gpos_pair (void);       /* gpos-pair */
SCM scm_symbol__gpos_cursive (void);    /* gpos-cursive */
SCM scm_symbol__gpos_mark_to_base (void);       /* gpos-mark-to-base */
SCM scm_symbol__gpos_mark_to_ligature (void);   /* gpos-mark-to-ligature */
SCM scm_symbol__gpos_mark_to_mark (void);       /* gpos-mark-to-mark */
SCM scm_symbol__gpos_context (void);    /* gpos-context */
SCM scm_symbol__gpos_chaining_contextual (void);        /* gpos-chaining-contextual */
SCM scm_symbol__gpos_extension (void);  /* gpos-extension */

/*-----------------------------------------------------------------------*/

/* OpenType anchors. */

SCM scm_symbol__mark (void);    /* mark */
SCM scm_symbol__base (void);    /* base */
SCM scm_symbol__ligature (void);        /* ligature */
SCM scm_symbol__base_mark (void);       /* base-mark */
SCM scm_symbol__entry (void);   /* entry */
SCM scm_symbol__exit (void);    /* exit */

/*-----------------------------------------------------------------------*/

/* Peg spacing. */

SCM scm_symbol__kerning_only (void);    /* kerning-only */
SCM scm_symbol__special (void); /* special */

/*-----------------------------------------------------------------------*/

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_SYMBOLS_H */
