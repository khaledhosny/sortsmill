/*
 * Copyright (C) 2013 Khaled Hosny and Barry Schwartz
 * This file is part of the Sorts Mill Tools.
 * 
 * Sorts Mill Tools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Sorts Mill Tools is distributed in the hope that it will be useful,
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

/* Font formats. */

SCM scm_symbol__pfa (void);     /* pfa */
SCM scm_symbol__pfb (void);     /* pfb */
SCM scm_symbol__pfb_macbin (void);      /* pfb-macbin */
SCM scm_symbol__pfb_multiple (void);    /* pfb-multiple */
SCM scm_symbol__mma (void);     /* mma */
SCM scm_symbol__mmb (void);     /* mmb */
SCM scm_symbol__type3 (void);   /* type3 */
SCM scm_symbol__type0 (void);   /* type0 */
SCM scm_symbol__cid (void);     /* cid */
SCM scm_symbol__cff (void);     /* cff */
SCM scm_symbol__cff_cid (void); /* cff-cid */
SCM scm_symbol__type42 (void);  /* type42 */
SCM scm_symbol__type11 (void);  /* type11 */// ‘Type 42 CID’.
SCM scm_symbol__ttf (void);     /* ttf */
SCM scm_symbol__ttf_symbol (void);      /* ttf-symbol */
SCM scm_symbol__ttf_macbin (void);      /* ttf-macbin */
SCM scm_symbol__ttc (void);     /* ttc */
SCM scm_symbol__ttf_dfont (void);       /* ttf-dfont */
SCM scm_symbol__otf (void);     /* otf */
SCM scm_symbol__otf_dfont (void);       /* otf-dfont */
SCM scm_symbol__otf_cid (void); /* otf-cid */
SCM scm_symbol__otf_cid_dfont (void);   /* otf-cid-dfont */
SCM scm_symbol__ufo (void);     /* ufo */
SCM scm_symbol__woff (void);    /* woff */
SCM scm_symbol__no_font (void); /* no-font */

/*-----------------------------------------------------------------------*/

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_SYMBOLS_H */
