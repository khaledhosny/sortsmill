#include <config.h>

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <sortsmill/guile/symbols.h>
#include <sortsmill/guile/initialized_global_constants.h>

//-------------------------------------------------------------------------
//
// Symbols having widespread applications.

SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__coords, "coords");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__name, "name");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__type, "type");

SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__left, "left");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__right, "right");

SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__all, "all");

//-------------------------------------------------------------------------
//
// Glyph layers.

SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__guide, "guide");

//-------------------------------------------------------------------------
//
// OpenType lookups.

SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__subtable_name, "subtable-name");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__lookup_name, "lookup-name");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__lookup_type, "lookup-type");

SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gsub_single, "gsub-single");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gsub_multiple, "gsub-multiple");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gsub_alternate, "gsub-alternate");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gsub_ligature, "gsub-ligature");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gsub_context, "gsub-context");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gsub_chaining_contextual,
                     "gsub-chaining-contextual");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gsub_extension, "gsub-extension");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gsub_reverse_chaining_contextual,
                     "gsub-reverse-chaining-contextual");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gpos_single, "gpos-single");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gpos_pair, "gpos-pair");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gpos_cursive, "gpos-cursive");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gpos_mark_to_base,
                     "gpos-mark-to-base");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gpos_mark_to_ligature,
                     "gpos-mark-to-ligature");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gpos_mark_to_mark,
                     "gpos-mark-to-mark");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gpos_context, "gpos-context");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gpos_chaining_contextual,
                     "gpos-chaining-contextual");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__gpos_extension, "gpos-extension");

//-------------------------------------------------------------------------
//
// OpenType anchors.

SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__mark, "mark");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__base, "base");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__ligature, "ligature");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__base_mark, "base-mark");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__entry, "entry");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__exit, "exit");

//-------------------------------------------------------------------------
//
// Peg spacing.

SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__kerning_only, "kerning-only");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__special, "special");

//-------------------------------------------------------------------------
//
// Font formats.

SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__pfa, "pfa");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__pfb, "pfb");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__pfb_macbin, "pfb-macbin");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__pfb_multiple, "pfb-multiple");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__mma, "mma");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__mmb, "mmb");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__type3, "type3");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__type0, "type0");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__cid, "cid");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__cff, "cff");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__cff_cid, "cff-cid");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__type42, "type42");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__type11, "type11"); // ‘Type 42 CID’.
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__ttf, "ttf");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__ttf_symbol, "ttf-symbol");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__ttf_macbin, "ttf-macbin");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__ttc, "ttc");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__ttf_dfont, "ttf-dfont");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__otf, "otf");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__otf_dfont, "otf-dfont");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__otf_cid, "otf-cid");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__otf_cid_dfont, "otf-cid-dfont");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__svg, "svg");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__ufo, "ufo");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__woff, "woff");
SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__no_font, "no-font");

//-------------------------------------------------------------------------
