#include <config.h>

// Copyright (C) 2013 Barry Schwartz
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

#include <sortsmill/guile/symbols.h>
#include <sortsmill/initialized_global_constants.h>

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

SCM_SYMBOL_CONSTANT (VISIBLE, scm_symbol__grid, "grid");

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
