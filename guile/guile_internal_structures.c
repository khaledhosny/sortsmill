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

#include <sortsmillff/guile/view.h>
#include <sortsmillff/guile/internal-structures.h>
#include <baseviews.h>
#include <stdio.h>
#include <stdbool.h>

void init_guile_sortsmillff_view (void);
void init_guile_sortsmillff_internal_structures (void);

//-------------------------------------------------------------------------

#define _WRAPPED_POINTER(KIND)			\
  static SCM _##KIND##_p = SCM_UNDEFINED;	\
						\
  VISIBLE SCM					\
  scm_##KIND##_p (SCM obj)			\
  {						\
    return scm_call_1 (_##KIND##_p, obj);	\
  }						\
						\
  VISIBLE int					\
  scm_is_##KIND (SCM obj)			\
  {						\
    return scm_is_true (scm_##KIND##_p (obj));	\
  }

#define _SET_POINTER_WRAPPER(KIND, SCHEME_KIND)			\
  do								\
    {								\
      _##KIND##_p = scm_c_eval_string (SCHEME_KIND "?");	\
    }								\
  while (false)

//-------------------------------------------------------------------------

_WRAPPED_POINTER (font_view);
_WRAPPED_POINTER (glyph_view);

VISIBLE void
init_guile_sortsmillff_view (void)
{
  _SET_POINTER_WRAPPER (font_view, "font-view");
  _SET_POINTER_WRAPPER (glyph_view, "glyph-view");
}

//-------------------------------------------------------------------------

_WRAPPED_POINTER (ff_FontViewBase);
_WRAPPED_POINTER (ff_SplineChar);
_WRAPPED_POINTER (ff_SplineFont);
_WRAPPED_POINTER (ff_EncMap);

VISIBLE void
init_guile_sortsmillff_internal_structures (void)
{
  _SET_POINTER_WRAPPER (ff_FontViewBase, "ff:FontViewBase");
  _SET_POINTER_WRAPPER (ff_SplineChar, "ff:SplineChar");
  _SET_POINTER_WRAPPER (ff_SplineFont, "ff:SplineFont");
  _SET_POINTER_WRAPPER (ff_EncMap, "ff:EncMap");
}

//-------------------------------------------------------------------------
