#include <config.h>             // -*- coding: utf-8 -*-

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

#include <splinefont.h>
#include <sortsmill/guile.h>

typedef SCM _scm_symbol_func (void);

static _scm_symbol_func *symbol_lookup[] = {
  [ff_pfa] = scm_symbol__pfa,   // pfa
  [ff_pfb] = scm_symbol__pfb,   // pfb
  [ff_pfbmacbin] = scm_symbol__pfb_macbin,      // pfb-macbin
  [ff_multiple] = scm_symbol__pfb_multiple,     // pfb-multiple
  [ff_mma] = scm_symbol__mma,   // mma
  [ff_mmb] = scm_symbol__mmb,   // mmb
  [ff_ptype3] = scm_symbol__type3,      // type3
  [ff_ptype0] = scm_symbol__type0,      // type0
  [ff_cid] = scm_symbol__cid,   // cid
  [ff_cff] = scm_symbol__cff,   // cff
  [ff_cffcid] = scm_symbol__cff_cid,    // cff-cid
  [ff_type42] = scm_symbol__type42,     // type42
  [ff_type42cid] = scm_symbol__type11,  // type11
  [ff_ttf] = scm_symbol__ttf,   // ttf
  [ff_ttfsym] = scm_symbol__ttf_symbol, // ttf-symbol
  [ff_ttfmacbin] = scm_symbol__ttf_macbin,      // ttf-macbin
  [ff_ttc] = scm_symbol__ttc,   // ttc
  [ff_ttfdfont] = scm_symbol__ttf_dfont,        // ttf-dfont
  [ff_otf] = scm_symbol__otf,   // otf
  [ff_otfdfont] = scm_symbol__otf_dfont,        // otf-dfont
  [ff_otfcid] = scm_symbol__otf_cid,    // otf-cid
  [ff_otfciddfont] = scm_symbol__otf_cid_dfont, // otf-cid-dfont
  [ff_ufo] = scm_symbol__ufo,   // ufo
  [ff_woff] = scm_symbol__woff, // woff
  [ff_none] = scm_symbol__no_font       // no-font
};

VISIBLE SCM
scm_from_FontFormat (int fontformat)
{
  return ((0 <= fontformat && fontformat <= ff_none) ?
          symbol_lookup[fontformat] () : SCM_BOOL_F);
}

VISIBLE int
scm_to_FontFormat (SCM symbol)
{
  // Returns -1 if the argument is not a symbol representing one of
  // the outline font formats.

  int result = -1;
  if (scm_is_symbol (symbol))
    {
      int i = ff_none;
      while (i != -1 && !scm_is_eq (symbol, symbol_lookup[i] ()))
        i--;
      result = i;
    }
  return result;
}

VISIBLE SCM
scm_integer_to_font_format (SCM i)
{
  return scm_from_FontFormat (scm_to_int (i));
}

VISIBLE SCM
scm_font_format_to_integer (SCM symbol)
{
  const int i = scm_to_FontFormat (symbol);
  return (i != -1) ? scm_from_int (i) : SCM_BOOL_F;
}

//-------------------------------------------------------------------------

void init_sortsmill_guile_fonts_font_formats (void);

VISIBLE void
init_sortsmill_guile_fonts_font_formats (void)
{
  scm_c_define_gsubr ("integer->font-format", 1, 0, 0,
                      scm_integer_to_font_format);
  scm_c_define_gsubr ("font-format->integer", 1, 0, 0,
                      scm_font_format_to_integer);
}

//-------------------------------------------------------------------------
