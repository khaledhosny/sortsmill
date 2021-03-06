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

#ifndef _SORTSMILL_GUILE_H
#define _SORTSMILL_GUILE_H

#include <sortsmill/guile/core.h>
#include <sortsmill/guile/arrays.h>
#include <sortsmill/guile/copy_with_strides.h>
#include <sortsmill/guile/editor.h>
#include <sortsmill/guile/fonts.h>
#include <sortsmill/guile/iconv.h>
#include <sortsmill/guile/initialized_global_constants.h>
#include <sortsmill/guile/math.h>
#include <sortsmill/guile/nearness.h>
#include <sortsmill/guile/notices.h>
#include <sortsmill/guile/postscript.h>
#include <sortsmill/guile/core.h>
#include <sortsmill/guile/strings.h>
#include <sortsmill/guile/symbols.h>
#include <sortsmill/guile/types.h>
#include <sortsmill/guile/wrap.h>

#if 0
/* These have to be included explicitly by the user. One reason is not
   to force the poor behavior of ‘#include <Python.h>’ on people. But,
   also, they are not part of the core system. */
#include <sortsmill/guile/pure.h>
#include <sortsmill/guile/python.h>
#endif

#endif /* _SORTSMILL_GUILE_H */
