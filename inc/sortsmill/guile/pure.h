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

#ifndef _SORTSMILL_PURE_H
#define _SORTSMILL_PURE_H

#include <pure/runtime.h>
#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

SCM scm_big_integer_to_pure_expr (SCM);
SCM scm_pure_expr_to_small_integer_or_f (SCM);
SCM scm_pure_expr_to_big_integer_or_f (SCM);
SCM scm_rational_to_pure_expr (SCM);
SCM scm_pure_expr_to_rational_or_f (SCM);
SCM scm_pure_expr_to_inexact_or_f (SCM);
SCM scm_complex_to_pure_expr (SCM);
SCM scm_pure_expr_to_complex_or_f (SCM);
SCM scm_pure_expr_to_pointer_or_f (SCM);
SCM scm_pure_expr_to_string_or_f (SCM);
SCM scm_pure_expr_is_string (SCM);
SCM scm_symbol_pure_expr_to_small_integer_or_f (SCM);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_PURE_H */
