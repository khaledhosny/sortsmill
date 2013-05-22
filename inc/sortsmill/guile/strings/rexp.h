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

#ifndef _SORTSMILL_GUILE_STRINGS_REXP_H
#define _SORTSMILL_GUILE_STRINGS_REXP_H

#include <libguile.h>
#include <sortsmill/rexp.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

SCM scm_from_rexp_t (rexp_t);
rexp_t scm_to_rexp_t (SCM);
SCM scm_from_rexp_match_t (rexp_match_t);
rexp_match_t scm_to_rexp_match_t (SCM);

SCM scm_rexp_compile (SCM pattern);
SCM scm_rexp_compile_study (SCM pattern);
SCM scm_rexp_compile_jit (SCM pattern);
SCM scm_rexp_compile_once (SCM pattern);
SCM scm_rexp_compile_once_study (SCM pattern);
SCM scm_rexp_compile_once_jit (SCM pattern);
SCM scm_rexp_match (SCM re, SCM string, SCM start);
SCM scm_rexp_search (SCM re, SCM string, SCM start);
SCM scm_rexp_number_of_subexpressions (SCM match);
SCM scm_rexp_interval (SCM match, SCM subexpression);
SCM scm_rexp_substring (SCM match, SCM string, SCM subexpression);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_STRINGS_REXP_H */
