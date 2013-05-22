/*
 * Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
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

#ifndef _SORTSMILL_GUILE_MATH_GSL_ERROR_H
#define _SORTSMILL_GUILE_MATH_GSL_ERROR_H

#include <libguile.h>
#include <gsl/gsl_errno.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

SCM scm_gsl_errno_to_symbol (SCM errval);
SCM scm_c_gsl_errno_to_symbol (int errval);
SCM scm_raise_gsl_error (SCM arguments);
void scm_gsl_error_handler_for_raising_a_gsl_error (const char *reason,
                                                    const char *file,
                                                    int line, int gsl_errno);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_GSL_ERROR_H */
