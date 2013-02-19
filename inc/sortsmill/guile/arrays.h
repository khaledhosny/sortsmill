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

#ifndef _SORTSMILL_GUILE_ARRAYS_H
#define _SORTSMILL_GUILE_ARRAYS_H

#include <libguile.h>
#include <gsl/gsl_matrix.h>
#include <sortsmill/guile/gsl.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

void scm_array_handle_unwind_handler (void *handlep);
void scm_dynwind_array_handle_release (scm_t_array_handle *handlep);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_ARRAYS_H */
