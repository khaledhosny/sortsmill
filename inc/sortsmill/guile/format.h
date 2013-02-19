/*
 * Copyright (C) 2012 Barry Schwartz
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

#ifndef _SORTSMILL_GUILE_FORMAT_H
#define _SORTSMILL_GUILE_FORMAT_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* ‘format’ from (ice-9 format). */
SCM scm_format (SCM destination, SCM message, SCM args);
SCM scm_c_format (SCM destination, const char *message, SCM args);

/* ‘format’ with ‘destination’ set to #f. */
SCM scm_sformat (SCM message, SCM args);
SCM scm_c_sformat (const char *message, SCM args);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FORMAT_H */
