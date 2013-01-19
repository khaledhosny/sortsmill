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

#ifndef _SORTSMILLFF_RNRS_CONDITIONS_H
#define _SORTSMILLFF_RNRS_CONDITIONS_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

SCM rnrs_condition (SCM condition_list);
SCM rnrs_raise_condition (SCM condition_list);
SCM rnrs_make_error (void);
SCM rnrs_make_assertion_violation (void);
SCM rnrs_make_who_condition (SCM who);
SCM rnrs_c_make_who_condition (const char *who);
SCM rnrs_make_message_condition (SCM message);
SCM rnrs_c_make_message_condition (const char *message);
SCM rnrs_make_irritants_condition (SCM irritants);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILLFF_RNRS_CONDITIONS_H */
