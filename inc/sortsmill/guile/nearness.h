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

#ifndef _SORTSMILL_GUILE_NEARNESS_H
#define _SORTSMILL_GUILE_NEARNESS_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

bool scm_are_Within4RoundingErrors (SCM v1, SCM v2);
SCM scm_Within4RoundingErrors_p (SCM v1, SCM v2);
bool scm_are_Within16RoundingErrors (SCM v1, SCM v2);
SCM scm_Within16RoundingErrors_p (SCM v1, SCM v2);
bool scm_are_Within64RoundingErrors (SCM v1, SCM v2);
SCM scm_Within64RoundingErrors_p (SCM v1, SCM v2);

bool scm_are_RealNear (SCM a, SCM b);
SCM scm_RealNear_p (SCM a, SCM b);
bool scm_are_RealNearish (SCM a, SCM b);
SCM scm_RealNearish_p (SCM a, SCM b);
bool scm_are_RealApprox (SCM a, SCM b);
SCM scm_RealApprox_p (SCM a, SCM b);
bool scm_are_RealWithin (SCM a, SCM b, SCM fudge);
SCM scm_RealWithin_p (SCM a, SCM b, SCM fudge);
bool scm_are_RealRatio (SCM a, SCM b, SCM fudge);
SCM scm_RealRatio_p (SCM a, SCM b, SCM fudge);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_NEARNESS_H */
