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

#ifndef _SORTSMILL_NEARNESS_H
#define _SORTSMILL_NEARNESS_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

bool Within4RoundingErrors (double v1, double v2);
bool Within16RoundingErrors (double v1, double v2);
bool Within64RoundingErrors (double v1, double v2);

bool RealNear (double a, double b);
bool RealNearish (double a, double b);
bool RealApprox (double a, double b);
bool RealWithin (double a, double b, double fudge);
bool RealRatio (double a, double b, double fudge);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_NEARNESS_H */
