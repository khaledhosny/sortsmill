// Copyright (C) 2012 Barry Schwartz
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#ifndef _PASCALS_TRIANGLE_H
#define _PASCALS_TRIANGLE_H

#include <config.h>

VISIBLE const int *pascals_triangle_row (unsigned int n);
VISIBLE const int *pascals_triangle_row_altsigns (unsigned int n);

#endif // _PASCALS_TRIANGLE_H
