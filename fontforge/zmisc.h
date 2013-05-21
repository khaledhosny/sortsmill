// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#ifndef _FF_INTERNAL_ZMISC_H
#define _FF_INTERNAL_ZMISC_H

/**
 * Misc portability and convenience macros.
 **/

#include <stdlib.h>

#define zalloc malloc
#define zrealloc realloc
#define zfree free

#define znew(type, n) (type *)zalloc(sizeof(type) * (n))
#define zrenew(type, p, n) (type *)zrealloc((p), sizeof(type) * (n))

#endif // _FF_INTERNAL_ZMISC_H
