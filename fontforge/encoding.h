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

#ifndef _ENCODING_H
#define _ENCODING_H

struct cidaltuni {
    struct cidaltuni *next;
    int uni;
    int cid;
};

struct cidmap {
    char *registry, *ordering;
    int supplement, maxsupple;
    int cidmax;			/* Max cid found in the charset */
    int namemax;		/* Max cid with useful info */
    uint32_t *unicode;
    char **name;
    struct cidaltuni *alts;
    struct cidmap *next;
};

VISIBLE extern struct cidmap *cidmaps;

VISIBLE extern void DeleteEncoding(Encoding *me);
VISIBLE extern void RemoveMultiples(Encoding *item);
#endif
