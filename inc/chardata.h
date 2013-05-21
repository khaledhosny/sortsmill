/*
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

#include <config.h>
#include <basics.h>

struct charmap {
    int first, last;
    unsigned char **table;
    uint32_t *totable;
};
struct charmap2 {
    int first, last;
    unsigned short **table;
    uint32_t *totable;
};

VISIBLE extern const uint32_t unicode_from_jis201[];
VISIBLE extern const uint32_t unicode_from_win[];
VISIBLE extern const uint32_t unicode_from_mac[];
VISIBLE extern struct charmap mac_from_unicode;

VISIBLE extern const uint32_t unicode_from_jis208[];
/* Subtract 0x8100 before indexing this array */
VISIBLE extern const uint32_t unicode_from_big5hkscs[];
VISIBLE extern const uint32_t unicode_from_ksc5601[];
/* Subtract 0x8400 before indexing this array */
VISIBLE extern const uint32_t unicode_from_johab[];
VISIBLE extern const uint32_t unicode_from_gb2312[];

/* a mask for each character saying what charset(s) it may be found in */
VISIBLE extern const unsigned long * const unicode_backtrans[];

VISIBLE extern const uint32_t *const * const unicode_alternates[];
