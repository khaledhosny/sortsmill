#include <config.h>             // -*- coding: utf-8 -*-

// Copyright (C) 2013 Barry Schwartz
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

#include <splinefont.h>

// Generate instances of these inline functions.
VISIBLE void make_enc_to_gid (EncMap *map);
VISIBLE void release_enc_to_gid (EncMap *map);
VISIBLE void clear_enc_to_gid (EncMap *map);
VISIBLE void set_enc_to_gid (EncMap *map, ssize_t enc, ssize_t gid);
VISIBLE void remove_enc_to_gid (EncMap *map, ssize_t enc);
VISIBLE ssize_t enc_to_gid (EncMap *map, ssize_t enc);
VISIBLE bool enc_to_gid_is_set (EncMap *map, ssize_t enc);

// Generate instances of these inline functions.
VISIBLE void set_gid_to_enc (EncMap *map, ssize_t gid, ssize_t enc);
VISIBLE void add_gid_to_enc (EncMap *map, ssize_t gid, ssize_t enc);
VISIBLE void remove_all_gid_to_enc (EncMap *map, ssize_t gid);
VISIBLE ssize_t gid_to_enc (EncMap *map, ssize_t gid);
VISIBLE bool gid_to_enc_is_set (EncMap *map, ssize_t gid);

VISIBLE void
remove_gid_to_enc (EncMap *map, ssize_t gid, ssize_t enc)
{
  // FIXME: This is a temporary definition until there is support for
  // multiple code points in @code{gid_to_enc}.

  if (gid_to_enc (map, gid) == enc)
    {
      // Replace the entry with either another suitable entry, if
      // there is one. Otherwise delete the entry.

      // FIXME: This is too clever. I think the reason a loop in the
      // negative direction was used here is that it results in
      // @code{i == -1} if no match is found, which turns out to be,
      // by mere coincidence, the ‘code point’ for deleting an entry.

      ssize_t i = map->enc_limit - 1;
      while (0 <= i && enc_to_gid (map, i) != gid)
        i--;
      set_gid_to_enc (map, gid, i);
    }
}
