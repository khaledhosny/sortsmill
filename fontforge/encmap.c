#include <config.h>

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
VISIBLE void clear_enc_to_gid (EncMap *map);
VISIBLE void set_enc_to_gid (EncMap *map, ssize_t enc, ssize_t gid);
VISIBLE ssize_t enc_to_gid (EncMap *map, ssize_t enc);
