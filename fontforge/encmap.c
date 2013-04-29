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
#include <sortsmill/initialized_global_constants.h>

//-------------------------------------------------------------------------

INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, _greater_than,
                      scm_c_initialize_from_eval_string, ">");

//-------------------------------------------------------------------------
//
// Generate instances of inline functions.

VISIBLE void make_enc_to_gid (EncMap *map);
VISIBLE void release_enc_to_gid (EncMap *map);
VISIBLE void clear_enc_to_gid (EncMap *map);
VISIBLE void set_enc_to_gid (EncMap *map, ssize_t enc, ssize_t gid);
VISIBLE void remove_enc_to_gid (EncMap *map, ssize_t enc);
VISIBLE ssize_t enc_to_gid (EncMap *map, ssize_t enc);
VISIBLE bool enc_to_gid_is_set (EncMap *map, ssize_t enc);

VISIBLE enc_iter_t enc_iter (EncMap *map);
VISIBLE enc_iter_t enc_iter_last (EncMap *map);
VISIBLE bool enc_done (enc_iter_t iter);
VISIBLE enc_iter_t enc_next (enc_iter_t iter);
VISIBLE enc_iter_t enc_prev (enc_iter_t iter);
VISIBLE ssize_t enc_enc (enc_iter_t iter);
VISIBLE ssize_t enc_gid (enc_iter_t iter);

VISIBLE void make_gid_to_enc (EncMap *map);
VISIBLE void release_gid_to_enc (EncMap *map);
VISIBLE void clear_gid_to_enc (EncMap *map);
VISIBLE void set_gid_to_enc (EncMap *map, ssize_t gid, ssize_t enc);
VISIBLE void add_gid_to_enc (EncMap *map, ssize_t gid, ssize_t enc);
VISIBLE void remove_all_gid_to_enc (EncMap *map, ssize_t gid);
VISIBLE ssize_t gid_to_enc (EncMap *map, ssize_t gid);
VISIBLE bool gid_to_enc_is_set (EncMap *map, ssize_t gid);

VISIBLE gid_iter_t gid_iter (EncMap *map);
VISIBLE gid_iter_t gid_iter_last (EncMap *map);
VISIBLE bool gid_done (gid_iter_t iter);
VISIBLE gid_iter_t gid_next (gid_iter_t iter);
VISIBLE gid_iter_t gid_prev (gid_iter_t iter);
VISIBLE ssize_t gid_gid (gid_iter_t iter);
VISIBLE ssize_t gid_enc (gid_iter_t iter);

//-------------------------------------------------------------------------

VISIBLE void
copy_enc_to_gid_contents (EncMap *new, EncMap *old)
{
  for (enc_iter_t p = enc_iter (old); !enc_done (p); p = enc_next (p))
    set_enc_to_gid (new, enc_enc (p), enc_gid (p));
}

VISIBLE void
copy_gid_to_enc_contents (EncMap *new, EncMap *old)
{
  for (gid_iter_t p = gid_iter (old); !gid_done (p); p = gid_next (p))
    set_gid_to_enc (new, gid_gid (p), gid_enc (p));
}

VISIBLE void
add_gid_to_enc (EncMap *map, ssize_t gid, ssize_t enc)
{
  if (enc == -1)
    remove_gid_to_enc (map, gid, enc);
  else
    {
      SCM scm_enc = scm_from_ssize_t (enc);
      SCM key = scm_from_ssize_t (gid);

      SCM value = scm_rbmapi_ref (map->_gid_to_enc, key, SCM_BOOL_F);

      SCM new_value;
      if (scm_is_false (value))
        new_value = scm_enc;
      else if (scm_is_pair (value))
        new_value = ((scm_is_false (scm_memv (scm_enc, value))) ?
                     scm_append (scm_list_2 (value, scm_list_1 (scm_enc))) :
                     value);
      else
        new_value = ((scm_to_ssize_t (value) != enc) ?
                     scm_list_2 (value, scm_enc) : value);

      scm_rbmapi_set_x (map->_gid_to_enc, key, new_value);
    }
}

VISIBLE void
remove_gid_to_enc (EncMap *map, ssize_t gid, ssize_t enc)
{
  SCM scm_enc = scm_from_ssize_t (enc);
  SCM key = scm_from_ssize_t (gid);

  SCM value = scm_rbmapi_ref (map->_gid_to_enc, key, SCM_BOOL_F);

  if (scm_is_true (value))
    {
      if (scm_is_pair (value))
        {
          SCM lst;
          if (scm_to_ssize_t (SCM_CAR (value)) == enc)
            // If we are removing the ‘current’ entry, replace it with
            // the highest-numbered code point. That is what the old
            // FontForge would have done.
            lst = scm_sort_list (SCM_CDR (value), _greater_than ());
          else
            lst = scm_delv (scm_enc, value);

          // @var{value} should be a list of length at least 2, with
          // unique elements; thus @var{lst} should have length at
          // least 1.
          assert (!scm_is_null (lst));

          SCM new_value = ((scm_is_null (SCM_CDR (lst))) ? SCM_CAR (lst) : lst);
          scm_rbmapi_set_x (map->_gid_to_enc, key, new_value);
        }
      else if (scm_to_ssize_t (value) == enc)
        scm_rbmapi_delete_x (map->_gid_to_enc, key);
    }
}

VISIBLE void
build_gid_to_enc (EncMap *map)
{
  // Work backwards, so the ‘current’ entry for a glyph with multiple
  // code points will be the greatest of the code points. That is what
  // the old FontForge would have done.
  for (enc_iter_t p = enc_iter_last (map); !enc_done (p); p = enc_prev (p))
    set_gid_to_enc (map, enc_gid (p), enc_enc (p));
}

VISIBLE void
rebuild_gid_to_enc (EncMap *map)
{
  clear_gid_to_enc (map);
  build_gid_to_enc (map);
}

//-------------------------------------------------------------------------
