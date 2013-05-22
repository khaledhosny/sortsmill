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

#ifndef _SORTSMILL_GUILE_CONTAINERS_RBMAP_H
#define _SORTSMILL_GUILE_CONTAINERS_RBMAP_H

#include <libguile.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

#define SCM_RBMAPI_NODE_HEAD intmax_t key; SCM value

typedef struct
{
  SCM_RBMAPI_NODE_HEAD;
} scm_t_rbmapi_node_data;

typedef scm_t_rbmapi_node_data *scm_t_rbmapi_iter;

inline intmax_t scm_rbmapi_iter_key (scm_t_rbmapi_iter iter);
inline SCM scm_rbmapi_iter_value (scm_t_rbmapi_iter iter);
inline void scm_rbmapi_iter_set_value (scm_t_rbmapi_iter iter, SCM value);

scm_t_rbmapi_iter scm_c_rbmapi_first (SCM map);
scm_t_rbmapi_iter scm_c_rbmapi_last (SCM map);
scm_t_rbmapi_iter scm_c_rbmapi_nsearch (SCM map, SCM key);
scm_t_rbmapi_iter scm_c_rbmapi_psearch (SCM map, SCM key);
scm_t_rbmapi_iter scm_c_rbmapi_next (SCM map, scm_t_rbmapi_iter iter);
scm_t_rbmapi_iter scm_c_rbmapi_prev (SCM map, scm_t_rbmapi_iter iter);

SCM scm_make_rbmapi (void);
SCM scm_rbmapi_set_x (SCM map, SCM key, SCM value);
SCM scm_rbmapi_delete_x (SCM map, SCM key);
SCM scm_rbmapi_ref (SCM map, SCM key, SCM default_value);
SCM scm_rbmapi_fold_left (SCM proc, SCM init, SCM map, SCM start_key);
SCM scm_rbmapi_fold_right (SCM proc, SCM init, SCM map, SCM start_key);
SCM scm_alist_to_rbmapi (SCM alist);
SCM scm_rbmapi_to_alist (SCM map);
SCM scm_rbmapi_map_to_list (SCM proc, SCM map);
SCM scm_rbmapi_for_each (SCM proc, SCM map);
SCM scm_rbmapi_count (SCM pred, SCM map);
SCM scm_rbmapi_size (SCM map);

inline intmax_t
scm_rbmapi_iter_key (scm_t_rbmapi_iter iter)
{
  return iter->key;
}

inline SCM
scm_rbmapi_iter_value (scm_t_rbmapi_iter iter)
{
  return iter->value;
}

inline void
scm_rbmapi_iter_set_value (scm_t_rbmapi_iter iter, SCM value)
{
  iter->value = value;
}

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_CONTAINERS_RBMAP_H */
