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

#include <sortsmill/guile/rbmap.h>
#include <sortsmill/rb.h>
#include <sortsmill/initialized_global_constants.h>
#include <sortsmill/attributes.h>

//-------------------------------------------------------------------------

INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, _pointer_to_rbmapi,
                      scm_c_initialize_from_eval_string,
                      "(@@ (sortsmill rbmap) pointer->rbmapi)");

INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, _rbmapi_to_pointer,
                      scm_c_initialize_from_eval_string,
                      "(@@ (sortsmill rbmap) rbmapi->pointer)");

//-------------------------------------------------------------------------
//
// Maps with intmax_t as keys.

typedef struct rbmapi_node_s rbmapi_node_t;

struct rbmapi_node_s
{
  SCM_RBMAPI_NODE_HEAD;
  rb_node (rbmapi_node_t) rbmapi_link;
};

// Why the following typedef is done this way: it works around
// confusion in at least some versions of GNU Emacs.
#define _rb_tree_rbmapi_node_t rb_tree (rbmapi_node_t)
typedef _rb_tree_rbmapi_node_t rbmapi_t;

static int
rbmapi_cmp (rbmapi_node_t *a_node, rbmapi_node_t *a_other)
{
  int cmp;
  if (a_node->key < a_other->key)
    cmp = -1;
  else if (a_other->key < a_node->key)
    cmp = 1;
  else
    cmp = 0;
  return cmp;
}

rb_gen (_FF_MAYBE_UNUSED static, rbmapi_, rbmapi_t, rbmapi_node_t, rbmapi_link,
        rbmapi_cmp);

static SCM
scm_from_c_rbmapi (rbmapi_t *p)
{
  return scm_call_1 (_pointer_to_rbmapi (), scm_from_pointer (p, NULL));
}

static rbmapi_t *
scm_to_c_rbmapi (SCM map)
{
  return (rbmapi_t *) scm_to_pointer (scm_call_1 (_rbmapi_to_pointer (), map));
}

// Create instances of inline functions.
VISIBLE intmax_t scm_rbmapi_iter_key (scm_t_rbmapi_iter iter);
VISIBLE SCM scm_rbmapi_iter_value (scm_t_rbmapi_iter iter);
VISIBLE void scm_rbmapi_iter_set_value (scm_t_rbmapi_iter iter, SCM value);

VISIBLE scm_t_rbmapi_iter
scm_c_rbmapi_first (SCM map)
{
  return (scm_t_rbmapi_iter) rbmapi_first (scm_to_c_rbmapi (map));
}

VISIBLE scm_t_rbmapi_iter
scm_c_rbmapi_last (SCM map)
{
  return (scm_t_rbmapi_iter) rbmapi_last (scm_to_c_rbmapi (map));
}

VISIBLE scm_t_rbmapi_iter
scm_c_rbmapi_nsearch (SCM map, SCM key)
{
  rbmapi_t *tree = scm_to_c_rbmapi (map);
  scm_t_rbmapi_iter iter;
  if (SCM_UNBNDP (key))
    iter = (scm_t_rbmapi_iter) rbmapi_first (tree);
  else
    {
      const intmax_t i_key = scm_to_intmax (key);
      rbmapi_node_t key_node = {.key = i_key };
      iter = (scm_t_rbmapi_iter) rbmapi_nsearch (tree, &key_node);
    }
  return iter;
}

VISIBLE scm_t_rbmapi_iter
scm_c_rbmapi_psearch (SCM map, SCM key)
{
  rbmapi_t *tree = scm_to_c_rbmapi (map);
  scm_t_rbmapi_iter iter;
  if (SCM_UNBNDP (key))
    iter = (scm_t_rbmapi_iter) rbmapi_last (tree);
  else
    {
      const intmax_t i_key = scm_to_intmax (key);
      rbmapi_node_t key_node = {.key = i_key };
      iter = (scm_t_rbmapi_iter) rbmapi_psearch (tree, &key_node);
    }
  return iter;
}

VISIBLE scm_t_rbmapi_iter
scm_c_rbmapi_next (SCM map, scm_t_rbmapi_iter iter)
{
  return (scm_t_rbmapi_iter) rbmapi_next (scm_to_c_rbmapi (map),
                                          (rbmapi_node_t *) iter);
}

VISIBLE scm_t_rbmapi_iter
scm_c_rbmapi_prev (SCM map, scm_t_rbmapi_iter iter)
{
  return (scm_t_rbmapi_iter) rbmapi_prev (scm_to_c_rbmapi (map),
                                          (rbmapi_node_t *) iter);
}

VISIBLE SCM
scm_make_rbmapi (void)
{
  rbmapi_t *tree = scm_gc_malloc (sizeof (rbmapi_t), "rbmapi_t");
  rbmapi_new (tree);
  return scm_from_c_rbmapi (tree);
}

static inline void
internal_rbmapi_insert (rbmapi_t *tree, intmax_t i_key, SCM value)
{
  rbmapi_node_t *node = scm_gc_malloc (sizeof (rbmapi_node_t), "rbmapi_node_t");
  node->key = i_key;
  node->value = value;
  rbmapi_insert (tree, node);
}

VISIBLE SCM
scm_rbmapi_set_x (SCM map, SCM key, SCM value)
{
  rbmapi_t *tree = scm_to_c_rbmapi (map);
  const intmax_t i_key = scm_to_intmax (key);
  rbmapi_node_t key_node = {.key = i_key };

  rbmapi_node_t *node = rbmapi_search (tree, &key_node);
  if (node == NULL)
    internal_rbmapi_insert (tree, i_key, value);
  else
    node->value = value;

  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_rbmapi_delete_x (SCM map, SCM key)
{
  rbmapi_t *tree = scm_to_c_rbmapi (map);
  const intmax_t i_key = scm_to_intmax (key);
  rbmapi_node_t key_node = {.key = i_key };

  rbmapi_node_t *node = rbmapi_search (tree, &key_node);

  if (node != NULL)
    rbmapi_remove (tree, node);

  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_rbmapi_ref (SCM map, SCM key, SCM default_value)
{
  SCM dflt = SCM_UNBNDP (default_value) ? SCM_BOOL_F : default_value;

  rbmapi_t *tree = scm_to_c_rbmapi (map);
  const intmax_t i_key = scm_to_intmax (key);
  rbmapi_node_t key_node = {.key = i_key };

  const rbmapi_node_t *node = rbmapi_search (tree, &key_node);

  return (node == NULL) ? dflt : node->value;
}

VISIBLE SCM
scm_rbmapi_fold_left (SCM proc, SCM init, SCM map, SCM start_key)
{
  for (scm_t_rbmapi_iter p = scm_c_rbmapi_nsearch (map, start_key); p != NULL;
       p = scm_c_rbmapi_next (map, p))
    init =
      scm_call_3 (proc, init, scm_from_intmax (scm_rbmapi_iter_key (p)),
                  scm_rbmapi_iter_value (p));
  return init;
}

VISIBLE SCM
scm_rbmapi_fold_right (SCM proc, SCM init, SCM map, SCM start_key)
{
  for (scm_t_rbmapi_iter p = scm_c_rbmapi_psearch (map, start_key); p != NULL;
       p = scm_c_rbmapi_prev (map, p))
    init =
      scm_call_3 (proc, scm_from_intmax (scm_rbmapi_iter_key (p)),
                  scm_rbmapi_iter_value (p), init);
  return init;
}

VISIBLE SCM
scm_rbmapi_count (SCM pred, SCM map)
{
  SCM count = scm_from_int (0);
  for (scm_t_rbmapi_iter p = scm_c_rbmapi_first (map); p != NULL;
       p = scm_c_rbmapi_next (map, p))
    if (scm_is_true
        (scm_call_2 (pred, scm_from_intmax (scm_rbmapi_iter_key (p)),
                     scm_rbmapi_iter_value (p))))
      count = scm_oneplus (count);
  return count;
}

VISIBLE SCM
scm_rbmapi_size (SCM map)
{
  SCM size = scm_from_int (0);
  for (scm_t_rbmapi_iter p = scm_c_rbmapi_first (map); p != NULL;
       p = scm_c_rbmapi_next (map, p))
    size = scm_oneplus (size);
  return size;
}

//-------------------------------------------------------------------------

void init_sortsmill_guile_rbmap (void);

VISIBLE void
init_sortsmill_guile_rbmap (void)
{
  scm_c_define_gsubr ("make-rbmapi", 0, 0, 0, scm_make_rbmapi);
  scm_c_define_gsubr ("rbmapi-set!", 3, 0, 0, scm_rbmapi_set_x);
  scm_c_define_gsubr ("rbmapi-delete!", 2, 0, 0, scm_rbmapi_delete_x);
  scm_c_define_gsubr ("rbmapi-ref", 2, 1, 0, scm_rbmapi_ref);
  scm_c_define_gsubr ("rbmapi-fold-left", 3, 1, 0, scm_rbmapi_fold_left);
  scm_c_define_gsubr ("rbmapi-fold-right", 3, 1, 0, scm_rbmapi_fold_right);
  scm_c_define_gsubr ("rbmapi-count", 2, 0, 0, scm_rbmapi_count);
  scm_c_define_gsubr ("rbmapi-size", 1, 0, 0, scm_rbmapi_size);
}

//-------------------------------------------------------------------------
