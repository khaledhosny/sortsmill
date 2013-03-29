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

#include <sortsmill/guile/arrays.h>

void init_guile_sortsmill_arrays (void);

//-------------------------------------------------------------------------

VISIBLE void
scm_array_handle_unwind_handler (void *handlep)
{
  scm_array_handle_release ((scm_t_array_handle *) handlep);
}

VISIBLE void
scm_dynwind_array_handle_release (scm_t_array_handle *handlep)
{
  scm_dynwind_unwind_handler (scm_array_handle_unwind_handler, handlep,
                              SCM_F_WIND_EXPLICITLY);
}

//-------------------------------------------------------------------------

#define _FF_GUILE_VECTAG_DEFN(NAME, VECTAG_NAME)                        \
									\
  VISIBLE SCM _##NAME = SCM_UNDEFINED;                                  \
  VISIBLE volatile AO_t _##NAME##_is_initialized = false;		\
  static pthread_mutex_t _##NAME##_mutex = PTHREAD_MUTEX_INITIALIZER;	\
                                                                        \
  void _initialize_##NAME (void);                                       \
									\
  VISIBLE void								\
  _initialize_##NAME (void)						\
  {									\
    pthread_mutex_lock (&_##NAME##_mutex);				\
    if (!_##NAME##_is_initialized)					\
      {									\
        _##NAME = scm_from_latin1_symbol (#VECTAG_NAME);                \
	AO_store_release_write (&_##NAME##_is_initialized, true);	\
      }									\
    pthread_mutex_unlock (&_##NAME##_mutex);				\
  }

_FF_GUILE_VECTAG_DEFN (scm_symbol_u8, u8);
_FF_GUILE_VECTAG_DEFN (scm_symbol_s8, s8);
_FF_GUILE_VECTAG_DEFN (scm_symbol_u16, u16);
_FF_GUILE_VECTAG_DEFN (scm_symbol_s16, s16);
_FF_GUILE_VECTAG_DEFN (scm_symbol_u32, u32);
_FF_GUILE_VECTAG_DEFN (scm_symbol_s32, s32);
_FF_GUILE_VECTAG_DEFN (scm_symbol_u64, u64);
_FF_GUILE_VECTAG_DEFN (scm_symbol_s64, s64);
_FF_GUILE_VECTAG_DEFN (scm_symbol_f32, f32);
_FF_GUILE_VECTAG_DEFN (scm_symbol_f64, f64);
_FF_GUILE_VECTAG_DEFN (scm_symbol_c32, c32);
_FF_GUILE_VECTAG_DEFN (scm_symbol_c64, c64);

static bool index_means_uniform_signed_integer[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = false,
  [_FF_INDEX_ARRAY_NONUNIFORM] = false,
  [_FF_INDEX_ARRAY_U8] = false,
  [_FF_INDEX_ARRAY_S8] = true,
  [_FF_INDEX_ARRAY_U16] = false,
  [_FF_INDEX_ARRAY_S16] = true,
  [_FF_INDEX_ARRAY_U32] = false,
  [_FF_INDEX_ARRAY_S32] = true,
  [_FF_INDEX_ARRAY_U64] = false,
  [_FF_INDEX_ARRAY_S64] = true,
  [_FF_INDEX_ARRAY_F32] = false,
  [_FF_INDEX_ARRAY_F64] = false,
  [_FF_INDEX_ARRAY_C32] = false,
  [_FF_INDEX_ARRAY_C64] = false
};

static bool index_means_uniform_unsigned_integer[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = false,
  [_FF_INDEX_ARRAY_NONUNIFORM] = false,
  [_FF_INDEX_ARRAY_U8] = true,
  [_FF_INDEX_ARRAY_S8] = false,
  [_FF_INDEX_ARRAY_U16] = true,
  [_FF_INDEX_ARRAY_S16] = false,
  [_FF_INDEX_ARRAY_U32] = true,
  [_FF_INDEX_ARRAY_S32] = false,
  [_FF_INDEX_ARRAY_U64] = true,
  [_FF_INDEX_ARRAY_S64] = false,
  [_FF_INDEX_ARRAY_F32] = false,
  [_FF_INDEX_ARRAY_F64] = false,
  [_FF_INDEX_ARRAY_C32] = false,
  [_FF_INDEX_ARRAY_C64] = false
};

static bool index_means_uniform_integer[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = false,
  [_FF_INDEX_ARRAY_NONUNIFORM] = false,
  [_FF_INDEX_ARRAY_U8] = true,
  [_FF_INDEX_ARRAY_S8] = true,
  [_FF_INDEX_ARRAY_U16] = true,
  [_FF_INDEX_ARRAY_S16] = true,
  [_FF_INDEX_ARRAY_U32] = true,
  [_FF_INDEX_ARRAY_S32] = true,
  [_FF_INDEX_ARRAY_U64] = true,
  [_FF_INDEX_ARRAY_S64] = true,
  [_FF_INDEX_ARRAY_F32] = false,
  [_FF_INDEX_ARRAY_F64] = false,
  [_FF_INDEX_ARRAY_C32] = false,
  [_FF_INDEX_ARRAY_C64] = false
};

static bool index_means_uniform_real[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = false,
  [_FF_INDEX_ARRAY_NONUNIFORM] = false,
  [_FF_INDEX_ARRAY_U8] = true,
  [_FF_INDEX_ARRAY_S8] = true,
  [_FF_INDEX_ARRAY_U16] = true,
  [_FF_INDEX_ARRAY_S16] = true,
  [_FF_INDEX_ARRAY_U32] = true,
  [_FF_INDEX_ARRAY_S32] = true,
  [_FF_INDEX_ARRAY_U64] = true,
  [_FF_INDEX_ARRAY_S64] = true,
  [_FF_INDEX_ARRAY_F32] = true,
  [_FF_INDEX_ARRAY_F64] = true,
  [_FF_INDEX_ARRAY_C32] = false,
  [_FF_INDEX_ARRAY_C64] = false
};

static bool index_means_uniform_real_float[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = false,
  [_FF_INDEX_ARRAY_NONUNIFORM] = false,
  [_FF_INDEX_ARRAY_U8] = false,
  [_FF_INDEX_ARRAY_S8] = false,
  [_FF_INDEX_ARRAY_U16] = false,
  [_FF_INDEX_ARRAY_S16] = false,
  [_FF_INDEX_ARRAY_U32] = false,
  [_FF_INDEX_ARRAY_S32] = false,
  [_FF_INDEX_ARRAY_U64] = false,
  [_FF_INDEX_ARRAY_S64] = false,
  [_FF_INDEX_ARRAY_F32] = true,
  [_FF_INDEX_ARRAY_F64] = true,
  [_FF_INDEX_ARRAY_C32] = false,
  [_FF_INDEX_ARRAY_C64] = false
};

static bool index_means_uniform_complex_float[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = false,
  [_FF_INDEX_ARRAY_NONUNIFORM] = false,
  [_FF_INDEX_ARRAY_U8] = false,
  [_FF_INDEX_ARRAY_S8] = false,
  [_FF_INDEX_ARRAY_U16] = false,
  [_FF_INDEX_ARRAY_S16] = false,
  [_FF_INDEX_ARRAY_U32] = false,
  [_FF_INDEX_ARRAY_S32] = false,
  [_FF_INDEX_ARRAY_U64] = false,
  [_FF_INDEX_ARRAY_S64] = false,
  [_FF_INDEX_ARRAY_F32] = false,
  [_FF_INDEX_ARRAY_F64] = false,
  [_FF_INDEX_ARRAY_C32] = true,
  [_FF_INDEX_ARRAY_C64] = true
};

static bool index_means_uniform_number[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = false,
  [_FF_INDEX_ARRAY_NONUNIFORM] = false,
  [_FF_INDEX_ARRAY_U8] = true,
  [_FF_INDEX_ARRAY_S8] = true,
  [_FF_INDEX_ARRAY_U16] = true,
  [_FF_INDEX_ARRAY_S16] = true,
  [_FF_INDEX_ARRAY_U32] = true,
  [_FF_INDEX_ARRAY_S32] = true,
  [_FF_INDEX_ARRAY_U64] = true,
  [_FF_INDEX_ARRAY_S64] = true,
  [_FF_INDEX_ARRAY_F32] = true,
  [_FF_INDEX_ARRAY_F64] = true,
  [_FF_INDEX_ARRAY_C32] = true,
  [_FF_INDEX_ARRAY_C64] = true
};

VISIBLE scm_t_array_type_index
scm_array_type_to_array_type_index (SCM array_type)
{
  scm_t_array_type_index i = _FF_INDEX_NOT_AN_ARRAY;
  if (scm_is_eq (array_type, SCM_BOOL_T))
    i = _FF_INDEX_ARRAY_NONUNIFORM;
  else if (scm_is_eq (array_type, scm_symbol_f64 ()))
    i = _FF_INDEX_ARRAY_F64;
  else if (scm_is_eq (array_type, scm_symbol_s32 ()))
    i = _FF_INDEX_ARRAY_S32;
  else if (scm_is_eq (array_type, scm_symbol_s16 ()))
    i = _FF_INDEX_ARRAY_S16;
  else if (scm_is_eq (array_type, scm_symbol_s8 ()))
    i = _FF_INDEX_ARRAY_S8;
  else if (scm_is_eq (array_type, scm_symbol_s64 ()))
    i = _FF_INDEX_ARRAY_S64;
  else if (scm_is_eq (array_type, scm_symbol_u32 ()))
    i = _FF_INDEX_ARRAY_U32;
  else if (scm_is_eq (array_type, scm_symbol_u16 ()))
    i = _FF_INDEX_ARRAY_U16;
  else if (scm_is_eq (array_type, scm_symbol_u8 ()))
    i = _FF_INDEX_ARRAY_U8;
  else if (scm_is_eq (array_type, scm_symbol_u64 ()))
    i = _FF_INDEX_ARRAY_U64;
  else if (scm_is_eq (array_type, scm_symbol_c64 ()))
    i = _FF_INDEX_ARRAY_C64;
  else if (scm_is_eq (array_type, scm_symbol_f32 ()))
    i = _FF_INDEX_ARRAY_F32;
  else if (scm_is_eq (array_type, scm_symbol_c32 ()))
    i = _FF_INDEX_ARRAY_C32;
  return i;
}

VISIBLE scm_t_array_type_index
scm_array_handle_to_array_type_index (scm_t_array_handle *handlep)
{
  // FIXME: Is scm_array_handle_element_type supposed to be
  // documented? It is not, in Guile 2.0.7.
  SCM array_type = scm_array_handle_element_type (handlep);

  return scm_array_type_to_array_type_index (array_type);
}

VISIBLE scm_t_array_type_index
scm_to_array_type_index (SCM obj)
{
  scm_t_array_type_index i = _FF_INDEX_NOT_AN_ARRAY;
  if (scm_is_array (obj))
    {
      scm_t_array_handle handle;
      scm_array_get_handle (obj, &handle);
      i = scm_array_handle_to_array_type_index (&handle);
      scm_array_handle_release (&handle);
    }
  return i;
}

static bool
scm_array_dimension_pred (bool (*nonuniform_pred) (SCM), size_t rank,
                          const scm_t_array_dim *dims, const SCM *elems,
                          size_t current_dim, size_t start_index)
{
  bool result;
  ssize_t lbnd = dims[current_dim].lbnd;
  ssize_t ubnd = dims[current_dim].ubnd;
  const size_t n = (size_t) (ubnd - lbnd) + 1;
  ssize_t inc = dims[current_dim].inc;
  size_t i = start_index;
  if (current_dim == rank - 1)
    {
      result = true;
      size_t j = 0;
      while (result == true && j < n)
        {
          result = nonuniform_pred (elems[i]);
          i += inc;
          j++;
        }
    }
  else
    {
      result = true;
      size_t j = 0;
      while (result == true && j < n)
        {
          result =
            scm_array_dimension_pred (nonuniform_pred, rank, dims, elems,
                                      current_dim + 1, i);
          i += inc;
          j++;
        }
    }
  return result;
}

static bool
scm_array_elements_pred (SCM obj, bool (*nonuniform_pred) (SCM),
                         bool uniform_pred[])
{
  bool result = false;
  if (scm_is_array (obj))
    {
      scm_dynwind_begin (0);
      scm_t_array_handle handle;
      scm_array_get_handle (obj, &handle);
      scm_dynwind_array_handle_release (&handle);
      scm_t_array_type_index i = scm_array_handle_to_array_type_index (&handle);
      if (i == _FF_INDEX_ARRAY_NONUNIFORM)
        {
          const size_t rank = scm_array_handle_rank (&handle);
          const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
          const SCM *elems = scm_array_handle_elements (&handle);
          result =
            scm_array_dimension_pred (nonuniform_pred, rank, dims, elems, 0, 0);
        }
      else
        result = uniform_pred[i];
      scm_dynwind_end ();
    }
  return result;
}

VISIBLE bool
scm_array_handle_is_uniform_array (scm_t_array_handle *handlep)
{
  return
    index_means_uniform_number[scm_array_handle_to_array_type_index (handlep)];
}

VISIBLE bool
scm_is_uniform_array (SCM array)
{
  return (!scm_is_typed_array (array, SCM_BOOL_T));
}

VISIBLE bool
scm_is_uniform_signed_integer_array (SCM obj)
{
  return index_means_uniform_signed_integer[scm_to_array_type_index (obj)];
}

VISIBLE bool
scm_is_uniform_unsigned_integer_array (SCM obj)
{
  return index_means_uniform_unsigned_integer[scm_to_array_type_index (obj)];
}

VISIBLE bool
scm_is_uniform_integer_array (SCM obj)
{
  return index_means_uniform_integer[scm_to_array_type_index (obj)];
}

VISIBLE bool
scm_is_uniform_real_float_array (SCM obj)
{
  return index_means_uniform_real_float[scm_to_array_type_index (obj)];
}

VISIBLE bool
scm_is_uniform_complex_float_array (SCM obj)
{
  return index_means_uniform_complex_float[scm_to_array_type_index (obj)];
}

static bool
scm_is_integer_bool (SCM obj)
{
  return scm_is_integer (obj);
}

VISIBLE bool
scm_is_integer_array (SCM obj)
{
  return scm_array_elements_pred (obj, scm_is_integer_bool,
                                  index_means_uniform_integer);
}

static bool
scm_is_real_bool (SCM obj)
{
  return scm_is_real (obj);
}

VISIBLE bool
scm_is_real_array (SCM obj)
{
  return scm_array_elements_pred (obj, scm_is_real_bool,
                                  index_means_uniform_real);
}

static bool
scm_is_number_bool (SCM obj)
{
  return scm_is_number (obj);
}

VISIBLE bool
scm_is_number_array (SCM obj)
{
  return scm_array_elements_pred (obj, scm_is_number_bool,
                                  index_means_uniform_number);
}

static bool
scm_is_exact_bool (SCM obj)
{
  return scm_is_exact (obj);
}

VISIBLE bool
scm_is_exact_array (SCM obj)
{
  return scm_array_elements_pred (obj, scm_is_exact_bool,
                                  index_means_uniform_integer);
}

static bool
scm_is_inexact_real_bool (SCM obj)
{
  return scm_is_inexact (obj) && scm_is_real (obj);
}

VISIBLE bool
scm_is_inexact_real_array (SCM obj)
{
  return scm_array_elements_pred (obj, scm_is_inexact_real_bool,
                                  index_means_uniform_real_float);
}

//-------------------------------------------------------------------------

VISIBLE void
init_guile_sortsmill_arrays (void)
{
  scm_c_define_gsubr ("uniform-array?", 1, 0, 0, scm_uniform_array_p);
  scm_c_define_gsubr ("uniform-signed-integer-array?", 1, 0, 0,
                      scm_uniform_signed_integer_array_p);
  scm_c_define_gsubr ("uniform-unsigned-integer-array?", 1, 0, 0,
                      scm_uniform_unsigned_integer_array_p);
  scm_c_define_gsubr ("uniform-integer-array?", 1, 0, 0,
                      scm_uniform_integer_array_p);
  scm_c_define_gsubr ("uniform-real-float-array?", 1, 0, 0,
                      scm_uniform_real_float_array_p);
  scm_c_define_gsubr ("uniform-complex-float-array?", 1, 0, 0,
                      scm_uniform_complex_float_array_p);
  scm_c_define_gsubr ("integer-array?", 1, 0, 0, scm_integer_array_p);
  scm_c_define_gsubr ("real-array?", 1, 0, 0, scm_real_array_p);
  scm_c_define_gsubr ("number-array?", 1, 0, 0, scm_number_array_p);
  scm_c_define_gsubr ("exact-array?", 1, 0, 0, scm_exact_array_p);
  scm_c_define_gsubr ("inexact-real-array?", 1, 0, 0, scm_inexact_real_array_p);
}

//-------------------------------------------------------------------------
