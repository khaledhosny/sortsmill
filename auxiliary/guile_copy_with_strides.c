#include <config.h>             // -*- coding: utf-8 -*-

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
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

#include <sortsmill/guile.h>
#include <sortsmill/core.h>
#include <intl.h>
#include <assert.h>

VISIBLE void
copy_scm_with_strides (ssize_t dest_stride,
                       SCM *dest,
                       ssize_t src_stride,
                       const SCM *src,
                       size_t count)
{
  COPY_WITH_STRIDES (SCM, dest, src, dest_stride, src_stride, count);
};

#define _FF_VOID_COPY_WITH_STRIDES(ELEMTYPE, TYPE)                      \
  void                                                                  \
  _void_copy_##ELEMTYPE##_with_strides (ssize_t dest_stride,            \
                                        void *dest,                     \
                                        ssize_t src_stride,             \
                                        const void *src,                \
                                        size_t count)                   \
  {                                                                     \
    copy_##ELEMTYPE##_with_strides (dest_stride, (TYPE *) dest,         \
                                    src_stride, (const TYPE *) src,     \
                                    count);                             \
  }

static inline _FF_VOID_COPY_WITH_STRIDES (scm, SCM);
static inline _FF_VOID_COPY_WITH_STRIDES (u8, uint8_t);
static inline _FF_VOID_COPY_WITH_STRIDES (s8, int8_t);
static inline _FF_VOID_COPY_WITH_STRIDES (u16, uint16_t);
static inline _FF_VOID_COPY_WITH_STRIDES (s16, int16_t);
static inline _FF_VOID_COPY_WITH_STRIDES (u32, uint32_t);
static inline _FF_VOID_COPY_WITH_STRIDES (s32, int32_t);
static inline _FF_VOID_COPY_WITH_STRIDES (u64, uint64_t);
static inline _FF_VOID_COPY_WITH_STRIDES (s64, int64_t);
static inline _FF_VOID_COPY_WITH_STRIDES (f32, float);
static inline _FF_VOID_COPY_WITH_STRIDES (f64, double);
static inline _FF_VOID_COPY_WITH_STRIDES (c32, float);
static inline _FF_VOID_COPY_WITH_STRIDES (c64, double);

VISIBLE void
copy_type_indexed_with_strides (scm_t_array_type_index i,
                                ssize_t dest_stride, void *dest,
                                ssize_t src_stride, const void *src,
                                size_t count)
{
  switch (i)
    {
    case _FF_INDEX_ARRAY_NONUNIFORM:
      _void_copy_scm_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_U8:
      _void_copy_u8_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_S8:
      _void_copy_s8_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_U16:
      _void_copy_u16_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_S16:
      _void_copy_s16_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_U32:
      _void_copy_u32_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_S32:
      _void_copy_s32_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_U64:
      _void_copy_u64_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_S64:
      _void_copy_s64_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_F32:
      _void_copy_f32_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_F64:
      _void_copy_f64_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_C32:
      _void_copy_c32_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    case _FF_INDEX_ARRAY_C64:
      _void_copy_c64_with_strides (dest_stride, dest, src_stride, src, count);
      break;

    default:
      assert (false);
    }
}

VISIBLE void
copy_typed_with_strides (SCM type, ssize_t dest_stride, void *dest,
                         ssize_t src_stride, const void *src, size_t count)
{
  scm_t_array_type_index i = scm_array_type_to_array_type_index (type);
  if (i == _FF_INDEX_NOT_AN_ARRAY)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("copy_typed_with_strides"),
        rnrs_c_make_message_condition (_("not a valid array type")),
        rnrs_make_irritants_condition (scm_list_1 (type))));
  copy_type_indexed_with_strides (i, dest_stride, dest, src_stride, src, count);
}
