/*
 * Copyright (C) 2013 Barry Schwartz
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_COPY_WITH_STRIDES_H
#define _SORTSMILL_COPY_WITH_STRIDES_H

#include <sortsmill/guile/arrays.h>
#include <unistd.h>
#include <libguile.h>

#define _FF_COPY_WITH_STRIDES_DECL(NAME, TYPE)          \
  void NAME (ssize_t dest_stride, TYPE *dest,           \
             ssize_t src_stride, const TYPE *src,       \
             size_t count);

_FF_COPY_WITH_STRIDES_DECL (copy_scm_with_strides, SCM);
_FF_COPY_WITH_STRIDES_DECL (copy_u8_with_strides, uint8_t);
_FF_COPY_WITH_STRIDES_DECL (copy_s8_with_strides, int8_t);
_FF_COPY_WITH_STRIDES_DECL (copy_u16_with_strides, uint16_t);
_FF_COPY_WITH_STRIDES_DECL (copy_s16_with_strides, int16_t);
_FF_COPY_WITH_STRIDES_DECL (copy_u32_with_strides, uint32_t);
_FF_COPY_WITH_STRIDES_DECL (copy_s32_with_strides, int32_t);
_FF_COPY_WITH_STRIDES_DECL (copy_u64_with_strides, uint64_t);
_FF_COPY_WITH_STRIDES_DECL (copy_s64_with_strides, int64_t);
_FF_COPY_WITH_STRIDES_DECL (copy_f32_with_strides, float);
_FF_COPY_WITH_STRIDES_DECL (copy_f64_with_strides, double);
_FF_COPY_WITH_STRIDES_DECL (copy_c32_with_strides, float);
_FF_COPY_WITH_STRIDES_DECL (copy_c64_with_strides, double);

void copy_type_indexed_with_strides (scm_t_array_type_index i,
                                     ssize_t dest_stride, void *dest,
                                     ssize_t src_stride, const void *src,
                                     size_t count);
void copy_typed_with_strides (SCM type, ssize_t dest_stride, void *dest,
                              ssize_t src_stride, const void *src,
                              size_t count);

#endif /* _SORTSMILL_COPY_WITH_STRIDES_H */
