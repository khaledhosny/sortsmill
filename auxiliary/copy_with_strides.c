#include <config.h>             // -*- coding: utf-8 -*-

// Copyright (C) 2013 by Barry Schwartz
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

#include <sortsmill/copy_with_strides.h>

VISIBLE void
copy_f64_with_strides (ssize_t dest_stride, double *dest,
                       ssize_t src_stride, const double *src, size_t count)
{
  for (size_t k = 0; k < count; k++)
    {
      *dest = *src;
      dest += dest_stride;
      src += src_stride;
    }
}

VISIBLE void
copy_scm_with_strides (ssize_t dest_stride, SCM *dest,
                       ssize_t src_stride, const SCM *src, size_t count)
{
  for (size_t k = 0; k < count; k++)
    {
      *dest = *src;
      dest += dest_stride;
      src += src_stride;
    }
}
