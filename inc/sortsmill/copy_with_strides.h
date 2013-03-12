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

#include <unistd.h>
#include <libguile.h>

void copy_f64_with_strides (ssize_t dest_stride, double *dest,
                            ssize_t src_stride, const double *src,
                            size_t count);

void copy_scm_with_strides (ssize_t dest_stride, SCM *dest,
                            ssize_t src_stride, const SCM *src, size_t count);

#endif /* _SORTSMILL_COPY_WITH_STRIDES_H */
