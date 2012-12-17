#include <config.h>

// Copyright (C) 2012 Barry Schwartz
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

#include <stdint.h>

void *_FF_FORTRAN_c_intptr_t_to_c_ptr (intptr_t);
void *_FF_FORTRAN_c_intptr_t_to_c_funptr(intptr_t);
intptr_t _FF_FORTRAN_c_ptr_to_c_intptr_t (void *);
intptr_t _FF_FORTRAN_c_funptr_to_c_intptr_t (void *);
void *_FF_FORTRAN_c_ptr_plus_offset (void *, int);
intptr_t _FF_FORTRAN_c_int8_t_to_c_intptr_t (int8_t);
intptr_t _FF_FORTRAN_c_int16_t_to_c_intptr_t (int16_t);
intptr_t _FF_FORTRAN_c_int32_t_to_c_intptr_t (int32_t);
intptr_t _FF_FORTRAN_c_int64_t_to_c_intptr_t (int64_t);
int8_t _FF_FORTRAN_c_intptr_t_to_c_int8_t (intptr_t p);
int16_t _FF_FORTRAN_c_intptr_t_to_c_int16_t (intptr_t p);
int32_t _FF_FORTRAN_c_intptr_t_to_c_int32_t (intptr_t p);
int64_t _FF_FORTRAN_c_intptr_t_to_c_int64_t (intptr_t p);

VISIBLE void *
_FF_FORTRAN_c_intptr_t_to_c_ptr (intptr_t p)
{
  return (void *) p;
}

VISIBLE void *
_FF_FORTRAN_c_intptr_t_to_c_funptr (intptr_t p)
{
  return (void *) p;
}

VISIBLE intptr_t
_FF_FORTRAN_c_ptr_to_c_intptr_t (void *p)
{
  return (intptr_t) p;
}

VISIBLE intptr_t
_FF_FORTRAN_c_funptr_to_c_intptr_t (void *p)
{
  return (intptr_t) p;
}

VISIBLE void *
_FF_FORTRAN_c_ptr_plus_offset (void *p, int offset)
{
  return (void *) ((char *) p + offset);
}

VISIBLE intptr_t
_FF_FORTRAN_c_int8_t_to_c_intptr_t (int8_t i)
{
  return (intptr_t) (uintptr_t) (uint8_t) i;
}

VISIBLE intptr_t
_FF_FORTRAN_c_int16_t_to_c_intptr_t (int16_t i)
{
  return (intptr_t) (uintptr_t) (uint16_t) i;
}

VISIBLE intptr_t
_FF_FORTRAN_c_int32_t_to_c_intptr_t (int32_t i)
{
  return (intptr_t) (uintptr_t) (uint32_t) i;
}

VISIBLE intptr_t
_FF_FORTRAN_c_int64_t_to_c_intptr_t (int64_t i)
{
  return (intptr_t) (uintptr_t) (uint64_t) i;
}

VISIBLE int8_t
_FF_FORTRAN_c_intptr_t_to_c_int8_t (intptr_t p)
{
  return (int8_t) (uint8_t) (uintptr_t) p;
}

VISIBLE int16_t
_FF_FORTRAN_c_intptr_t_to_c_int16_t (intptr_t p)
{
  return (int16_t) (uint16_t) (uintptr_t) p;
}

VISIBLE int32_t
_FF_FORTRAN_c_intptr_t_to_c_int32_t (intptr_t p)
{
  return (int32_t) (uint32_t) (uintptr_t) p;
}

VISIBLE int64_t
_FF_FORTRAN_c_intptr_t_to_c_int64_t (intptr_t p)
{
  return (int64_t) (uint64_t) (uintptr_t) p;
}
