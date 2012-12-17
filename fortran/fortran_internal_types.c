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
