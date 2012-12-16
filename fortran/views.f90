! -*- mode: F90 -*-

! Copyright (C) 2012 Barry Schwartz
! 
! This program is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3 of the License, or
! (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, see <http://www.gnu.org/licenses/>.

module sortsmillff_views
  use iso_c_binding
  implicit none

  type font_view
     type(c_ptr) :: p
  end type font_view

  type glyph_view
     type(c_ptr) :: p
  end type glyph_view

end module sortsmillff_views
