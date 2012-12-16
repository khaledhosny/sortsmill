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

module sortsmillff_usermenu
  use iso_c_binding
  use sortsmillff_views
  implicit none

  ! FIXME: These may not be necessary, because we can recommend
  ! registration of Fortran procedures be done through C or Guile.
  integer, parameter, public ::            &
       FF_FONT_WINDOW  = b'000001',        &
       FF_GLYPH_WINDOW = b'000010',        &
       FF_CHAR_WINDOW  = FF_GLYPH_WINDOW
  
  abstract interface
     subroutine ff_menu_entry_action_t (obj, data) bind(c)
       import
       type(c_ptr) :: obj
       type(c_ptr) :: data
     end subroutine ff_menu_entry_action_t

     function ff_menu_entry_enabled_t (obj, data) result(enab) bind(c)
       import
       type(c_ptr) :: obj
       type(c_ptr) :: data
       logical(c_bool) :: enab
     end function ff_menu_entry_enabled_t
  end interface

end module sortsmillff_usermenu
