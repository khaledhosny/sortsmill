! This is a tiny example of how to write a FontForge menu extension
! in Fortran
!
! Copying and distribution of this file, with or without
! modification, are permitted in any medium without royalty. This
! file is offered as-is, without any warranty.
!
!-------------------------------------------------------------------------
!
! Install Sorts Mill FontForge. Then compile your extension into a
! dynamically loadable library -- preferably using autoconf,
! automake, and libtool, but on GNU/Linux it is likely you can do
! this:
!
!    gfortran -fPIC -shared -I/usr/include \
!          alternate_simple_menu_extension_in_Fortran.f90 \
!          -o alternate_simple_menu_extension_in_Fortran.so \
!          -lsortsmillff_fortran_api
!
! Add something like the following to
! ${HOME}/.config/sortsmill-fontforge/user-init.scm:
!
! (let ((dll (dynamic-link
!              "/full/path/to/alternate_simple_menu_extension_in_Fortran.so")))
!
!   (register-fontforge-menu-entry
!      #:window 'glyph
!      #:menu-path '("Tools" "Useless tools"
!                    "Glyph view alternate extension written in Fortran")
!      #:action (wrap-ff_menu_entry_action_t
!                  (dynamic-func "glyph_menu_action" dll)
!                  (string->pointer
!                     "This is a glyph view, and the glyph is not 'question'."))
!      #:enabled (wrap-ff_menu_entry_enabled_t
!                   (dynamic-func "glyph_menu_enabled" dll)))
!
!   (register-fontforge-menu-entry
!      #:window 'font
!      #:menu-path '("Tools" "Font view alternate extension written in Fortran")
!      #:action (wrap-ff_menu_entry_action_t
!                  (dynamic-func "font_menu_action" dll)
!                  (string->pointer "This is a font view."))))
!
! You do not need use the full path or the .so extension, if you
! install the shared module somewhere that libltdl can find it. See
! http://www.gnu.org/software/libtool/manual/html_node/Using-libltdl.html
!
! Now run Sorts Mill FontForge from the command line; there should be
! some new menu entries, and clicking on them should print some stuff
! to your terminal emulator.

module simple_menu_extension_in_Fortran
  use iso_c_binding
  use sortsmillff_internal_types
  implicit none

  interface
     integer(c_size_t) function strlen (s) bind(c)
       import
       type(c_ptr), value, intent(in) :: s
     end function strlen
  end interface

contains

  subroutine glyph_menu_action (obj, data) &
       bind(c, name='simple_menu_extension_in_Fortran_LTX_glyph_menu_action')
    type(c_ptr), value :: obj, data
 
    type(SplineChar) :: sc
    type(c_ptr) :: glyph_name, parent, font_name
    character(len=:), allocatable :: message

    sc = SplineChar (obj)
    glyph_name = .cptr. get_name (sc)
    if (c_associated (glyph_name)) then
       print *, 'Glyph name: ', c_string_to_fortran (glyph_name)
       parent = .cptr. get_parent (sc)
       if (c_associated (parent)) then
          font_name = .cptr. get_font_name (SplineFont (parent))
          if (c_associated (font_name)) then
             print *, 'Font name: ', c_string_to_fortran (font_name)
          end if
       end if
    end if
    message = c_string_to_fortran (data)
    print *, 'The message: ', message
  end subroutine glyph_menu_action

  logical(c_bool) function glyph_menu_enabled (obj, data) result(enabled) &
       bind(c, name='simple_menu_extension_in_Fortran_LTX_glyph_menu_enabled')
    type(c_ptr), value :: obj, data

    type(c_ptr) :: glyph_name

    enabled = .false.
    glyph_name = .cptr. get_name (SplineChar (obj))
    if (c_associated (glyph_name)) then
       enabled = (c_string_to_fortran (glyph_name) /= 'question')
    end if
  end function glyph_menu_enabled

  subroutine font_menu_action (obj, data) &
       bind(c, name='simple_menu_extension_in_Fortran_LTX_font_menu_action')
    type(c_ptr), value :: obj, data

    type(c_ptr) :: sf, font_name
    character(len=:), allocatable :: message

    sf = .cptr. get_sf (FontViewBase (obj))
    if (c_associated (sf)) then
       font_name = .cptr. get_font_name (SplineFont (sf))
       if (c_associated (sf)) then
          print *, 'Font name: ', c_string_to_fortran (font_name)
       end if
    end if
    message = c_string_to_fortran (data)
    print *, 'The message: ', message
  end subroutine font_menu_action

  function c_string_to_fortran (s) result(s_f)
    type(c_ptr), value, intent(in) :: s
    character(len=:), allocatable :: s_f

    character(kind=c_char), dimension(:), pointer :: s_c
    integer :: length, i

    length = strlen (s)
    call c_f_pointer (s, s_c, shape=[length])
    s_f = ''
    do i = 1, length
       s_f = s_f // s_c(i)
    end do
  end function c_string_to_fortran

end module simple_menu_extension_in_Fortran
