dnl -*- autoconf -*-

dnl FONTFORGE_PLATFORM_SPECIFICS
dnl ----------------------------
AC_DEFUN([FONTFORGE_PLATFORM_SPECIFICS],
[
AC_CANONICAL_HOST

#  FontForge knows about 4 different keyboard settings, a windows keyboard, a
#   mac keyboard, a mac keyboard under SUSE linux, and a sun keyboard
#   When it starts up FontForge assumes that the keyboard is some default type
#   You can override the type by setting _Keyboard to
#  0 -- windows
#  1 -- mac running mac osx
#  3 -- mac running SUSE linux (7.1)
#  2 -- sparc
#  Basically this affects the text that appears in menus. The sun keyboard
#   uses meta where the windows one uses alt, and the mac use command and
#   option.

AS_CASE([$host],
   [*-apple-darwin*],[
      gww_ismac="yes"
      AC_DEFINE([__Mac],[1],[If you are on a Mac then define __Mac to 1.])
      AC_DEFINE([_Keyboard],[1],[Define this to platform-specific keyboard setting number,
                                 if appropriate.])
      gww_define_caps_for_alt="1"
   ],

   [powerpc-*-*linux*],[AC_DEFINE([_Keyboard],[3])],

   [*sparc*],[AC_DEFINE([_Keyboard],[2])],

   [:]  dnl DEFAULT AS_CASE

) dnl END AS_CASE
])
