dnl -*- autoconf -*-

dnl FONTFORGE_CONFIG_X_LIBRARIES
dnl ----------------------------
AC_DEFUN([FONTFORGE_CONFIG_X_LIBRARIES],
[
if test x"${i_do_have_x}" = xyes; then
   AC_SEARCH_LIBS([XOpenDisplay],[X11])
   FONTFORGE_SEARCH_LIBS([XOpenDevice],[Xi],
                  [XINPUT_LIBS="${found_lib}"],
                  [AC_DEFINE(_NO_XINPUT,1,[Define if not using xinput.])],
                  [$X_LIBS $X_PRE_LIBS $X_EXTRA_LIBS])
   FONTFORGE_SEARCH_LIBS([XkbQueryExtension],[xkbui],
                  [XKB_LIBS="${found_lib}"],
                  [AC_DEFINE(_NO_XKB,1,[Define if not using xkb.])],
                  [$X_LIBS $X_PRE_LIBS $X_EXTRA_LIBS])
fi
AC_SUBST([XINPUT_LIBS])
AC_SUBST([XKB_LIBS])
])
