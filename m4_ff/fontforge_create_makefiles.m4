dnl -*- autoconf -*-

dnl FONTFORGE_CREATE_MAKEFILES
dnl --------------------------
AC_DEFUN([FONTFORGE_CREATE_MAKEFILES],
[
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([lib/Makefile])
AC_CONFIG_FILES([inc/Makefile])
AC_CONFIG_FILES([auxiliary/Makefile])
AC_CONFIG_FILES([gutils/Makefile])
AC_CONFIG_FILES([gdraw/Makefile])
AC_CONFIG_FILES([fontforge/Makefile])
AC_CONFIG_FILES([po/Makefile.in])
AC_CONFIG_FILES([mackeys/Makefile])
AC_CONFIG_FILES([doc/Makefile])
AC_CONFIG_FILES([htdocs/Makefile])
AC_CONFIG_FILES([pycontrib/Makefile])
AC_CONFIG_FILES([pyhook/Makefile])
AC_CONFIG_FILES([cidmap/Makefile])
AC_CONFIG_FILES([tests/Makefile])
AC_CONFIG_FILES([data/Makefile])
AC_CONFIG_FILES([data/pixmaps/Makefile])
AC_CONFIG_FILES([fonttools/Makefile])
AC_CONFIG_FILES([scheme/Makefile])

# Snippets for use with "include".
AC_CONFIG_FILES([mk/xgettext_search.mk])
])
