dnl -*- autoconf -*-

dnl FONTFORGE_CREATE_INDENT_PRO
dnl ---------------------------
dnl Make build tree symlinks to the Indent profile.
dnl
AC_DEFUN([FONTFORGE_CREATE_INDENT_PRO],
[
   AC_CONFIG_COMMANDS([.indent.pro],
   [
      if test "${ac_srcdir}" -ef "${ac_builddir}"; then
         :
      else
         for dir in . inc auxiliary gutils gdraw fontforge scheme pyhook tests fonttools; do
             rm -f "${ac_builddir}/${dir}/.indent.pro"
             (cd "${ac_builddir}/${dir}" && ${LN_S} "${ac_srcdir}"/.indent.pro .indent.pro)
         done
      fi
   ])
])
