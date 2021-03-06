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
         for dir in . inc inc/sortsmill inc/sortsmill/guile \
                 auxiliary gutils gdraw fontforge guile main    \
                 python/sortsmill fortran pure tests          \
                 fonttools
         do
             AS_MKDIR_P(["${ac_builddir}/${dir}"])
             rm -f "${ac_builddir}/${dir}/.indent.pro"
             (cd "${ac_builddir}/${dir}" && ${LN_S} "${ac_srcdir}"/.indent.pro .indent.pro)
         done
      fi
   ])
])
