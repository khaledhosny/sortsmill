dnl -*- autoconf -*-


dnl FONTFORGE_ICONV_CHOOSE_ENCODING(var, encoding1 encoding2 ...)
dnl -------------------------------------------------------------
AC_DEFUN([FONTFORGE_ICONV_CHOOSE_ENCODING],
[
    if test x"[$]{$1}" != x; then
       AS_TR_SH(fontforge_cv_lib_iconv_open_name_$1)="[$]{$1}"
    fi
    FONTFORGE_ICONV_TRY_ENCODINGS([$1],[$2])
    if test x"[$]{$1}" = x; then
       AC_MSG_ERROR([

Failed to find an encoding name for $1
that iconv_open(3) recognizes.

You can try setting the $1 environment variable
to an appropriate name. Consider using GNU libiconv instead.
])
    fi
    AC_DEFINE_UNQUOTED([$1],[[$]{$1}],[Define to an encoding recognized by iconv_open(3).])
    AC_DEFINE_UNQUOTED([$1_STRING],[[$]{$1_STRING}],
        [Define to a C string representation of an encoding recognized by iconv_open(3).])
    AC_SUBST([$1])
    AC_SUBST([$1_STRING])
])


dnl FONTFORGE_ICONV_TRY_ENCODINGS(var, encoding1 encoding2 ...)
dnl ----------------------------------------xs-------------------
AC_DEFUN([FONTFORGE_ICONV_TRY_ENCODINGS],
[
__encoding=""
m4_foreach_w([__enc],[$2],
   [
      if test x"${__encoding}" = x; then
         FONTFORGE_ICONV_SUPPORTS_ENCODING(__enc)
         if test x"${AS_TR_SH([fontforge_cv_lib_iconv_open_supports_]__enc)}" = xyes; then
            __encoding='__enc'
         fi
      fi
   ])
AC_CACHE_CHECK([names iconv_open(3) might recognize for $1],
               [AS_TR_SH(fontforge_cv_lib_iconv_open_name_$1)],
[
   AS_TR_SH(fontforge_cv_lib_iconv_open_name_$1)="${__encoding}"
])
$1="${AS_TR_SH(fontforge_cv_lib_iconv_open_name_$1)}"
$1_STRING="\"${AS_TR_SH(fontforge_cv_lib_iconv_open_name_$1)}\""
])


dnl FONTFORGE_ICONV_SUPPORTS_ENCODING(encoding)
dnl -------------------------------------------
dnl Test if iconv_open(3) recognizes a given encoding.
AC_DEFUN([FONTFORGE_ICONV_SUPPORTS_ENCODING],
[
AM_ICONV
__cflags="${CFLAGS} ${INCICONV}"
__libs="${LIBS} ${LIBICONV}"
AC_LANG_PUSH([C])
AC_CACHE_CHECK([whether iconv_open(3) supports $1],
               [AS_TR_SH([fontforge_cv_lib_iconv_open_supports_$1])],
[AC_RUN_IFELSE(
        [AC_LANG_PROGRAM(
                [#include <iconv.h>],
                [
/* We assume that conversion to/from ISO-8859-1 is supported. */
if (iconv_open("ISO-8859-1", "$1") == -1) return 1;
if (iconv_open("$1", "ISO-8859-1") == -1) return 1;
return 0;
])],
        [AS_TR_SH([fontforge_cv_lib_iconv_open_supports_$1])=yes],
        [AS_TR_SH([fontforge_cv_lib_iconv_open_supports_$1])=no])])
AC_LANG_POP
CFLAGS="${__cflags}"
LIBS="${__libs}"
])


dnl FONTFORGE_ICONV_TOCODE_SUFFIX(macro-name, suffix, tocode, fromcode)
dnl -------------------------------------------------------------------
dnl Test if iconv_open(3) recognizes a tocode-suffix, such as
dnl "//TRANSLIT" or "//IGNORE", and define programming items
dnl accordingly
AC_DEFUN([FONTFORGE_ICONV_TOCODE_SUFFIX],
[
AM_ICONV
__cflags="${CFLAGS} ${INCICONV}"
__libs="${LIBS} ${LIBICONV}"
AC_LANG_PUSH([C])
AC_CACHE_CHECK([whether iconv_open(3) recognizes the suffix $2],
               [AS_TR_SH([fontforge_cv_lib_iconv_open_suffix_$1])],
[AC_RUN_IFELSE(
        [AC_LANG_PROGRAM(
                [#include <iconv.h>],
                [
if (iconv_open("$3$2", "$4") == -1) return 1;
return 0;
])],
        [AS_TR_SH([fontforge_cv_lib_iconv_open_suffix_$1])=yes],
        [AS_TR_SH([fontforge_cv_lib_iconv_open_suffix_$1])=no])])
AC_LANG_POP
CFLAGS="${__cflags}"
LIBS="${__libs}"

if test x"${AS_TR_SH([fontforge_cv_lib_iconv_open_suffix_$1])}" = xyes; then
   __macro='$2'
   __string_macro='"$2"'
else
   __macro=
   __string_macro='""'
fi

AC_DEFINE_UNQUOTED([$1],[[$]{__macro}],
   [Define as $2 if that suffix is recognized by iconv_open(3);
    otherwise define as empty.])
AC_DEFINE_UNQUOTED([$1_STRING],[[$]{__string_macro}],
   [Define as "$2" if that suffix is recognized by iconv_open(3);
    otherwise define as "".])

AC_SUBST([$1],[[$]{__macro}])
AC_SUBST([$1_STRING],[[$]{__string_macro}])

$1="${__macro}"
$1_STRING="${__string_macro}"
])
