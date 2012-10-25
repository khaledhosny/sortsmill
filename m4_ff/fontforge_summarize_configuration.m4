dnl -*- autoconf -*-

dnl FONTFORGE_CREATE_SUMMARY
dnl ------------------------
AC_DEFUN([FONTFORGE_CREATE_SUMMARY],
[
   FONTFORGE_SUMMARIZE_CONFIGURATION
   AC_CONFIG_COMMANDS([summarize],
      [
         if test -r config-summary; then
            echo
            cat config-summary
         fi
      ])
])


dnl FONTFORGE_SUMMARIZE_CONFIGURATION
dnl ---------------------------------
AC_DEFUN([FONTFORGE_SUMMARIZE_CONFIGURATION],
[
cat > config-summary <<EOF
Optional features:

  real (floating pt) ${my_real_type}
  programs           ${i_do_have_programs}
  native scripting   ${i_do_have_native_scripting}
  python scripting   ${i_do_have_python_scripting}
  python extension   ${i_do_have_python_extension}
  freetype debugger  ${i_do_have_freetype_debugger}
  capslock for alt   ${i_do_have_capslock_for_alt}
  raw points mode    ${i_do_have_debug_raw_points}
  tile path          ${i_do_have_tile_path}

Optional dependencies:

  giflib             ${i_do_have_giflib}  	${giflib_url}
  libjpeg            ${i_do_have_libjpeg}  	${libjpeg_url}
  libtiff            ${i_do_have_libtiff}  	${libtiff_url}
  libspiro           ${i_do_have_libspiro}  	${libspiro_url}
  libunicodenames    ${i_do_have_libunicodenames}  	${libunicodenames_url}
  X Window System    ${i_do_have_x}
EOF

if test x"${i_do_have_python_scripting}" = xyes -o x"${i_do_have_python_extension}" = xyes; then

   cat >> config-summary <<EOF
  numpy              ${fontforge_cv_python_numpy}  	${numpy_url}
  numpy.linalg       ${fontforge_cv_python_numpy_linalg}  	${numpy_url}
EOF

   if test x"${fontforge_cv_python_numpy}" != xyes -o  x"${fontforge_cv_python_numpy_linalg}" != xyes; then
      cat >> config-summary <<EOF

Suggestions:

  * Consider installing the Python module 'NumPy' with
    LAPACK support compiled in, for more stable linear
    algebra calculations in Python scripts.
    See ${numpy_url}

EOF
   fi

   echo >> config-summary
fi
])
