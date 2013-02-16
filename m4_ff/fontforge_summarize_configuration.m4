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
  python API         ${i_do_have_python_api}  	${python_url}
  python compat.     ${i_do_have_python_compatibility}
  fortran API        ${i_do_have_fortran_api}
  pure API           ${i_do_have_pure_api}  	${pure_url}
  freetype debugger  ${i_do_have_freetype_debugger}
  capslock for alt   ${i_do_have_capslock_for_alt}
  raw points mode    ${i_do_have_debug_raw_points}
  tile path          ${i_do_have_tile_path}

Optional dependencies:

  giflib             ${i_do_have_giflib}  	${giflib_url}
  libjpeg            ${i_do_have_libjpeg}  	${libjpeg_url}
  libtiff            ${i_do_have_libtiff}  	${libtiff_url}
  libunicodenames    ${i_do_have_libunicodenames}  	${libunicodenames_url}
  X Window System    ${i_do_have_x}
EOF

if test x"${i_do_have_x}" != xyes; then
   cat >> config-summary <<EOF

Because the X Window System is unavailable, the graphical interface
will not be built.

EOF
fi
])
