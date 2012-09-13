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
  raw points mode    ${i_do_have_capslock_for_alt}
  tile path          ${i_do_have_tile_path}

Optional dependencies:

  cairo              ${i_do_have_cairo}  	${cairo_url}
  pango              ${i_do_have_pango}  	${pango_url}
  freetype           ${i_do_have_freetype}  	${freetype_url}
  giflib             ${i_do_have_giflib}  	${giflib_url}
  libjpeg            ${i_do_have_libjpeg}  	${libjpeg_url}
  libpng             ${i_do_have_libpng}  	${libpng_url}
  libtiff            ${i_do_have_libtiff}  	${libtiff_url}
  libxml             ${i_do_have_libxml}  	${libxml_url}
  libspiro           ${i_do_have_libspiro}  	${libspiro_url}
  libunicodenames    ${i_do_have_libunicodenames}  	${libunicodenames_url}
  X Window System    ${i_do_have_x}
EOF
])
