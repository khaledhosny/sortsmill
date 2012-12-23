/*
 * This is a tiny example of how to write a FontForge menu extension
 * in C.
 *
 * Copying and distribution of this file, with or without
 * modification, are permitted in any medium without royalty. This
 * file is offered as-is, without any warranty.
 *
 *-------------------------------------------------------------------------
 *
 * Install Sorts Mill FontForge. Then compile your extension into a
 * dynamically loadable library -- preferably using autoconf,
 * automake, and libtool, but on GNU/Linux it is likely you can do
 * this:
 *
 *    gcc --std=gnu99 -fPIC -shared simple_menu_extension_in_C.c \
 *          -o simple_menu_extension_in_C.so
 *
 * Add something like the following to
 * ${HOME}/.config/sortsmill-fontforge/user-init.scm:
 *
 * (let ((dll (dynamic-link
 *              "/full/path/to/simple_menu_extension_in_C.so")))
 *
 *   (register-fontforge-menu-entry
 *      #:window 'glyph
 *      #:menu-path '("Tools" "Useless tools"
 *                    "Glyph view extension written in C")
 *      #:action (wrap-ff_menu_entry_action_t
 *                  (dynamic-func "glyph_menu_action" dll)
 *                  (string->pointer
 *                     "This is a glyph view, and the glyph is not 'question'."))
 *      #:enabled (wrap-ff_menu_entry_enabled_t
 *                   (dynamic-func "glyph_menu_enabled" dll)))
 *
 *   (register-fontforge-menu-entry
 *      #:window 'font
 *      #:menu-path '("Tools" "Font view extension written in C")
 *      #:action (wrap-ff_menu_entry_action_t
 *                  (dynamic-func "font_menu_action" dll)
 *                  (string->pointer "This is a font view."))))
 *
 * You do not need use the full path or the .so extension, if you
 * install the shared module somewhere that libltdl can find it. See
 * http://www.gnu.org/software/libtool/manual/html_node/Using-libltdl.html
 *
 * Now run Sorts Mill FontForge from the command line; there should be
 * some new menu entries, and clicking on them should print some stuff
 * to your terminal emulator.
 *
 *-------------------------------------------------------------------------
 */

/*
 * Guile 2.0 uses libltdl to load dynamic modules, and libltdl knows
 * to look for symbols prefixed by "modulename_LTX_". The prefix is
 * optional but helps prevent symbol clashes. See
 * http://www.gnu.org/software/libtool/manual/html_node/Using-libltdl.html
 */
#define glyph_menu_action simple_menu_extension_in_C_LTX_glyph_menu_action
#define glyph_menu_enabled simple_menu_extension_in_C_LTX_glyph_menu_enabled
#define font_menu_action simple_menu_extension_in_C_LTX_font_menu_action

#include <sortsmillff/internal_types.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

void
glyph_menu_action (void *obj, void *data)
{
  void *sc = get_ff_CharViewBase_sc (obj);
  if (sc != NULL)
    {
      void *glyph_name = get_ff_SplineChar_name (sc);
      if (glyph_name != NULL)
        {
          fprintf (stderr, "Glyph name: %s\n", (char *) glyph_name);
          void *parent = get_ff_SplineChar_parent (sc);
          if (parent != NULL && get_ff_SplineFont_font_name (parent) != NULL)
            fprintf (stderr, "Font name: %s\n",
                     (char *) get_ff_SplineFont_font_name (parent));
        }
    }
  char *message = (char *) data;
  fprintf (stderr, "The message: %s\n", message);
}

bool
glyph_menu_enabled (void *obj, void *data)
{
  void *sc = get_ff_CharViewBase_sc (obj);
  void *glyph_name = (sc != NULL) ? get_ff_SplineChar_name (sc) : NULL;
  return (glyph_name != NULL
          && strcmp ((char *) glyph_name, "question") != 0);
}

void
font_menu_action (void *obj, void *data)
{
  void *sf = get_ff_FontViewBase_sf (obj);
  if (sf != NULL && get_ff_SplineFont_font_name (sf) != NULL)
    fprintf (stderr, "Font name: %s\n",
             (char *) get_ff_SplineFont_font_name (sf));
  char *message = (char *) data;
  fprintf (stderr, "The message: %s\n", message);
}
