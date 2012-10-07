dnl -*- autoconf -*-

dnl FONTFORGE_OBSCURE_CONFIGURATION
dnl -------------------------------
dnl
dnl Obscure configuration options that can be set from environment variables.
dnl
AC_DEFUN([FONTFORGE_OBSCURE_CONFIGURATION],
[
test x"${RECENT_MAX}" = x && RECENT_MAX=10
AC_DEFINE_UNQUOTED([RECENT_MAX],[${RECENT_MAX}],
        [The number of files displayed in the "File->Recent" menu.])

test x"${FORMER_MAX}" = x && FORMER_MAX=10
AC_DEFINE_UNQUOTED([FORMER_MAX],[${FORMER_MAX}],
        [The number of tabs allowed in the outline glyph view of former glyphs.])

test x"${BACK_LAYER_MAX}" = x && BACK_LAYER_MAX=256
AC_DEFINE_UNQUOTED([BACK_LAYER_MAX],[${BACK_LAYER_MAX}],
        [The maximum number of layers allowed in a normal font
         (this includes the default foreground and background layers)
         -- this does not limit type3 fonts.])

test x"${FONTFORGE_CONFIG_APPLE_ONLY_TTF}" = x && FONTFORGE_CONFIG_APPLE_ONLY_TTF=0
AC_DEFINE_UNQUOTED([FONTFORGE_CONFIG_APPLE_ONLY_TTF],
        [${FONTFORGE_CONFIG_APPLE_ONLY_TTF}],
        [Apple suggests using a sfnt version of 'true' for fonts
         designed for use only on a mac (windows refuses such
         fonts). I generally prefer to have a font work everywhere, so
         normally ff produces fonts with version 1.0. Set this to 1 if you
         want Apple only fonts (produced when Apple mode is set and
         Opentype mode is unset in the Generate Fonts-Options
         dialog).])

test x"${FONTFORGE_CONFIG_APPLE_UNICODE_NAMES}" = x && FONTFORGE_CONFIG_APPLE_UNICODE_NAMES=0
AC_DEFINE_UNQUOTED([FONTFORGE_CONFIG_APPLE_UNICODE_NAMES],
        [${FONTFORGE_CONFIG_APPLE_UNICODE_NAMES}],
        [Nobody else puts apple unicode encodings into the name
         table. So I probably shouldn't either.  But, if someone wants
         them, set this to 1.])

test x"${FONTFORGE_CONFIG_BDF_GLYPH_RANGES}" = x && FONTFORGE_CONFIG_BDF_GLYPH_RANGES=0
AC_DEFINE_UNQUOTED([FONTFORGE_CONFIG_BDF_GLYPH_RANGES],
        [${FONTFORGE_CONFIG_BDF_GLYPH_RANGES}],
        [There used to be a property _XFREE86_GLYPH_RANGES (in
         bdf/pcf) fonts which gave a quick view about what glyphs were
         in a bdf font. From what I gather this property has been
         dropped because it was redundant.  If you would like
         FontForge to generate it, set this to 1.])

test x"${FONTFORGE_CONFIG_NON_SYMMETRIC_QUADRATIC_CONVERSION}" = x && FONTFORGE_CONFIG_NON_SYMMETRIC_QUADRATIC_CONVERSION=0
AC_DEFINE_UNQUOTED([FONTFORGE_CONFIG_NON_SYMMETRIC_QUADRATIC_CONVERSION],
        [${FONTFORGE_CONFIG_NON_SYMMETRIC_QUADRATIC_CONVERSION}],
        [I used to use an approximation method when converting cubic
         to quadratic splines which was non-symmetric. In some cases
         it produced better results than the current approach. Set this
         to 1 to restore the old algorithm.])

test x"${FONTFORGE_CONFIG_WRITE_PFM}" = x && FONTFORGE_CONFIG_WRITE_PFM=0
AC_DEFINE_UNQUOTED([FONTFORGE_CONFIG_WRITE_PFM],
        [${FONTFORGE_CONFIG_WRITE_PFM}],
        [Harald Harders would like to be able to generate a PFM file
         without creating a font along with it. I don't see the need
         for this, but he provided a patch. Set this to 1 to
         enable his patch.])

test x"${FONTFORGE_CONFIG_CVT_OLD_MAC_FEATURES}" = x && FONTFORGE_CONFIG_CVT_OLD_MAC_FEATURES=0
AC_DEFINE_UNQUOTED([FONTFORGE_CONFIG_CVT_OLD_MAC_FEATURES],
        [${FONTFORGE_CONFIG_CVT_OLD_MAC_FEATURES}],
        [Prior to late Sept of 2003 FontForge converted certain Mac
         feature/settings into opentype-like tags. Some features could
         be converted directly but for a few I made up tags.  Now
         FontForge is capable of using the Mac feature settings
         directly. If you set this flag to 1, then, when FontForge loads in
         an sfd file with these non-standard opentype tags, it will
         convert them into the appropriate Mac feature/setting
         combinations.])

test x"${FONTFORGE_CONFIG_PS_REFS_GET_SUBRS}" = x && FONTFORGE_CONFIG_PS_REFS_GET_SUBRS=0
AC_DEFINE_UNQUOTED([FONTFORGE_CONFIG_PS_REFS_GET_SUBRS],
        [${FONTFORGE_CONFIG_PS_REFS_GET_SUBRS}],
        [In addition to placing snippets of charstrings into subrs, I
        (George Williams) tried adding whole glyphs (when that was possible).
        To my surprise, it made things worse in one of my test cases, and
        barely registered an improvement in another.  So I think we're
        better off without it. But I don't understand why things are
        worse so I'm leaving the code in to play with. FIXME: This
        might have been due to the sizeof bug that I (Barry Schwartz)
        fixed, so we might turn this feature on someday. To try that now,
        set this variable to 1.])
])
