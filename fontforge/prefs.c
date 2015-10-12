#include <config.h>             /* -*- coding: utf-8 -*- */

// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

/* Copyright (C) 2000-2012 by George Williams */
/*
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.

 * The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "fontforgeui.h"
#include "groups.h"
#include <gfile.h>
#include <gresource.h>
#include <ustring.h>
#include <gkeysym.h>

#include <sys/types.h>
#include <dirent.h>
#include <locale.h>
#include <time.h>
#include <sys/time.h>
#include <stdlib.h>
#include <xstriconv.h>
#include <xunistring.h>
#include <sortsmill/core.h>

#include "ttf.h"

#ifndef DOCDIR
#error You must define DOCDIR.
#endif

#ifndef SHAREDIR
#error You must define SHAREDIR.
#endif

#define RAD2DEG	(180/3.1415926535897932)

extern int adjustwidth;
extern int adjustlbearing;
extern Encoding *default_encoding;
extern int autohint_before_generate;
extern int OpenCharsInNewWindow;
extern int ItalicConstrained;
extern int accent_offset;
extern int GraveAcuteCenterBottom;
extern int PreferSpacingAccents;
extern int CharCenterHighest;
extern int ask_user_for_resolution;
extern int stop_at_join;
extern int cv_auto_goto;
extern int recognizePUA;
extern float arrowAmount;
extern float arrowAccelFactor;
extern float snapdistance;
extern int snaptoint;
extern float joinsnap;
extern char *BDFFoundry;
extern char *TTFFoundry;
extern char *xuid;
extern char *SaveTablesPref;

extern int default_fv_row_count;        /* in fontview */
extern int default_fv_col_count;        /* in fontview */
extern int default_fv_showhmetrics;     /* in fontview */
extern int default_fv_showvmetrics;     /* in fontview */
extern int default_fv_glyphlabel;       /* in fontview */
extern int save_to_dir;         /* in fontview, use sfdir rather than sfd */
extern int palettes_docked;     /* in cvpalettes */
extern int cvvisible[2], bvvisible[3];  /* in cvpalettes.c */
extern int maxundoes;           /* in cvundoes */
extern int pref_mv_shift_and_arrow_skip;        /* in metricsview.c */
extern int pref_mv_control_shift_and_arrow_skip;        /* in metricsview.c */
extern int prefer_cjk_encodings;        /* in parsettf */
extern int onlycopydisplayed, copymetadata, copyttfinstr;
extern struct cvshows CVShows;
extern int infowindowdistance;  /* in cvruler.c */
extern int oldformatstate;      /* in generatefontdlg.c */
extern int oldbitmapstate;      /* in generatefontdlg.c */
static int old_ttf_flags = 0, old_otf_flags = 0;
extern int old_sfnt_flags;      /* in generatefont.c */
extern int old_ps_flags;        /* in generatefont.c */
extern int old_validate;        /* in generatefontdlg.c */
extern int old_fontlog;         /* in generatefontdlg.c */
extern int oldsystem;           /* in bitmapdlg.c */
extern int preferpotrace;       /* in autotrace.c */
extern int autotrace_ask;       /* in autotrace.c */
extern int mf_ask;              /* in autotrace.c */
extern int mf_clearbackgrounds; /* in autotrace.c */
extern int mf_showerrors;       /* in autotrace.c */
extern char *mf_args;           /* in autotrace.c */
static int glyph_2_name_map = 0;        /* was in tottf.c, now a flag in generatefont options dlg */
extern int coverageformatsallowed;      /* in tottfgpos.c */
extern int debug_wins;          /* in cvdebug.c */
extern int gridfit_dpi, gridfit_depth;  /* in cvgridfit.c */
extern float gridfit_pointsizey;        /* in cvgridfit.c */
extern float gridfit_pointsizex;        /* in cvgridfit.c */
extern int gridfit_x_sameas_y;  /* in cvgridfit.c */
extern int hint_diagonal_ends;  /* in stemdb.c */
extern int hint_diagonal_intersections; /* in stemdb.c */
extern int hint_bounding_boxes; /* in stemdb.c */
extern int detect_diagonal_stems;       /* in stemdb.c */
extern float stem_slope_error;  /* in stemdb.c */
extern float stub_slope_error;  /* in stemdb.c */
extern int instruct_diagonal_stems;     /* in nowakowskittfinstr.c */
extern int instruct_serif_stems;        /* in nowakowskittfinstr.c */
extern int instruct_ball_terminals;     /* in nowakowskittfinstr.c */
extern int interpolate_strong;  /* in nowakowskittfinstr.c */
extern int control_counters;    /* in nowakowskittfinstr.c */
extern uint32_t *script_menu_names[SCRIPT_MENU_MAX];
extern char *script_filenames[SCRIPT_MENU_MAX];
static char *xdefs_filename;
extern int new_em_size;         /* in splineutil2.c */
extern int new_fonts_are_order2;        /* in splineutil2.c */
extern int loaded_fonts_same_as_new;    /* in splineutil2.c */
extern int use_second_indic_scripts;    /* in tottfgpos.c */
extern char *helpdir;           /* in uiutil.c */
static char *othersubrsfile = NULL;
extern int updateflex;          /* in charview.c */
extern int default_autokern_dlg;        /* in lookupui.c */
extern int allow_utf8_glyphnames;       /* in lookupui.c */
extern int clear_tt_instructions_when_needed;   /* in cvundoes.c */
extern int export_clipboard;    /* in cvundoes.c */
extern int default_cv_width;    /* in charview.c */
extern int default_cv_height;   /* in charview.c */
extern int interpCPsOnMotion;   /* in charview.c */
extern int mv_width;            /* in metricsview.c */
extern int mv_height;           /* in metricsview.c */
extern int bv_width;            /* in bitmapview.c */
extern int bv_height;           /* in bitmapview.c */
extern int ask_user_for_cmap;   /* in parsettf.c */
extern int mvshowgrid;          /* in metricsview.c */

extern int rectelipse, polystar, regular_star;  /* from cvpalettes.c */
extern int center_out[2];       /* from cvpalettes.c */
extern float rr_radius;         /* from cvpalettes.c */
extern int ps_pointcnt;         /* from cvpalettes.c */
extern float star_percent;      /* from cvpalettes.c */
extern int home_char;           /* from fontview.c */
extern int compact_font_on_open;        /* from fontview.c */
extern int aa_pixelsize;        /* from anchorsaway.c */
extern enum cvtools cv_b1_tool, cv_cb1_tool, cv_b2_tool, cv_cb2_tool;   /* cvpalettes.c */
extern int show_kerning_pane_in_class;  /* kernclass.c */
extern int AutoSaveFrequency;   /* autosave.c */

extern NameList *force_names_when_opening;
extern NameList *force_names_when_saving;
extern NameList *namelist_for_new_fonts;

extern int default_font_filter_index;
extern struct openfilefilters *user_font_filters;
static int alwaysgenapple = false, alwaysgenopentype = false;

static int gfc_showhidden, gfc_dirplace;
static char *gfc_bookmarks = NULL;

static int pointless;

#define CID_ScriptMNameBase	200
#define CID_ScriptMFileBase	(200+SCRIPT_MENU_MAX)
#define CID_ScriptMBrowseBase	(200+2*SCRIPT_MENU_MAX)

#define CID_PrefsBase	1000
#define CID_PrefsOffset	100
#define CID_PrefsBrowseOffset	(CID_PrefsOffset/2)

/**************************************************************************** */


/* don't use mnemonics 'C' or 'O' (Cancel & OK) */
enum pref_types
{
  pr_int,
  pr_real,
  pr_bool,
  pr_enum,
  pr_encoding,
  pr_string,
  pr_file,
  pr_namelist,
  pr_unicode,
  pr_angle
};

struct enums
{
  char *name;
  int value;
};

struct enums fvsize_enums[] = { {NULL, 0} };

#define PREFS_LIST_EMPTY { NULL, 0, NULL, NULL, NULL, '\0', NULL, 0, NULL }
static struct prefs_list
{
  char *name;
  /* In the prefs file the untranslated name will always be used, but */
  /* in the UI that name may be translated. */
  enum pref_types type;
  void *val;
  void *(*get) (void);
  void (*set) (void *);
  char mn;
  struct enums *enums;
  bool dontdisplay;
  char *popup;
} general_list[] =
{
/* TRANSLATORS: The following strings have no spaces and an odd capitalization */
/* this is because these strings are used in two different ways, one */
/* translated (which the user sees, and should probably have added spaces,*/
/* and one untranslated which needs the current odd format */
  {
  N_("ResourceFile"), pr_file, &xdefs_filename, NULL, NULL, 'R', NULL, 0,
      N_
      ("When FontForge starts up, it loads display related resources from a\nproperty on the screen. Sometimes it is useful to be able to store\nthese resources in a file. These resources are only read at start\nup, so changing this has no effect until the next time you start\nFontForge.")},
#if 0
  {
  N_("PixmapDir"), pr_file, &pixmapdir, NULL, NULL, 'R', NULL, 0,
      N_("As FontForge creates windows, it loads images for its menus\n"
           "from files in a standard directory. You may change this to\n"
           "point to a different directory to load a different icon set.\n"
           "(If you want no icons at all, change to an empty directory).\n"
           "This may not effect windows of a type that is already initialized,\n"
           "restarting FontForge will fix that.")},
#endif
  {
  N_("OtherSubrsFile"), pr_file, &othersubrsfile, NULL, NULL, 'O', NULL, 0,
      N_
      ("If you wish to replace Adobe's OtherSubrs array (for Type1 fonts)\nwith an array of your own, set this to point to a file containing\na list of up to 14 PostScript subroutines. Each subroutine must\nbe preceded by a line starting with '%%%%' (any text before the\nfirst '%%%%' line will be treated as an initial copyright notice).\nThe first three subroutines are for flex hints, the next for hint\nsubstitution (this MUST be present), the 14th (or 13 as the\nnumbering actually starts with 0) is for counter hints.\nThe subroutines should not be enclosed in a [ ] pair.")},
  {
  N_("ExportClipboard"), pr_bool, &export_clipboard, NULL, NULL, '\0', NULL,
      0,
      N_
      ("If you are running an X11 clipboard manager you might want\nto turn this off. FF can put things into its internal clipboard\nwhich it cannot export to X11 (things like copying more than\none glyph in the fontview). If you have a clipboard manager\nrunning it will force these to be exported with consequent\nloss of data.")},
  {
N_("AutoSaveFrequency"), pr_int, &AutoSaveFrequency, NULL, NULL, '\0',
      NULL, 0,
      N_
      ("The number of seconds between autosaves. If you set this to 0 there will be no autosaves.")},
    PREFS_LIST_EMPTY}, new_list[] =
{
  {
  N_("NewCharset"), pr_encoding, &default_encoding, NULL, NULL, 'N', NULL,
      0, N_("Default encoding for\nnew fonts")},
  {
  N_("NewEmSize"), pr_int, &new_em_size, NULL, NULL, 'S', NULL, 0,
      N_("The default size of the Em-Square in a newly created font.")},
  {
  N_("NewFontsQuadratic"), pr_bool, &new_fonts_are_order2, NULL, NULL, 'Q',
      NULL, 0,
      N_
      ("Whether new fonts should contain splines of quadratic (truetype)\nor cubic (postscript & opentype).")},
  {
N_("LoadedFontsAsNew"), pr_bool, &loaded_fonts_same_as_new, NULL, NULL,
      'L', NULL, 0,
      N_
      ("Whether fonts loaded from the disk should retain their splines\nwith the original order (quadratic or cubic), or whether the\nsplines should be converted to the default order for new fonts\n(see NewFontsQuadratic).")},
    PREFS_LIST_EMPTY}, open_list[] =
{
  {
  N_("PreferCJKEncodings"), pr_bool, &prefer_cjk_encodings, NULL, NULL, 'C',
      NULL, 0,
      N_
      ("When loading a truetype or opentype font which has both a unicode\nand a CJK encoding table, use this flag to specify which\nshould be loaded for the font.")},
  {
  N_("AskUserForCMap"), pr_bool, &ask_user_for_cmap, NULL, NULL, 'O', NULL,
      0,
      N_
      ("When loading a font in sfnt format (TrueType, OpenType, etc.),\nask the user to specify which cmap to use initially.")},
  {
  N_("PreserveTables"), pr_string, &SaveTablesPref, NULL, NULL, 'P', NULL,
      0,
      N_
      ("Enter a list of 4 letter table tags, separated by commas.\nFontForge will make a binary copy of these tables when it\nloads a True/OpenType font, and will output them (unchanged)\nwhen it generates the font. Do not include table tags which\nFontForge thinks it understands.")},
  {
  N_("SeekCharacter"), pr_unicode, &home_char, NULL, NULL, '\0', NULL, 0,
      N_
      ("When fontforge opens a (non-sfd) font it will try to display this unicode character in the fontview.")},
  {
N_("CompactOnOpen"), pr_bool, &compact_font_on_open, NULL, NULL, 'O',
      NULL, 0, N_("When a font is opened, should it be made compact?")},
    PREFS_LIST_EMPTY}, navigation_list[] =
{
  {
  N_("GlyphAutoGoto"), pr_bool, &cv_auto_goto, NULL, NULL, '\0', NULL, 0,
      N_
      ("Typing a normal character in the glyph view window changes the window to look at that character.\nEnabling GlyphAutoGoto will disable the shortcut where holding just the ` key will enable Preview mode as long as the key is held.")},
  {
N_("OpenCharsInNewWindow"), pr_bool, &OpenCharsInNewWindow, NULL, NULL,
      '\0', NULL, 0,
      N_
      ("When double clicking on a character in the font view\nopen that character in a new window, otherwise\nreuse an existing one.")},
    PREFS_LIST_EMPTY}, editing_list[] =
{
  {
  N_("ItalicConstrained"), pr_bool, &ItalicConstrained, NULL, NULL, '\0',
      NULL, 0,
      N_
      ("In the Outline View, the Shift key constrains motion to be parallel to the ItalicAngle rather than constraining it to be vertical.")},
  {
  N_("ArrowMoveSize"), pr_real, &arrowAmount, NULL, NULL, '\0', NULL, 0,
      N_
      ("The number of em-units by which an arrow key will move a selected point")},
  {
  N_("ArrowAccelFactor"), pr_real, &arrowAccelFactor, NULL, NULL, '\0',
      NULL, 0,
      N_
      ("Holding down the Alt (or Meta) key will speed up arrow key motion by this factor")},
  {
  N_("InterpolateCPsOnMotion"), pr_bool, &interpCPsOnMotion, NULL, NULL,
      '\0', NULL, 0,
      N_
      ("When moving one end point of a spline but not the other\ninterpolate the control points between the two.")},
  {
  N_("SnapDistance"), pr_real, &snapdistance, NULL, NULL, '\0', NULL, 0,
      N_
      ("When the mouse pointer is within this many pixels\nof one of the various interesting features (baseline,\nwidth, grid splines, etc.) the pointer will snap\nto that feature.")},
  {
  N_("SnapToInt"), pr_bool, &snaptoint, NULL, NULL, '\0', NULL, 0,
      N_
      ("When the user clicks in the editing window, round the location to the nearest integers.")},
  {
  N_("JoinSnap"), pr_real, &joinsnap, NULL, NULL, '\0', NULL, 0,
      N_
      ("The Edit->Join command will join points which are this close together\nA value of 0 means they must be coincident")},
  {
  N_("StopAtJoin"), pr_bool, &stop_at_join, NULL, NULL, '\0', NULL, 0,
      N_
      ("When dragging points in the outline view a join may occur\n(two open contours may connect at their endpoints). When\nthis is On a join will cause FontForge to stop moving the\nselection (as if the user had released the mouse button).\nThis is handy if your fingers are inclined to wiggle a bit.")},
  {
  N_("CopyMetaData"), pr_bool, &copymetadata, NULL, NULL, '\0', NULL, 0,
      N_
      ("When copying glyphs from the font view, also copy the\nglyphs' metadata (name, encoding, comment, etc).")},
  {
  N_("UndoDepth"), pr_int, &maxundoes, NULL, NULL, '\0', NULL, 0,
      N_("The maximum number of Undoes/Redoes stored in a glyph")},
  {
  N_("UpdateFlex"), pr_bool, &updateflex, NULL, NULL, '\0', NULL, 0,
      N_("Figure out flex hints after every change")},
  {
  N_("AutoKernDialog"), pr_bool, &default_autokern_dlg, NULL, NULL, '\0',
      NULL, 0, N_("Open AutoKern dialog for new kerning subtables")},
  {
  N_("MetricsShiftSkip"), pr_int, &pref_mv_shift_and_arrow_skip, NULL, NULL,
      '\0', NULL, 0,
      N_
      ("Number of units to increment/decrement a table value by in the metrics window when shift is held")},
  {
N_("MetricsControlShiftSkip"), pr_int,
      &pref_mv_control_shift_and_arrow_skip, NULL, NULL, '\0', NULL, 0,
      N_
      ("Number of units to increment/decrement a table value by in the metrics window when both control and shift is held")},
    PREFS_LIST_EMPTY}, sync_list[] =
{
  {
  N_("AutoWidthSync"), pr_bool, &adjustwidth, NULL, NULL, '\0', NULL, 0,
      N_
      ("Changing the width of a glyph\nchanges the widths of all accented\nglyphs based on it.")},
  {
N_("AutoLBearingSync"), pr_bool, &adjustlbearing, NULL, NULL, '\0', NULL,
      0,
      N_
      ("Changing the left side bearing\nof a glyph adjusts the lbearing\nof other references in all accented\nglyphs based on it.")},
    PREFS_LIST_EMPTY}, tt_list[] =
{
  {
  N_("ClearInstrsBigChanges"), pr_bool, &clear_tt_instructions_when_needed,
      NULL, NULL, 'C', NULL, 0,
      N_
      ("Instructions in a TrueType font refer to\npoints by number, so if you edit a glyph\nin such a way that some points have different\nnumbers (add points, remove them, etc.) then\nthe instructions will be applied to the wrong\npoints with disasterous results.\n  Normally FontForge will remove the instructions\nif it detects that the points have been renumbered\nin order to avoid the above problem. You may turn\nthis behavior off -- but be careful!")},
  {
N_("CopyTTFInstrs"), pr_bool, &copyttfinstr, NULL, NULL, '\0', NULL, 0,
      N_
      ("When copying glyphs from the font view, also copy the\nglyphs' truetype instructions.")},
    PREFS_LIST_EMPTY}, accent_list[] =
{
  {
  N_("AccentOffsetPercent"), pr_int, &accent_offset, NULL, NULL, '\0', NULL,
      0,
      N_
      ("The percentage of an em by which an accent is offset from its base glyph in Build Accent")},
  {
  N_("AccentCenterLowest"), pr_bool, &GraveAcuteCenterBottom, NULL, NULL,
      '\0', NULL, 0,
      N_
      ("When placing grave and acute accents above letters, should\nFontForge center them based on their full width, or\nshould it just center based on the lowest point\nof the accent.")},
  {
  N_("CharCenterHighest"), pr_bool, &CharCenterHighest, NULL, NULL, '\0',
      NULL, 0,
      N_
      ("When centering an accent over a glyph, should the accent\nbe centered on the highest point(s) of the glyph,\nor the middle of the glyph?")},
  {
N_("PreferSpacingAccents"), pr_bool, &PreferSpacingAccents, NULL, NULL,
      '\0', NULL, 0,
      N_
      ("Use spacing accents (Unicode: 02C0-02FF) rather than\ncombining accents (Unicode: 0300-036F) when\nbuilding accented glyphs.")},
    PREFS_LIST_EMPTY}, args_list[] =
{
  {
  N_("PreferPotrace"), pr_bool, &preferpotrace, NULL, NULL, '\0', NULL, 0,
      N_
      ("FontForge supports two different helper applications to do autotracing\n autotrace and potrace\nIf your system only has one it will use that one, if you have both\nuse this option to tell FontForge which to pick.")},
  {
  N_("AutotraceArgs"), pr_string, NULL, GetAutoTraceArgs, SetAutoTraceArgs,
      '\0', NULL, 0,
      N_
      ("Extra arguments for configuring the autotrace program\n(either autotrace or potrace)")},
  {
  N_("AutotraceAsk"), pr_bool, &autotrace_ask, NULL, NULL, '\0', NULL, 0,
      N_
      ("Ask the user for autotrace arguments each time autotrace is invoked")},
  {
  N_("MfArgs"), pr_string, &mf_args, NULL, NULL, '\0', NULL, 0,
      N_
      ("Commands to pass to mf (metafont) program, the filename will follow these")},
  {
  N_("MfAsk"), pr_bool, &mf_ask, NULL, NULL, '\0', NULL, 0,
      N_("Ask the user for mf commands each time mf is invoked")},
  {
  N_("MfClearBg"), pr_bool, &mf_clearbackgrounds, NULL, NULL, '\0', NULL, 0,
      N_
      ("FontForge loads large images into the background of each glyph\nprior to autotracing them. You may retain those\nimages to look at after mf processing is complete, or\nremove them to save space")},
  {
N_("MfShowErr"), pr_bool, &mf_showerrors, NULL, NULL, '\0', NULL, 0,
      N_
      ("MetaFont (mf) generates lots of verbiage to stdout.\nMost of the time I find it an annoyance but it is\nimportant to see if something goes wrong.")},
    PREFS_LIST_EMPTY}, fontinfo_list[] =
{
  {
  N_("FoundryName"), pr_string, &BDFFoundry, NULL, NULL, 'F', NULL, 0,
      N_("Name used for foundry field in bdf\nfont generation")},
  {
  N_("TTFFoundry"), pr_string, &TTFFoundry, NULL, NULL, 'T', NULL, 0,
      N_
      ("Name used for Vendor ID field in\nttf (OS/2 table) font generation.\nMust be no more than 4 characters")},
  {
  N_("NewFontNameList"), pr_namelist, &namelist_for_new_fonts, NULL, NULL,
      '\0', NULL, 0,
      N_
      ("FontForge will use this namelist when assigning\nglyph names to code points in a new font.")},
  {
  N_("RecognizePUANames"), pr_bool, &recognizePUA, NULL, NULL, 'U', NULL, 0,
      N_
      ("Once upon a time, Adobe assigned PUA (public use area) encodings\nfor many stylistic variants of characters (small caps, old style\nnumerals, etc.). Adobe no longer believes this to be a good idea,\nand recommends that these encodings be ignored.\n\n The assignments were originally made because most applications\ncould not handle OpenType features for accessing variants. Adobe\nnow believes that all apps that matter can now do so. Applications\nlike Word and OpenOffice still can't handle these features, so\n fontforge's default behavior is to ignore Adobe's current\nrecommendations.\n\nNote: This does not affect figuring out unicode from the font's encoding,\nit just controls determining unicode from a name.")},
  {
  N_("UnicodeGlyphNames"), pr_bool, &allow_utf8_glyphnames, NULL, NULL, 'O',
      NULL, 0,
      N_
      ("Allow the full unicode character set in glyph names.\nThis does not conform to adobe's glyph name standard.\nSuch names should be for internal use only and\nshould NOT end up in production fonts.")},
    PREFS_LIST_EMPTY}, generate_list[] =
{
  {
  N_("AskBDFResolution"), pr_bool, &ask_user_for_resolution, NULL, NULL,
      'B', NULL, 0,
      N_
      ("When generating a set of BDF fonts ask the user\nto specify the screen resolution of the fonts\notherwise FontForge will guess depending on the pixel size.")},
  {
N_("AutoHint"), pr_bool, &autohint_before_generate, NULL, NULL, 'H', NULL,
      0, N_("AutoHint changed glyphs before generating a font")},
    PREFS_LIST_EMPTY}, hints_list[] =
{
  {
  N_("StandardSlopeError"), pr_angle, &stem_slope_error, NULL, NULL, '\0',
      NULL, 0,
      N_
      ("The maximum slope difference which still allows to consider two points \"parallel\".\nEnlarge this to make the autohinter more tolerable to small deviations from straight lines when detecting stem edges.")},
  {
  N_("SerifSlopeError"), pr_angle, &stub_slope_error, NULL, NULL, '\0',
      NULL, 0,
      N_
      ("Same as above, but for terminals of small features (e. g. serifs), which can deviate more significantly from the horizontal or vertical direction.")},
  {
  N_("HintBoundingBoxes"), pr_bool, &hint_bounding_boxes, NULL, NULL, '\0',
      NULL, 0,
      N_
      ("FontForge will place vertical or horizontal hints to describe the bounding boxes of suitable glyphs.")},
  {
  N_("HintDiagonalEnds"), pr_bool, &hint_diagonal_ends, NULL, NULL, '\0',
      NULL, 0,
      N_
      ("FontForge will place vertical or horizontal hints at the ends of diagonal stems.")},
  {
  N_("HintDiagonalInter"), pr_bool, &hint_diagonal_intersections, NULL,
      NULL, '\0', NULL, 0,
      N_
      ("FontForge will place vertical or horizontal hints at the intersections of diagonal stems.")},
  {
N_("DetectDiagonalStems"), pr_bool, &detect_diagonal_stems, NULL, NULL,
      '\0', NULL, 0,
      N_
      ("FontForge will generate diagonal stem hints, which then can be used by the AutoInstr command.")},
    PREFS_LIST_EMPTY}, instrs_list[] =
{
  {
  N_("InstructDiagonalStems"), pr_bool, &instruct_diagonal_stems, NULL,
      NULL, '\0', NULL, 0,
      N_("Generate instructions for diagonal stem hints.")},
  {
  N_("InstructSerifs"), pr_bool, &instruct_serif_stems, NULL, NULL, '\0',
      NULL, 0,
      N_
      ("Try to detect serifs and other elements protruding from base stems and generate instructions for them.")},
  {
  N_("InstructBallTerminals"), pr_bool, &instruct_ball_terminals, NULL,
      NULL, '\0', NULL, 0, N_("Generate instructions for ball terminals.")},
  {
  N_("InterpolateStrongPoints"), pr_bool, &interpolate_strong, NULL, NULL,
      '\0', NULL, 0,
      N_
      ("Interpolate between stem edges some important points, not affected by other instructions.")},
  {
N_("CounterControl"), pr_bool, &control_counters, NULL, NULL, '\0', NULL,
      0,
      N_
      ("Make sure similar or equal counters remain the same in gridfitted outlines.\nEnabling this option may result in glyph advance widths being\ninconsistently scaled at some PPEMs.")},
    PREFS_LIST_EMPTY}, opentype_list[] =
{
  {
N_("UseNewIndicScripts"), pr_bool, &use_second_indic_scripts, NULL, NULL,
      'C', NULL, 0,
      N_
      ("MS has changed (in August 2006) the inner workings of their Indic shaping\nengine, and to disambiguate this change has created a parallel set of script\ntags (generally ending in '2') for Indic writing systems. If you are working\nwith the new system set this flag, if you are working with the old unset it.\n(if you aren't doing Indic work, this flag is irrelevant).")},
    PREFS_LIST_EMPTY},
/* These are hidden, so will never appear in preference ui, hence, no "N_(" */
/*  They are controled elsewhere AntiAlias is a menu item in the font window's View menu */
/*  etc. */
  hidden_list[] =
{
  {
  "AntiAlias", pr_bool, &default_fv_antialias, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "DefaultFVShowHmetrics", pr_int, &default_fv_showhmetrics, NULL, NULL,
      '\0', NULL, 1, NULL},
  {
  "DefaultFVShowVmetrics", pr_int, &default_fv_showvmetrics, NULL, NULL,
      '\0', NULL, 1, NULL},
  {
  "DefaultFVSize", pr_int, &default_fv_font_size, NULL, NULL, 'S', NULL, 1,
      NULL},
  {
  "DefaultFVRowCount", pr_int, &default_fv_row_count, NULL, NULL, 'S', NULL,
      1, NULL},
  {
  "DefaultFVColCount", pr_int, &default_fv_col_count, NULL, NULL, 'S', NULL,
      1, NULL},
  {
  "DefaultFVGlyphLabel", pr_int, &default_fv_glyphlabel, NULL, NULL, 'S',
      NULL, 1, NULL},
  {
  "SaveToDir", pr_int, &save_to_dir, NULL, NULL, 'S', NULL, 1, NULL},
  {
  "OnlyCopyDisplayed", pr_bool, &onlycopydisplayed, NULL, NULL, '\0', NULL,
      1, NULL},
  {
  "PalettesDocked", pr_bool, &palettes_docked, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "DefaultCVWidth", pr_int, &default_cv_width, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "DefaultCVHeight", pr_int, &default_cv_height, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "CVVisible0", pr_bool, &cvvisible[0], NULL, NULL, '\0', NULL, 1, NULL},
  {
  "CVVisible1", pr_bool, &cvvisible[1], NULL, NULL, '\0', NULL, 1, NULL},
  {
  "BVVisible0", pr_bool, &bvvisible[0], NULL, NULL, '\0', NULL, 1, NULL},
  {
  "BVVisible1", pr_bool, &bvvisible[1], NULL, NULL, '\0', NULL, 1, NULL},
  {
  "BVVisible2", pr_bool, &bvvisible[2], NULL, NULL, '\0', NULL, 1, NULL},
  {
  "MarkExtrema", pr_int, &CVShows.markextrema, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "MarkPointsOfInflect", pr_int, &CVShows.markpoi, NULL, NULL, '\0', NULL,
      1, NULL},
  {
  "ShowRulers", pr_bool, &CVShows.showrulers, NULL, NULL, '\0', NULL, 1,
      N_("Display rulers in the Outline Glyph View")},
  {
  "ShowCPInfo", pr_int, &CVShows.showcpinfo, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "InfoWindowDistance", pr_int, &infowindowdistance, NULL, NULL, '\0', NULL,
      1, NULL},
  {
  "ShowSideBearings", pr_int, &CVShows.showsidebearings, NULL, NULL, '\0',
      NULL, 1, NULL},
  {
  "ShowRefNames", pr_int, &CVShows.showrefnames, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "ShowPoints", pr_bool, &CVShows.showpoints, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "ShowFilled", pr_int, &CVShows.showfilled, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "ShowTabs", pr_int, &CVShows.showtabs, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "SnapOutlines", pr_int, &CVShows.snapoutlines, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "ShowAlmostHVLines", pr_bool, &CVShows.showalmosthvlines, NULL, NULL,
      '\0', NULL, 1, NULL},
  {
  "ShowAlmostHVCurves", pr_bool, &CVShows.showalmosthvcurves, NULL, NULL,
      '\0', NULL, 1, NULL},
  {
  "AlmostHVBound", pr_int, &CVShows.hvoffset, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "CheckSelfIntersects", pr_bool, &CVShows.checkselfintersects, NULL, NULL,
      '\0', NULL, 1, NULL},
  {
  "ShowDebugChanges", pr_bool, &CVShows.showdebugchanges, NULL, NULL, '\0',
      NULL, 1, NULL},
  {
  "DefaultScreenDpiSystem", pr_int, &oldsystem, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "DefaultOutputFormat", pr_int, &oldformatstate, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "DefaultBitmapFormat", pr_int, &oldbitmapstate, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "SaveValidate", pr_int, &old_validate, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "SaveFontLogAsk", pr_int, &old_fontlog, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "DefaultSFNTflags", pr_int, &old_sfnt_flags, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "DefaultPSflags", pr_int, &old_ps_flags, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "RegularStar", pr_bool, &regular_star, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "PolyStar", pr_bool, &polystar, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "RectEllipse", pr_bool, &rectelipse, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "RectCenterOut", pr_bool, &center_out[0], NULL, NULL, '\0', NULL, 1, NULL},
  {
  "EllipseCenterOut", pr_bool, &center_out[1], NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "PolyStartPointCnt", pr_int, &ps_pointcnt, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "RoundRectRadius", pr_real, &rr_radius, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "StarPercent", pr_real, &star_percent, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "CoverageFormatsAllowed", pr_int, &coverageformatsallowed, NULL, NULL,
      '\0', NULL, 1, NULL},
  {
  "DebugWins", pr_int, &debug_wins, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "GridFitDpi", pr_int, &gridfit_dpi, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "GridFitDepth", pr_int, &gridfit_depth, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "GridFitPointSize", pr_real, &gridfit_pointsizey, NULL, NULL, '\0', NULL,
      1, NULL},
  {
  "GridFitPointSizeX", pr_real, &gridfit_pointsizex, NULL, NULL, '\0', NULL,
      1, NULL},
  {
  "GridFitSameAs", pr_int, &gridfit_x_sameas_y, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "MVShowGrid", pr_int, &mvshowgrid, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "ForceNamesWhenOpening", pr_namelist, &force_names_when_opening, NULL,
      NULL, '\0', NULL, 1, NULL},
  {
  "ForceNamesWhenSaving", pr_namelist, &force_names_when_saving, NULL, NULL,
      '\0', NULL, 1, NULL},
  {
  "DefaultFontFilterIndex", pr_int, &default_font_filter_index, NULL, NULL,
      '\0', NULL, 1, NULL},
  {
  "FCShowHidden", pr_bool, &gfc_showhidden, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "FCDirPlacement", pr_int, &gfc_dirplace, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "FCBookmarks", pr_string, &gfc_bookmarks, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "DefaultMVWidth", pr_int, &mv_width, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "DefaultMVHeight", pr_int, &mv_height, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "DefaultBVWidth", pr_int, &bv_width, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "DefaultBVHeight", pr_int, &bv_height, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "AnchorControlPixelSize", pr_int, &aa_pixelsize, NULL, NULL, '\0', NULL,
      1, NULL},
  {
  "CV_B1Tool", pr_int, (int *) &cv_b1_tool, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "CV_CB1Tool", pr_int, (int *) &cv_cb1_tool, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "CV_B2Tool", pr_int, (int *) &cv_b2_tool, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "CV_CB2Tool", pr_int, (int *) &cv_cb2_tool, NULL, NULL, '\0', NULL, 1,
      NULL},
  {
  "XUID-Base", pr_string, &xuid, NULL, NULL, 'X', NULL, 0, N_("If specified this should be a space separated list of integers each\nless than 16777216 which uniquely identify your organization\nFontForge will generate a random number for the final component.")},  /* Obsolete */
  {
"ShowKerningPane", pr_int, (int *) &show_kerning_pane_in_class, NULL,
      NULL, '\0', NULL, 1, NULL}, PREFS_LIST_EMPTY}, oldnames[] =
{
  {
  "DumpGlyphMap", pr_bool, &glyph_2_name_map, NULL, NULL, '\0', NULL, 0,
      N_
      ("When generating a truetype or opentype font it is occasionally\nuseful to know the mapping between truetype glyph ids and\nglyph names. Setting this option will cause FontForge to\nproduce a file (with extension .g2n) containing those data.")},
  {
  "DefaultTTFApple", pr_int, &pointless, NULL, NULL, '\0', NULL, 1, NULL},
  {
  "AcuteCenterBottom", pr_bool, &GraveAcuteCenterBottom, NULL, NULL, '\0',
      NULL, 1,
      N_
      ("When placing grave and acute accents above letters, should\nFontForge center them based on their full width, or\nshould it just center based on the lowest point\nof the accent.")},
  {
  "AlwaysGenApple", pr_bool, &alwaysgenapple, NULL, NULL, 'A', NULL, 0,
      N_
      ("Apple and MS/Adobe differ about the format of truetype and opentype files.\nThis controls the default setting of the Apple checkbox in the\nFile->Generate Font dialog.\nThe main differences are:\n Bitmap data are stored in different tables\n Scaled composite glyphs are treated differently\nIf both this and OpenType are set, both formats are generated")},
  {
  "AlwaysGenOpenType", pr_bool, &alwaysgenopentype, NULL, NULL, 'O', NULL,
      0,
      N_
      ("Apple and MS/Adobe differ about the format of truetype and opentype files.\nThis controls the default setting of the OpenType checkbox in the\nFile->Generate Font dialog.\nThe main differences are:\n Bitmap data are stored in different tables\n Scaled composite glyphs are treated differently\nIf both this and Apple are set, both formats are generated")},
  {
  "DefaultTTFflags", pr_int, &old_ttf_flags, NULL, NULL, '\0', NULL, 1, NULL},
  {
"DefaultOTFflags", pr_int, &old_otf_flags, NULL, NULL, '\0', NULL, 1, NULL},
    PREFS_LIST_EMPTY}, *prefs_list[] =
{
general_list, new_list, open_list, navigation_list, sync_list, editing_list,
    accent_list, args_list, fontinfo_list, generate_list, tt_list,
    opentype_list, hints_list, instrs_list, hidden_list, NULL},
  *load_prefs_list[] =
{
general_list, new_list, open_list, navigation_list, sync_list, editing_list,
    accent_list, args_list, fontinfo_list, generate_list, tt_list,
    opentype_list, hints_list, instrs_list, hidden_list, oldnames, NULL};

struct visible_prefs_list
{
  char *tab_name;
  int nest;
  struct prefs_list *pl;
} visible_prefs_list[] =
{
  {
  N_("Generic"), 0, general_list},
  {
  N_("New Font"), 0, new_list},
  {
  N_("Open Font"), 0, open_list},
  {
  N_("Navigation"), 0, navigation_list},
  {
  N_("Editing"), 0, editing_list},
  {
  N_("Synchronize"), 1, sync_list},
  {
  N_("TT"), 1, tt_list},
  {
  N_("Accents"), 1, accent_list},
  {
  N_("Apps"), 1, args_list},
  {
  N_("Font Info"), 0, fontinfo_list},
  {
  N_("Generate"), 0, generate_list},
  {
  N_("PS Hints"), 1, hints_list},
  {
  N_("TT Instrs"), 1, instrs_list},
  {
  N_("OpenType"), 1, opentype_list},
  {
  NULL, 0, NULL}
};

static void
FileChooserPrefsChanged (void *pointless)
{
  SavePrefs (true);
}

static void
ProcessFileChooserPrefs (void)
{
  uint32_t **b;
  int i;

  GFileChooserSetShowHidden (gfc_showhidden);
  GFileChooserSetDirectoryPlacement (gfc_dirplace);
  if (gfc_bookmarks == NULL)
    {
      b = xmalloc (8 * sizeof (uint32_t *));
      i = 0;
      b[i++] =
        x_u8_to_u32 ((uint8_t *) "ftp://ctan.org/pub/tex-archive/fonts/");
      b[i++] = NULL;
      GFileChooserSetBookmarks (b);
    }
  else
    {
      char *pt, *start;
      start = gfc_bookmarks;
      for (i = 0;; ++i)
        {
          pt = strchr (start, ';');
          if (pt == NULL)
            break;
          start = pt + 1;
        }
      start = gfc_bookmarks;
      b = xmalloc ((i + 2) * sizeof (uint32_t *));
      for (i = 0;; ++i)
        {
          pt = strchr (start, ';');
          if (pt != NULL)
            *pt = '\0';
          b[i] = utf82u_copy (start);
          if (pt == NULL)
            break;
          *pt = ';';
          start = pt + 1;
        }
      b[i + 1] = NULL;
      GFileChooserSetBookmarks (b);
    }
  GFileChooserSetPrefsChangedCallback (NULL, FileChooserPrefsChanged);
}

static void
GetFileChooserPrefs (void)
{
  uint32_t **foo;

  gfc_showhidden = GFileChooserGetShowHidden ();
  gfc_dirplace = GFileChooserGetDirectoryPlacement ();
  foo = GFileChooserGetBookmarks ();
  free (gfc_bookmarks);
  if (foo == NULL || foo[0] == NULL)
    gfc_bookmarks = NULL;
  else
    {
      int i, len = 0;
      for (i = 0; foo[i] != NULL; ++i)
        len += 4 * u32_strlen (foo[i]) + 1;
      gfc_bookmarks = xmalloc (len + 10);
      len = 0;
      for (i = 0; foo[i] != NULL; ++i)
        {
          u2utf8_strcpy (gfc_bookmarks + len, foo[i]);
          len += strlen (gfc_bookmarks + len);
          gfc_bookmarks[len++] = ';';
        }
      if (len > 0)
        gfc_bookmarks[len - 1] = '\0';
      else
        {
          free (gfc_bookmarks);
          gfc_bookmarks = NULL;
        }
    }
}

#define TOPICS	(sizeof(visible_prefs_list)/sizeof(visible_prefs_list[0])-1)

static int
PrefsUI_GetPrefs (char *name, Val * val)
{
  int i, j;

  /* Support for obsolete preferences */
  alwaysgenapple = (old_sfnt_flags & ttf_flag_applemode) ? 1 : 0;
  alwaysgenopentype = (old_sfnt_flags & ttf_flag_otmode) ? 1 : 0;

  for (i = 0; prefs_list[i] != NULL; ++i)
    for (j = 0; prefs_list[i][j].name != NULL; ++j)
      {
        if (strcmp (prefs_list[i][j].name, name) == 0)
          {
            struct prefs_list *pf = &prefs_list[i][j];
            if (pf->type == pr_bool || pf->type == pr_int
                || pf->type == pr_unicode)
              {
                val->type = v_int;
                val->u.ival = *((int *) (pf->val));
              }
            else if (pf->type == pr_string || pf->type == pr_file)
              {
                val->type = v_str;

                char *tmpstr =
                  pf->val ? *((char **) (pf->val)) : (char *) (pf->get) ();
                val->u.sval = xstrdup_or_null (tmpstr ? tmpstr : "");

                if (!pf->val)
                  free (tmpstr);
              }
            else if (pf->type == pr_encoding)
              {
                val->type = v_str;
                if (*((NameList **) (pf->val)) == NULL)
                  val->u.sval = xstrdup_or_null ("NULL");
                else
                  val->u.sval =
                    xstrdup_or_null ((*((Encoding **) (pf->val)))->enc_name);
              }
            else if (pf->type == pr_namelist)
              {
                val->type = v_str;
                val->u.sval =
                  xstrdup_or_null ((*((NameList **) (pf->val)))->title);
              }
            else if (pf->type == pr_real || pf->type == pr_angle)
              {
                val->type = v_real;
                val->u.fval = *((float *) (pf->val));
                if (pf->type == pr_angle)
                  val->u.fval *= RAD2DEG;
              }
            else
              return false;

            return true;
          }
      }
  return false;
}

static void
CheckObsoletePrefs (void)
{
  if (alwaysgenapple == false)
    old_sfnt_flags &= ~ttf_flag_applemode;
  else if (alwaysgenapple == true)
    old_sfnt_flags |= ttf_flag_applemode;
  if (alwaysgenopentype == false)
    old_sfnt_flags &= ~ttf_flag_otmode;
  else if (alwaysgenopentype == true)
    old_sfnt_flags |= ttf_flag_otmode;
  if (old_ttf_flags != 0)
    old_sfnt_flags = old_ttf_flags | old_otf_flags;
}

static int
PrefsUI_SetPrefs (char *name, Val * val1, Val * val2)
{
  int i, j;

  /* Support for obsolete preferences */
  alwaysgenapple = -1;
  alwaysgenopentype = -1;

  for (i = 0; prefs_list[i] != NULL; ++i)
    for (j = 0; prefs_list[i][j].name != NULL; ++j)
      {
        if (strcmp (prefs_list[i][j].name, name) == 0)
          {
            struct prefs_list *pf = &prefs_list[i][j];
            if (pf->type == pr_bool || pf->type == pr_int
                || pf->type == pr_unicode)
              {
                if ((val1->type != v_int && val1->type != v_unicode)
                    || val2 != NULL)
                  return -1;
                *((int *) (pf->val)) = val1->u.ival;
              }
            else if (pf->type == pr_real || pf->type == pr_angle)
              {
                if (val1->type == v_real && val2 == NULL)
                  *((float *) (pf->val)) = val1->u.fval;
                else if (val1->type != v_int
                         || (val2 != NULL && val2->type != v_int))
                  return -1;
                else
                  *((float *) (pf->val)) =
                    (val2 ==
                     NULL ? val1->u.ival : val1->u.ival /
                     (double) val2->u.ival);
                if (pf->type == pr_angle)
                  *((float *) (pf->val)) /= RAD2DEG;
              }
            else if (pf->type == pr_string || pf->type == pr_file)
              {
                if (val1->type != v_str || val2 != NULL)
                  return -1;
                if (pf->set)
                  {
                    pf->set (val1->u.sval);
                  }
                else
                  {
                    free (*((char **) (pf->val)));
                    *((char **) (pf->val)) = xstrdup_or_null (val1->u.sval);
                  }
              }
            else if (pf->type == pr_encoding)
              {
                if (val2 != NULL)
                  return -1;
                else if (val1->type == v_str && pf->val == &default_encoding)
                  {
                    Encoding *enc = FindOrMakeEncoding (val1->u.sval);
                    if (enc == NULL)
                      return -1;
                    *((Encoding **) (pf->val)) = enc;
                  }
                else
                  return -1;
              }
            else if (pf->type == pr_namelist)
              {
                if (val2 != NULL)
                  return -1;
                else if (val1->type == v_str)
                  {
                    NameList *nl = NameListByName (val1->u.sval);
                    if (strcmp (val1->u.sval, "NULL") == 0
                        && pf->val != &namelist_for_new_fonts)
                      nl = NULL;
                    else if (nl == NULL)
                      return -1;
                    *((NameList **) (pf->val)) = nl;
                  }
                else
                  return -1;
              }
            else
              return false;

            CheckObsoletePrefs ();
            SavePrefs (true);
            return true;
          }
      }
  return false;
}

static char *
getPfaEditPrefs (void)
{
  static char *prefs = NULL;
  char buffer[1025];

  if (prefs != NULL)
    return prefs;
  if (getUserConfigDir () == NULL)
    return NULL;
  sprintf (buffer, "%s/prefs", getUserConfigDir ());
  prefs = xstrdup_or_null (buffer);
  return prefs;
}

static char *
PrefsUI_getFontForgeShareDir (void)
{
  return SHAREDIR;
}

static void
DefaultXUID (void)
{
  /* Adobe has assigned PfaEdit a base XUID of 1021. Each new user is going */
  /*  to get a couple of random numbers appended to that, hoping that will */
  /*  make for a fairly safe system. */
  /* FontForge will use the same scheme */
  int r1, r2;
  char buffer[50];
  struct timeval tv;

  gettimeofday (&tv, NULL);
  srand (tv.tv_usec);
  do
    {
      r1 = rand () & 0x3ff;
    }
  while (r1 == 0);              /* I reserve "0" for me! */
  gettimeofday (&tv, NULL);
  srandom (tv.tv_usec + 1);
  r2 = random ();
  sprintf (buffer, "1021 %d %d", r1, r2);
  free (xuid);
  xuid = xstrdup_or_null (buffer);
}

static void
DefaultHelp (void)
{
  if (helpdir == NULL)
    helpdir = xstrdup_or_null (DOCDIR "/");
}

static void
PrefsUI_SetDefaults (void)
{
  DefaultXUID ();
  DefaultHelp ();
}

static void
PrefsUI_LoadPrefs (void)
{
  char *prefs = getPfaEditPrefs ();
  FILE *p;
  char line[1100];
  int i, j, ri = 0, mn = 0, ms = 0, fn = 0, ff = 0, filt_max = 0;
  char *pt;
  struct prefs_list *pl;

  LoadPfaEditEncodings ();
  LoadGroupList ();

  if (prefs != NULL && (p = fopen (prefs, "r")) != NULL)
    {
      while (fgets (line, sizeof (line), p) != NULL)
        {
          if (*line == '#')
            continue;
          pt = strchr (line, ':');
          if (pt == NULL)
            continue;
          for (j = 0; load_prefs_list[j] != NULL; ++j)
            {
              for (i = 0; load_prefs_list[j][i].name != NULL; ++i)
                if (strncmp (line, load_prefs_list[j][i].name, pt - line) ==
                    0)
                  break;
              if (load_prefs_list[j][i].name != NULL)
                break;
            }
          pl = NULL;
          if (load_prefs_list[j] != NULL)
            pl = &load_prefs_list[j][i];
          for (++pt; *pt == '\t'; ++pt);
          if (line[strlen (line) - 1] == '\n')
            line[strlen (line) - 1] = '\0';
          if (line[strlen (line) - 1] == '\r')
            line[strlen (line) - 1] = '\0';
          if (pl == NULL)
            {
              if (strncmp (line, "Recent:", strlen ("Recent:")) == 0
                  && ri < RECENT_MAX)
                RecentFiles[ri++] = xstrdup_or_null (pt);
              else if (strncmp (line, "MenuScript:", strlen ("MenuScript:"))
                       == 0 && ms < SCRIPT_MENU_MAX)
                script_filenames[ms++] = xstrdup_or_null (pt);
              else if (strncmp (line, "MenuName:", strlen ("MenuName:")) == 0
                       && mn < SCRIPT_MENU_MAX)
                script_menu_names[mn++] = utf82u_copy (pt);
              else
                if (strncmp
                    (line, "FontFilterName:",
                     strlen ("FontFilterName:")) == 0)
                {
                  if (fn >= filt_max)
                    user_font_filters =
                      xrealloc (user_font_filters,
                                ((filt_max +=
                                  10) + 1) * sizeof (struct openfilefilters));
                  user_font_filters[fn].filter = NULL;
                  user_font_filters[fn++].name = xstrdup_or_null (pt);
                  user_font_filters[fn].name = NULL;
                }
              else if (strncmp (line, "FontFilter:", strlen ("FontFilter:"))
                       == 0)
                {
                  if (ff < filt_max)
                    user_font_filters[ff++].filter = xstrdup_or_null (pt);
                }
              continue;
            }
          switch (pl->type)
            {
            case pr_encoding:
              {
                Encoding *enc = FindOrMakeEncoding (pt);
                if (enc == NULL)
                  enc = FindOrMakeEncoding ("ISO8859-1");
                if (enc == NULL)
                  enc = &custom;
                *((Encoding **) (pl->val)) = enc;
              }
              break;
            case pr_namelist:
              {
                NameList *nl = NameListByName (pt);
                if (strcmp (pt, "NULL") == 0
                    && pl->val != &namelist_for_new_fonts)
                  *((NameList **) (pl->val)) = NULL;
                else if (nl != NULL)
                  *((NameList **) (pl->val)) = nl;
              }
              break;
            case pr_bool:
            case pr_int:
              sscanf (pt, "%d", (int *) pl->val);
              break;
            case pr_unicode:
              if (sscanf (pt, "U+%x", (int *) pl->val) != 1)
                if (sscanf (pt, "u+%x", (int *) pl->val) != 1)
                  sscanf (pt, "%x", (int *) pl->val);
              break;
            case pr_real:
            case pr_angle:
              {
                char *end;
                *((float *) pl->val) = strtod (pt, &end);
                if ((*end == ',' || *end == '.'))
                  {
                    *end = (*end == '.') ? ',' : '.';
                    *((float *) pl->val) = strtod (pt, NULL);
                  }
              }
              if (pl->type == pr_angle)
                *(float *) pl->val /= RAD2DEG;
              break;
            case pr_string:
            case pr_file:
              if (*pt == '\0')
                pt = NULL;
              if (pl->val != NULL)
                *((char **) (pl->val)) = xstrdup_or_null (pt);
              else
                (pl->set) (xstrdup_or_null (pt));
              break;
            }
        }
      fclose (p);
    }
  if (xdefs_filename != NULL)
    GResourceAddResourceFile (xdefs_filename, true);
  if (othersubrsfile != NULL && ReadOtherSubrsFile (othersubrsfile) <= 0)
    fprintf (stderr, "Failed to read OtherSubrs from %s\n", othersubrsfile);

  if (glyph_2_name_map)
    {
      old_sfnt_flags |= ttf_flag_glyphmap;
    }
  LoadNamelistDir (NULL);
  ProcessFileChooserPrefs ();
}

static void
PrefsUI_SavePrefs (int not_if_script)
{
  char *prefs = getPfaEditPrefs ();
  FILE *p;
  int i, j;
  char *temp;
  struct prefs_list *pl;
  extern int running_script;

  if (prefs == NULL)
    return;
  if (not_if_script && running_script)
    return;

  if ((p = fopen (prefs, "w")) == NULL)
    return;

  GetFileChooserPrefs ();

  for (j = 0; prefs_list[j] != NULL; ++j)
    for (i = 0; prefs_list[j][i].name != NULL; ++i)
      {
        pl = &prefs_list[j][i];
        switch (pl->type)
          {
          case pr_encoding:
            fprintf (p, "%s:\t%s\n", pl->name,
                     (*((Encoding **) (pl->val)))->enc_name);
            break;
          case pr_namelist:
            fprintf (p, "%s:\t%s\n", pl->name,
                     *((NameList **) (pl->val)) ==
                     NULL ? "NULL" : (*((NameList **) (pl->val)))->title);
            break;
          case pr_bool:
          case pr_int:
            fprintf (p, "%s:\t%d\n", pl->name, *(int *) (pl->val));
            break;
          case pr_unicode:
            fprintf (p, "%s:\tU+%04x\n", pl->name, *(int *) (pl->val));
            break;
          case pr_real:
            fprintf (p, "%s:\t%g\n", pl->name, (double) *(float *) (pl->val));
            break;
          case pr_string:
          case pr_file:
            if ((pl->val) != NULL)
              temp = *(char **) (pl->val);
            else
              temp = (char *) (pl->get ());
            if (temp != NULL)
              fprintf (p, "%s:\t%s\n", pl->name, temp);
            if ((pl->val) == NULL)
              free (temp);
            break;
          case pr_angle:
            fprintf (p, "%s:\t%g\n", pl->name,
                     ((double) *(float *) pl->val) * RAD2DEG);
            break;
          }
      }

  for (i = 0; i < RECENT_MAX && RecentFiles[i] != NULL; ++i)
    fprintf (p, "Recent:\t%s\n", RecentFiles[i]);
  for (i = 0; i < SCRIPT_MENU_MAX && script_filenames[i] != NULL; ++i)
    {
      fprintf (p, "MenuScript:\t%s\n", script_filenames[i]);
      ulc_fprintf (p, "MenuName:\t%llU\n", script_menu_names[i]);
    }
  if (user_font_filters != NULL)
    {
      for (i = 0; user_font_filters[i].name != NULL; ++i)
        {
          fprintf (p, "FontFilterName:\t%s\n", user_font_filters[i].name);
          fprintf (p, "FontFilter:\t%s\n", user_font_filters[i].filter);
        }
    }

  fclose (p);
}

struct pref_data
{
  int done;
  struct prefs_list *plist;
};

static int
Prefs_ScriptBrowse (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      GWindow gw = GGadgetGetWindow (g);
      GGadget *tf =
        GWidgetGetControl (gw, GGadgetGetCid (g) - SCRIPT_MENU_MAX);
      char *cur = GGadgetGetTitle8 (tf);
      char *ret;

      if (*cur == '\0')
        cur = NULL;
      ret = gwwv_open_filename (_("Call Script"), cur, "*.pe", NULL);
      free (cur);
      if (ret == NULL)
        return true;
      GGadgetSetTitle8 (tf, ret);
      free (ret);
    }
  return true;
}

static int
Prefs_BrowseFile (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      GWindow gw = GGadgetGetWindow (g);
      GGadget *tf =
        GWidgetGetControl (gw, GGadgetGetCid (g) - CID_PrefsBrowseOffset);
      char *cur = GGadgetGetTitle8 (tf);
      char *ret;
      struct prefs_list *pl = GGadgetGetUserData (tf);

      ret =
        gwwv_open_filename (pl->name, *cur == '\0' ? NULL : cur, NULL, NULL);
      free (cur);
      if (ret == NULL)
        return true;
      GGadgetSetTitle8 (tf, ret);
      free (ret);
    }
  return true;
}

void
GListAddStr (GGadget *list, uint32_t *str, void *ud)
{
  int32_t i, len;
  GTextInfo **ti = GGadgetGetList (list, &len);
  GTextInfo **replace = xmalloc ((len + 2) * sizeof (GTextInfo *));

  replace[len + 1] = xcalloc (1, sizeof (GTextInfo));
  for (i = 0; i < len; ++i)
    {
      replace[i] = xmalloc (sizeof (GTextInfo));
      *replace[i] = *ti[i];
      replace[i]->text = x_u32_strdup_or_null (ti[i]->text);
    }
  replace[i] = xcalloc (1, sizeof (GTextInfo));
  replace[i]->fg = replace[i]->bg = COLOR_DEFAULT;
  replace[i]->text = str;
  replace[i]->userdata = ud;
  GGadgetSetList (list, replace, false);
}

void
GListReplaceStr (GGadget *list, int index, uint32_t *str, void *ud)
{
  int32_t i, len;
  GTextInfo **ti = GGadgetGetList (list, &len);
  GTextInfo **replace = xmalloc ((len + 2) * sizeof (GTextInfo *));

  for (i = 0; i < len; ++i)
    {
      replace[i] = xmalloc (sizeof (GTextInfo));
      *replace[i] = *ti[i];
      if (i != index)
        replace[i]->text = x_u32_strdup_or_null (ti[i]->text);
    }
  replace[i] = xcalloc (1, sizeof (GTextInfo));
  replace[index]->text = str;
  replace[index]->userdata = ud;
  GGadgetSetList (list, replace, false);
}

static int
Prefs_Ok (GGadget *g, GEvent *e)
{
  int i, j, mi;
  int err = 0, enc;
  struct pref_data *p;
  GWindow gw;
  const uint32_t *ret;
  const uint32_t *names[SCRIPT_MENU_MAX], *scripts[SCRIPT_MENU_MAX];
  struct prefs_list *pl;
  real dangle;

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      gw = GGadgetGetWindow (g);
      p = GDrawGetUserData (gw);
      for (i = 0; i < SCRIPT_MENU_MAX; ++i)
        {
          names[i] =
            _GGadgetGetTitle (GWidgetGetControl
                              (gw, CID_ScriptMNameBase + i));
          scripts[i] =
            _GGadgetGetTitle (GWidgetGetControl
                              (gw, CID_ScriptMFileBase + i));
          if (*names[i] == '\0')
            names[i] = NULL;
          if (*scripts[i] == '\0')
            scripts[i] = NULL;
          if (scripts[i] == NULL && names[i] != NULL)
            {
              ff_post_error (_("Menu name with no associated script"),
                             _("Menu name with no associated script"));
              return true;
            }
          else if (scripts[i] != NULL && names[i] == NULL)
            {
              ff_post_error (_("Script with no associated menu name"),
                             _("Script with no associated menu name"));
              return true;
            }
        }
      for (i = mi = 0; i < SCRIPT_MENU_MAX; ++i)
        {
          if (names[i] != NULL)
            {
              names[mi] = names[i];
              scripts[mi] = scripts[i];
              ++mi;
            }
        }
      for (j = 0; visible_prefs_list[j].tab_name != 0; ++j)
        for (i = 0; visible_prefs_list[j].pl[i].name != NULL; ++i)
          {
            pl = &visible_prefs_list[j].pl[i];
            /* before assigning values, check for any errors */
            /* if any errors, then NO values should be assigned, in case they cancel */
            if (pl->dontdisplay)
              continue;
            if (pl->type == pr_int)
              {
                GetInt8 (gw, j * CID_PrefsOffset + CID_PrefsBase + i,
                         pl->name, &err);
              }
            else if (pl->type == pr_real)
              {
                GetReal8 (gw, j * CID_PrefsOffset + CID_PrefsBase + i,
                          pl->name, &err);
              }
            else if (pl->type == pr_angle)
              {
                dangle =
                  GetReal8 (gw, j * CID_PrefsOffset + CID_PrefsBase + i,
                            pl->name, &err);
                if (dangle > 90 || dangle < 0)
                  {
                    GGadgetProtest8 (pl->name);
                    err = true;
                  }
              }
            else if (pl->type == pr_unicode)
              {
                GetUnicodeChar8 (gw, j * CID_PrefsOffset + CID_PrefsBase + i,
                                 pl->name, &err);
              }
          }
      if (err)
        return true;

      for (j = 0; visible_prefs_list[j].tab_name != 0; ++j)
        for (i = 0; visible_prefs_list[j].pl[i].name != NULL; ++i)
          {
            pl = &visible_prefs_list[j].pl[i];
            if (pl->dontdisplay)
              continue;
            switch (pl->type)
              {
              case pr_int:
                *((int *) (pl->val)) =
                  GetInt8 (gw, j * CID_PrefsOffset + CID_PrefsBase + i,
                           pl->name, &err);
                break;
              case pr_unicode:
                *((int *) (pl->val)) =
                  GetUnicodeChar8 (gw,
                                   j * CID_PrefsOffset + CID_PrefsBase + i,
                                   pl->name, &err);
                break;
              case pr_bool:
                *((int *) (pl->val)) =
                  GGadgetIsChecked (GWidgetGetControl
                                    (gw,
                                     j * CID_PrefsOffset + CID_PrefsBase +
                                     i));
                break;
              case pr_real:
                *((float *) (pl->val)) =
                  GetReal8 (gw, j * CID_PrefsOffset + CID_PrefsBase + i,
                            pl->name, &err);
                break;
              case pr_encoding:
                {
                  Encoding *e;
                  e =
                    ParseEncodingNameFromList (GWidgetGetControl
                                               (gw,
                                                j * CID_PrefsOffset +
                                                CID_PrefsBase + i));
                  if (e != NULL)
                    *((Encoding **) (pl->val)) = e;
                  enc = 1;      /* So gcc doesn't complain about unused. It is unused, but why add the ifdef and make the code even messier? Sigh. icc complains anyway */
                }
                break;
              case pr_namelist:
                {
                  NameList *nl;
                  GTextInfo *ti =
                    GGadgetGetListItemSelected (GWidgetGetControl (gw,
                                                                   j *
                                                                   CID_PrefsOffset
                                                                   +
                                                                   CID_PrefsBase
                                                                   + i));
                  if (ti != NULL)
                    {
                      char *name =
                        NULL_PASSTHRU (ti->text, x_u32_to_u8 (ti->text));
                      nl = NameListByName (name);
                      free (name);
                      if (nl != NULL && nl->uses_unicode
                          && !allow_utf8_glyphnames)
                        ff_post_error (_("Namelist contains non-ASCII names"),
                                       _
                                       ("Glyph names should be limited to characters in the ASCII character set, but there are names in this namelist which use characters outside that range."));
                      else if (nl != NULL)
                        *((NameList **) (pl->val)) = nl;
                    }
                }
                break;
              case pr_string:
              case pr_file:
                ret =
                  _GGadgetGetTitle (GWidgetGetControl
                                    (gw,
                                     j * CID_PrefsOffset + CID_PrefsBase +
                                     i));
                if (pl->val != NULL)
                  {
                    free (*((char **) (pl->val)));
                    *((char **) (pl->val)) = NULL;
                    if (ret != NULL && *ret != '\0')
                      *((char **) (pl->val)) =
                        x_u32_to_u8 (u32_force_valid (ret));
                  }
                else
                  {
                    char *cret = x_u32_to_u8 (u32_force_valid (ret));
                    (pl->set) (cret);
                    free (cret);
                  }
                break;
              case pr_angle:
                *((float *) (pl->val)) =
                  GetReal8 (gw, j * CID_PrefsOffset + CID_PrefsBase + i,
                            pl->name, &err) / RAD2DEG;
                break;
              }
          }
      for (i = 0; i < SCRIPT_MENU_MAX; ++i)
        {
          free (script_menu_names[i]);
          script_menu_names[i] = NULL;
          free (script_filenames[i]);
          script_filenames[i] = NULL;
        }
      for (i = 0; i < mi; ++i)
        {
          script_menu_names[i] = x_u32_strdup_or_null (names[i]);
          script_filenames[i] = u2def_copy (scripts[i]);
        }

      if (xuid != NULL)
        {
          char *pt;
          for (pt = xuid; *pt == ' '; ++pt);
          if (*pt == '[')
            {                   /* People who know PS well, might want to put brackets arround the xuid base array, but I don't want them */
              pt = xstrdup_or_null (pt + 1);
              free (xuid);
              xuid = pt;
            }
          for (pt = xuid + strlen (xuid) - 1; pt > xuid && *pt == ' '; --pt);
          if (pt >= xuid && *pt == ']')
            *pt = '\0';
        }

      p->done = true;
      PrefsUI_SavePrefs (true);
      if (maxundoes == 0)
        {
          FontView *fv;
          for (fv = fv_list; fv != NULL; fv = (FontView *) (fv->b.next))
            SFRemoveUndoes (fv->b.sf, NULL, NULL);
        }
      if (othersubrsfile != NULL && ReadOtherSubrsFile (othersubrsfile) <= 0)
        fprintf (stderr, "Failed to read OtherSubrs from %s\n",
                 othersubrsfile);
    }
  return true;
}

static int
Prefs_Cancel (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct pref_data *p = GDrawGetUserData (GGadgetGetWindow (g));
      p->done = true;
    }
  return true;
}

static int
e_h (GWindow gw, GEvent *event)
{
  if (event->type == et_close)
    {
      struct pref_data *p = GDrawGetUserData (gw);
      p->done = true;
    }
  else if (event->type == et_char)
    {
      if (event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help)
        {
          help ("prefs.html");
          return true;
        }
      return false;
    }
  return true;
}

static void
PrefsInit (void)
{
  static int done = false;
  int i;

  if (done)
    return;
  done = true;
  for (i = 0; visible_prefs_list[i].tab_name != NULL; ++i)
    visible_prefs_list[i].tab_name = _(visible_prefs_list[i].tab_name);
}

void
DoPrefs (void)
{
  GRect pos;
  GWindow gw;
  GWindowAttrs wattrs;
  GGadgetCreateData *pgcd, gcd[5], sgcd[45];
  GGadgetCreateData sboxes[2], *sarray[50];
  GGadgetCreateData mboxes[3], *varray[5], *harray[8];
  GTextInfo *plabel, **list, label[5], slabel[45], *plabels[TOPICS + 5];
  GTabInfo aspects[TOPICS + 4];
  GGadgetCreateData **hvarray, boxes[2 * TOPICS];
  struct pref_data p;
  int i, gc, sgc, j, k, line, line_max, y, y2, ii, si;
  int32_t llen;
  char buf[20];
  int gcnt[20];
  static uint32_t nullstr[] = { 0 };
  struct prefs_list *pl;
  char *tempstr;
  GFont *font;

  PrefsInit ();

  MfArgsInit ();
  for (k = line_max = 0; visible_prefs_list[k].tab_name != 0; ++k)
    {
      for (i = line = gcnt[k] = 0; visible_prefs_list[k].pl[i].name != NULL;
           ++i)
        {
          if (visible_prefs_list[k].pl[i].dontdisplay)
            continue;
          gcnt[k] += 2;
          if (visible_prefs_list[k].pl[i].type == pr_bool)
            ++gcnt[k];
          else if (visible_prefs_list[k].pl[i].type == pr_file)
            ++gcnt[k];
          else if (visible_prefs_list[k].pl[i].type == pr_angle)
            ++gcnt[k];
          ++line;
        }
      if (visible_prefs_list[k].pl == args_list)
        {
          gcnt[k] += 6;
          line += 6;
        }
      if (line > line_max)
        line_max = line;
    }

  memset (&p, '\0', sizeof (p));
  memset (&wattrs, 0, sizeof (wattrs));
  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_restrict
    | wam_isdlg;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.restrict_input_to_me = 1;
  wattrs.is_dlg = 1;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title = _("Preferences");
  pos.x = pos.y = 0;
  pos.width = GGadgetScale (GDrawPointsToPixels (NULL, 350));
  pos.height = GDrawPointsToPixels (NULL, line_max * 26 + 69);
  gw = GDrawCreateTopWindow (NULL, &pos, e_h, &p, &wattrs);

  memset (sgcd, 0, sizeof (sgcd));
  memset (slabel, 0, sizeof (slabel));
  memset (&sboxes, 0, sizeof (sboxes));
  memset (&boxes, 0, sizeof (boxes));

  sgc = 0;
  y2 = 5;
  si = 0;

  slabel[sgc].text = (uint32_t *) _("Menu Name");
  slabel[sgc].text_is_1byte = true;
  sgcd[sgc].gd.label = &slabel[sgc];
  sgcd[sgc].gd.popup_msg =
    (uint32_t *)
    _
    ("You may create a script menu containing up to 10 frequently used scripts.\nEach entry in the menu needs both a name to display in the menu and\na script file to execute. The menu name may contain any unicode characters.\nThe button labeled \"...\" will allow you to browse for a script file.");
  sgcd[sgc].gd.pos.x = 8;
  sgcd[sgc].gd.pos.y = y2;
  sgcd[sgc].gd.flags = gg_visible | gg_enabled | gg_utf8_popup;
  sgcd[sgc++].creator = GLabelCreate;
  sarray[si++] = &sgcd[sgc - 1];

  slabel[sgc].text = (uint32_t *) _("Script File");
  slabel[sgc].text_is_1byte = true;
  sgcd[sgc].gd.label = &slabel[sgc];
  sgcd[sgc].gd.popup_msg =
    (uint32_t *)
    _
    ("You may create a script menu containing up to 10 frequently used scripts\nEach entry in the menu needs both a name to display in the menu and\na script file to execute. The menu name may contain any unicode characters.\nThe button labeled \"...\" will allow you to browse for a script file.");
  sgcd[sgc].gd.pos.x = 110;
  sgcd[sgc].gd.pos.y = y2;
  sgcd[sgc].gd.flags = gg_visible | gg_enabled | gg_utf8_popup;
  sgcd[sgc++].creator = GLabelCreate;
  sarray[si++] = &sgcd[sgc - 1];
  sarray[si++] = GCD_Glue;
  sarray[si++] = NULL;

  y2 += 14;

  for (i = 0; i < SCRIPT_MENU_MAX; ++i)
    {
      sgcd[sgc].gd.pos.x = 8;
      sgcd[sgc].gd.pos.y = y2;
      sgcd[sgc].gd.flags = gg_visible | gg_enabled;
      slabel[sgc].text =
        script_menu_names[i] == NULL ? nullstr : script_menu_names[i];
      sgcd[sgc].gd.label = &slabel[sgc];
      sgcd[sgc].gd.cid = i + CID_ScriptMNameBase;
      sgcd[sgc++].creator = GTextFieldCreate;
      sarray[si++] = &sgcd[sgc - 1];

      sgcd[sgc].gd.pos.x = 110;
      sgcd[sgc].gd.pos.y = y2;
      sgcd[sgc].gd.flags = gg_visible | gg_enabled;
      slabel[sgc].text =
        (uint32_t *) (script_filenames[i] == NULL ? "" : script_filenames[i]);
      slabel[sgc].text_is_1byte = true;
      sgcd[sgc].gd.label = &slabel[sgc];
      sgcd[sgc].gd.cid = i + CID_ScriptMFileBase;
      sgcd[sgc++].creator = GTextFieldCreate;
      sarray[si++] = &sgcd[sgc - 1];

      sgcd[sgc].gd.pos.x = 210;
      sgcd[sgc].gd.pos.y = y2;
      sgcd[sgc].gd.flags = gg_visible | gg_enabled;
      slabel[sgc].text = (uint32_t *) _("...");
      slabel[sgc].text_is_1byte = true;
      sgcd[sgc].gd.label = &slabel[sgc];
      sgcd[sgc].gd.cid = i + CID_ScriptMBrowseBase;
      sgcd[sgc].gd.handle_controlevent = Prefs_ScriptBrowse;
      sgcd[sgc++].creator = GButtonCreate;
      sarray[si++] = &sgcd[sgc - 1];
      sarray[si++] = NULL;

      y2 += 26;
    }
  sarray[si++] = GCD_Glue;
  sarray[si++] = GCD_Glue;
  sarray[si++] = GCD_Glue;
  sarray[si++] = NULL;
  sarray[si++] = NULL;

  sboxes[0].gd.flags = gg_enabled | gg_visible;
  sboxes[0].gd.u.boxelements = sarray;
  sboxes[0].creator = GHVBoxCreate;

  memset (&label, 0, sizeof (label));
  memset (&gcd, 0, sizeof (gcd));
  memset (&aspects, '\0', sizeof (aspects));
  aspects[0].selected = true;

  for (k = 0; visible_prefs_list[k].tab_name != 0; ++k)
    {
      pgcd = xcalloc (gcnt[k] + 4, sizeof (GGadgetCreateData));
      plabel = xcalloc (gcnt[k] + 4, sizeof (GTextInfo));
      hvarray = xcalloc ((gcnt[k] + 6) * 5 + 2, sizeof (GGadgetCreateData *));

      aspects[k].text = (uint32_t *) visible_prefs_list[k].tab_name;
      aspects[k].text_is_1byte = true;
      aspects[k].gcd = &boxes[2 * k];
      aspects[k].nesting = visible_prefs_list[k].nest;
      plabels[k] = plabel;

      gc = si = 0;
      for (i = line = 0, y = 5; visible_prefs_list[k].pl[i].name != NULL; ++i)
        {
          pl = &visible_prefs_list[k].pl[i];
          if (pl->dontdisplay)
            continue;
          plabel[gc].text = (uint32_t *) _(pl->name);
          plabel[gc].text_is_1byte = true;
          pgcd[gc].gd.label = &plabel[gc];
          pgcd[gc].gd.mnemonic = '\0';
          pgcd[gc].gd.popup_msg = (uint32_t *) _(pl->popup);
          pgcd[gc].gd.pos.x = 8;
          pgcd[gc].gd.pos.y = y + 6;
          pgcd[gc].gd.flags = gg_visible | gg_enabled | gg_utf8_popup;
          pgcd[gc++].creator = GLabelCreate;
          hvarray[si++] = &pgcd[gc - 1];

          plabel[gc].text_is_1byte = true;
          pgcd[gc].gd.label = &plabel[gc];
          pgcd[gc].gd.mnemonic = '\0';
          pgcd[gc].gd.popup_msg = (uint32_t *) _(pl->popup);
          pgcd[gc].gd.pos.x = 110;
          pgcd[gc].gd.pos.y = y;
          pgcd[gc].gd.flags = gg_visible | gg_enabled | gg_utf8_popup;
          pgcd[gc].data = pl;
          pgcd[gc].gd.cid = k * CID_PrefsOffset + CID_PrefsBase + i;
          switch (pl->type)
            {
            case pr_bool:
              plabel[gc].text = (uint32_t *) _("On");
              pgcd[gc].gd.pos.y += 3;
              pgcd[gc++].creator = GRadioCreate;
              hvarray[si++] = &pgcd[gc - 1];
              pgcd[gc] = pgcd[gc - 1];
              pgcd[gc].gd.pos.x += 50;
              pgcd[gc].gd.cid = 0;
              pgcd[gc].gd.label = &plabel[gc];
              plabel[gc].text = (uint32_t *) _("Off");
              plabel[gc].text_is_1byte = true;
              hvarray[si++] = &pgcd[gc];
              hvarray[si++] = GCD_Glue;
              if (*((int *) pl->val))
                pgcd[gc - 1].gd.flags |= gg_cb_on;
              else
                pgcd[gc].gd.flags |= gg_cb_on;
              ++gc;
              y += 22;
              break;
            case pr_int:
              sprintf (buf, "%d", *((int *) pl->val));
              plabel[gc].text = (uint32_t *) xstrdup_or_null (buf);
              pgcd[gc++].creator = GTextFieldCreate;
              hvarray[si++] = &pgcd[gc - 1];
              hvarray[si++] = GCD_Glue;
              hvarray[si++] = GCD_Glue;
              y += 26;
              break;
            case pr_unicode:
              /*sprintf(buf,"U+%04x", *((int *) pl->val)); */
              {
                char *pt;
                pt = buf;
                pt = utf8_idpb (pt, *((int *) pl->val));
                *pt = '\0';
              }
              plabel[gc].text = (uint32_t *) xstrdup_or_null (buf);
              pgcd[gc++].creator = GTextFieldCreate;
              hvarray[si++] = &pgcd[gc - 1];
              hvarray[si++] = GCD_Glue;
              hvarray[si++] = GCD_Glue;
              y += 26;
              break;
            case pr_real:
              sprintf (buf, "%g", *((float *) pl->val));
              plabel[gc].text = (uint32_t *) xstrdup_or_null (buf);
              pgcd[gc++].creator = GTextFieldCreate;
              hvarray[si++] = &pgcd[gc - 1];
              hvarray[si++] = GCD_Glue;
              hvarray[si++] = GCD_Glue;
              y += 26;
              break;
            case pr_encoding:
              pgcd[gc].gd.u.list = GetEncodingTypes ();
              pgcd[gc].gd.label =
                EncodingTypesFindEnc (pgcd[gc].gd.u.list,
                                      *(Encoding **) pl->val);
              for (ii = 0;
                   pgcd[gc].gd.u.list[ii].text != NULL
                   || pgcd[gc].gd.u.list[ii].line; ++ii)
                if (pgcd[gc].gd.u.list[ii].userdata != NULL
                    && (strcmp (pgcd[gc].gd.u.list[ii].userdata, "Compacted")
                        == 0
                        || strcmp (pgcd[gc].gd.u.list[ii].userdata,
                                   "Original") == 0))
                  pgcd[gc].gd.u.list[ii].disabled = true;
              pgcd[gc].creator = GListFieldCreate;
              pgcd[gc].gd.pos.width = 160;
              if (pgcd[gc].gd.label == NULL)
                pgcd[gc].gd.label = &encodingtypes[0];
              ++gc;
              hvarray[si++] = &pgcd[gc - 1];
              hvarray[si++] = GCD_ColSpan;
              hvarray[si++] = GCD_ColSpan;
              y += 28;
              break;
            case pr_namelist:
              {
                char **nlnames = AllNamelistNames ();
                int cnt;
                GTextInfo *namelistnames;
                for (cnt = 0; nlnames[cnt] != NULL; ++cnt);
                namelistnames = xcalloc (cnt + 1, sizeof (GTextInfo));
                for (cnt = 0; nlnames[cnt] != NULL; ++cnt)
                  {
                    namelistnames[cnt].text = (uint32_t *) nlnames[cnt];
                    namelistnames[cnt].text_is_1byte = true;
                    if (strcmp
                        (_((*(NameList **) (pl->val))->title),
                         nlnames[cnt]) == 0)
                      {
                        namelistnames[cnt].selected = true;
                        pgcd[gc].gd.label = &namelistnames[cnt];
                      }
                  }
                pgcd[gc].gd.u.list = namelistnames;
                pgcd[gc].creator = GListButtonCreate;
                pgcd[gc].gd.pos.width = 160;
                ++gc;
                hvarray[si++] = &pgcd[gc - 1];
                hvarray[si++] = GCD_ColSpan;
                hvarray[si++] = GCD_ColSpan;
                y += 28;
              }
              break;
            case pr_string:
            case pr_file:
              if (pl->set == SetAutoTraceArgs
                  || ((char **) pl->val) == &mf_args)
                pgcd[gc].gd.pos.width = 160;
              if (pl->val != NULL)
                tempstr = *((char **) (pl->val));
              else
                tempstr = (char *) ((pl->get) ());
              if (tempstr != NULL && u8_valid (tempstr))
                plabel[gc].text = x_u8_to_u32 (tempstr);
              else if (((char **) pl->val) == &BDFFoundry)
                plabel[gc].text = x_u8_to_u32 ("FontForge");
              else
                plabel[gc].text = x_u8_to_u32 ("");
              plabel[gc].text_is_1byte = false;
              pgcd[gc++].creator = GTextFieldCreate;
              hvarray[si++] = &pgcd[gc - 1];
              if (pl->type == pr_file)
                {
                  pgcd[gc] = pgcd[gc - 1];
                  pgcd[gc - 1].gd.pos.width = 140;
                  hvarray[si++] = GCD_ColSpan;
                  pgcd[gc].gd.pos.x += 145;
                  pgcd[gc].gd.cid += CID_PrefsBrowseOffset;
                  pgcd[gc].gd.label = &plabel[gc];
                  plabel[gc].text = (uint32_t *) "...";
                  plabel[gc].text_is_1byte = true;
                  pgcd[gc].gd.handle_controlevent = Prefs_BrowseFile;
                  pgcd[gc++].creator = GButtonCreate;
                  hvarray[si++] = &pgcd[gc - 1];
                }
              else if (pl->set == SetAutoTraceArgs
                       || ((char **) pl->val) == &mf_args)
                {
                  hvarray[si++] = GCD_ColSpan;
                  hvarray[si++] = GCD_Glue;
                }
              else
                {
                  hvarray[si++] = GCD_Glue;
                  hvarray[si++] = GCD_Glue;
                }
              y += 26;
              if (pl->val == NULL)
                free (tempstr);
              break;
            case pr_angle:
              sprintf (buf, "%g", *((float *) pl->val) * RAD2DEG);
              plabel[gc].text = (uint32_t *) xstrdup_or_null (buf);
              pgcd[gc++].creator = GTextFieldCreate;
              hvarray[si++] = &pgcd[gc - 1];
              plabel[gc].text = (uint32_t *) _("°");
              plabel[gc].text_is_1byte = true;
              pgcd[gc].gd.label = &plabel[gc];
              pgcd[gc].gd.pos.x =
                pgcd[gc - 1].gd.pos.x + gcd[gc - 1].gd.pos.width + 2;
              pgcd[gc].gd.pos.y = pgcd[gc - 1].gd.pos.y;
              pgcd[gc].gd.flags = gg_enabled | gg_visible;
              pgcd[gc++].creator = GLabelCreate;
              hvarray[si++] = &pgcd[gc - 1];
              hvarray[si++] = GCD_Glue;
              y += 26;
              break;
            }
          ++line;
          hvarray[si++] = NULL;
        }
      if (visible_prefs_list[k].pl == args_list)
        {
          static char *text[] = {
/* TRANSLATORS:
 * This and the next few strings show a limitation of my widget set which
 * cannot handle multi-line text labels. These strings should be concatenated
 * together, translated, and then broken up to fit the dialog. There is an
 * extra blank line, not used in English, into which your text may extend if
 * needed.
 */
            NC_ ("Prefs_App",
                 "Normally FontForge will find applications by searching for"),
            NC_ ("Prefs_App",
                 "them in your PATH environment variable, if you want"),
            NC_ ("Prefs_App",
                 "to alter that behavior you may set an environment"),
            NC_ ("Prefs_App",
                 "variable giving the full path spec of the application."),
            NC_ ("Prefs_App",
                 "FontForge recognizes BROWSER, MF and AUTOTRACE."),
            NC_ ("Prefs_App", " "),     /* A blank line */
            NULL
          };
          y += 8;
          for (i = 0; text[i] != 0; ++i)
            {
              plabel[gc].text =
                (uint32_t *) g_dpgettext2 (NULL, "Prefs_App", text[i]);
              plabel[gc].text_is_1byte = true;
              pgcd[gc].gd.label = &plabel[gc];
              pgcd[gc].gd.pos.x = 8;
              pgcd[gc].gd.pos.y = y;
              pgcd[gc].gd.flags = gg_visible | gg_enabled;
              pgcd[gc++].creator = GLabelCreate;
              hvarray[si++] = &pgcd[gc - 1];
              hvarray[si++] = GCD_ColSpan;
              hvarray[si++] = GCD_ColSpan;
              hvarray[si++] = NULL;
              y += 12;
            }
        }
      if (y > y2)
        y2 = y;
      hvarray[si++] = GCD_Glue;
      hvarray[si++] = GCD_Glue;
      hvarray[si++] = GCD_Glue;
      hvarray[si++] = GCD_Glue;
      hvarray[si++] = NULL;
      hvarray[si++] = NULL;
      boxes[2 * k].gd.flags = gg_enabled | gg_visible;
      boxes[2 * k].gd.u.boxelements = hvarray;
      boxes[2 * k].creator = GHVBoxCreate;
    }

  aspects[k].text = (uint32_t *) _("Script Menu");
  aspects[k].text_is_1byte = true;
  aspects[k++].gcd = sboxes;

  gc = 0;

  gcd[gc].gd.pos.x = gcd[gc].gd.pos.y = 2;
  gcd[gc].gd.pos.width = pos.width - 4;
  gcd[gc].gd.pos.height = pos.height - 2;
  gcd[gc].gd.flags = gg_enabled | gg_visible | gg_pos_in_pixels;
  gcd[gc++].creator = GGroupCreate;

  gcd[gc].gd.pos.x = 4;
  gcd[gc].gd.pos.y = 6;
  gcd[gc].gd.pos.width = GDrawPixelsToPoints (NULL, pos.width) - 8;
  gcd[gc].gd.pos.height = y2 + 20 + 18 + 4;
  gcd[gc].gd.u.tabs = aspects;
  gcd[gc].gd.flags = gg_visible | gg_enabled | gg_tabset_vert;
  gcd[gc++].creator = GTabSetCreate;
  varray[0] = &gcd[gc - 1];
  varray[1] = NULL;

  y = gcd[gc - 1].gd.pos.y + gcd[gc - 1].gd.pos.height;

  gcd[gc].gd.pos.x = 30 - 3;
  gcd[gc].gd.pos.y = y + 5 - 3;
  gcd[gc].gd.pos.width = -1;
  gcd[gc].gd.pos.height = 0;
  gcd[gc].gd.flags = gg_visible | gg_enabled | gg_but_default;
  label[gc].text = (uint32_t *) _("_OK");
  label[gc].text_is_1byte = true;
  label[gc].text_has_mnemonic = true;
  gcd[gc].gd.label = &label[gc];
  gcd[gc].gd.handle_controlevent = Prefs_Ok;
  gcd[gc++].creator = GButtonCreate;
  harray[0] = GCD_Glue;
  harray[1] = &gcd[gc - 1];
  harray[2] = GCD_Glue;
  harray[3] = GCD_Glue;

  gcd[gc].gd.pos.x = -30;
  gcd[gc].gd.pos.y = gcd[gc - 1].gd.pos.y + 3;
  gcd[gc].gd.pos.width = -1;
  gcd[gc].gd.pos.height = 0;
  gcd[gc].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  label[gc].text = (uint32_t *) _("_Cancel");
  label[gc].text_is_1byte = true;
  label[gc].text_has_mnemonic = true;
  gcd[gc].gd.label = &label[gc];
  gcd[gc].gd.handle_controlevent = Prefs_Cancel;
  gcd[gc++].creator = GButtonCreate;
  harray[4] = GCD_Glue;
  harray[5] = &gcd[gc - 1];
  harray[6] = GCD_Glue;
  harray[7] = NULL;

  memset (mboxes, 0, sizeof (mboxes));
  mboxes[2].gd.flags = gg_enabled | gg_visible;
  mboxes[2].gd.u.boxelements = harray;
  mboxes[2].creator = GHBoxCreate;
  varray[2] = &mboxes[2];
  varray[3] = NULL;
  varray[4] = NULL;

  mboxes[0].gd.pos.x = mboxes[0].gd.pos.y = 2;
  mboxes[0].gd.flags = gg_enabled | gg_visible;
  mboxes[0].gd.u.boxelements = varray;
  mboxes[0].creator = GHVGroupCreate;

  y = GDrawPointsToPixels (NULL, y + 37);
  gcd[0].gd.pos.height = y - 4;

  GGadgetsCreate (gw, mboxes);

  GHVBoxSetExpandableRow (mboxes[0].ret, 0);
  GHVBoxSetExpandableCol (mboxes[2].ret, gb_expandgluesame);
  GHVBoxSetExpandableRow (sboxes[0].ret, gb_expandglue);
  for (k = 0; k < TOPICS; ++k)
    GHVBoxSetExpandableRow (boxes[2 * k].ret, gb_expandglue);

  font = GDrawNewFont (gw, "monospace", 12, 400, fs_none);
  GHVBoxFitWindow (mboxes[0].ret);

  for (k = 0; visible_prefs_list[k].tab_name != 0; ++k)
    for (gc = 0, i = 0; visible_prefs_list[k].pl[i].name != NULL; ++i)
      {
        GGadgetCreateData *gcd = aspects[k].gcd[0].gd.u.boxelements[0];
        pl = &visible_prefs_list[k].pl[i];
        if (pl->dontdisplay)
          continue;
        switch (pl->type)
          {
          case pr_bool:
            ++gc;
            break;
          case pr_encoding:
            {
              GGadget *g = gcd[gc + 1].ret;
              list = GGadgetGetList (g, &llen);
              for (j = 0; j < llen; ++j)
                {
                  if (list[j]->text != NULL
                      && (void *) (intptr_t) (*((int *) pl->val)) ==
                      list[j]->userdata)
                    list[j]->selected = true;
                  else
                    list[j]->selected = false;
                }
              if (gcd[gc + 1].gd.u.list != encodingtypes)
                GTextInfoListFree (gcd[gc + 1].gd.u.list);
            }
            break;
          case pr_namelist:
            free (gcd[gc + 1].gd.u.list);
            break;
          case pr_string:
          case pr_file:
          case pr_int:
          case pr_real:
          case pr_unicode:
          case pr_angle:
            free (plabels[k][gc + 1].text);
            if (pl->type == pr_file || pl->type == pr_angle)
              ++gc;
            break;
          }
        gc += 2;
      }

  for (k = 0; visible_prefs_list[k].tab_name != 0; ++k)
    {
      free (aspects[k].gcd->gd.u.boxelements[0]);
      free (aspects[k].gcd->gd.u.boxelements);
      free (plabels[k]);
    }

  GDrawSetVisible (gw, true);
  while (!p.done)
    GDrawProcessOneEvent (NULL);
  GDrawDestroyWindow (gw);
}

void
RecentFilesRemember (char *filename)
{
  int i;

  for (i = 0; i < RECENT_MAX && RecentFiles[i] != NULL; ++i)
    if (strcmp (RecentFiles[i], filename) == 0)
      break;

  if (i < RECENT_MAX && RecentFiles[i] != NULL)
    {
      if (i != 0)
        {
          filename = RecentFiles[i];
          RecentFiles[i] = RecentFiles[0];
          RecentFiles[0] = filename;
        }
    }
  else
    {
      if (RecentFiles[RECENT_MAX - 1] != NULL)
        free (RecentFiles[RECENT_MAX - 1]);
      for (i = RECENT_MAX - 1; i > 0; --i)
        RecentFiles[i] = RecentFiles[i - 1];
      RecentFiles[0] = xstrdup_or_null (filename);
    }

  PrefsUI_SavePrefs (true);
}

struct prefs_interface gdraw_prefs_interface = {
  PrefsUI_SavePrefs,
  PrefsUI_LoadPrefs,
  PrefsUI_GetPrefs,
  PrefsUI_SetPrefs,
  PrefsUI_getFontForgeShareDir,
  PrefsUI_SetDefaults
};

struct prefs_list pointer_dialog_list[] = {
  {N_("ArrowMoveSize"), pr_real, &arrowAmount, NULL, NULL, '\0', NULL, 0,
   N_
   ("The number of em-units by which an arrow key will move a selected point")},
  {N_("ArrowAccelFactor"), pr_real, &arrowAccelFactor, NULL, NULL, '\0', NULL,
   0,
   N_
   ("Holding down the Alt (or Meta) key will speed up arrow key motion by this factor")},
  {N_("InterpolateCPsOnMotion"), pr_bool, &interpCPsOnMotion, NULL, NULL,
   '\0', NULL, 0,
   N_
   ("When moving one end point of a spline but not the other\ninterpolate the control points between the two.")},
  PREFS_LIST_EMPTY
};

static int
PointerDlg_Ok (GGadget *g, GEvent *e)
{
  GWindow gw = GGadgetGetWindow (g);
  struct pref_data *p = GDrawGetUserData (GGadgetGetWindow (g));
  struct prefs_list *plist = p->plist;
  struct prefs_list *pl = plist;
  int i = 0, j = 0;
  int err = 0;

  p->done = true;

  for (i = 0, pl = plist; pl->name; ++i, ++pl)
    {
      switch (pl->type)
        {
        case pr_bool:
          *((int *) (pl->val)) =
            GGadgetIsChecked (
                    GWidgetGetControl (gw, j * CID_PrefsOffset + CID_PrefsBase + i));
          break;
        case pr_real:
          *((float *) (pl->val)) =
            GetReal8 (gw, j * CID_PrefsOffset + CID_PrefsBase + i, pl->name,
                      &err);
          break;
        }
    }

  return true;
}

void
PointerDlg (CharView * cv)
{
  struct prefs_list *plist = pointer_dialog_list;
  struct prefs_list *pl = plist;
  GRect pos;
  GWindow gw;
  GWindowAttrs wattrs;
  GGadgetCreateData *pgcd, gcd[20], sgcd[45];
  GGadgetCreateData sboxes[2];
  GGadgetCreateData mboxes[3], mboxes2[5], *varray[5], *harray[8];
  GTextInfo *plabel, label[20], slabel[45];
  GTabInfo aspects[TOPICS + 4];
  GGadgetCreateData **hvarray, boxes[2 * TOPICS];
  struct pref_data p;
  int line, line_max = 3;
  int i = 0, gc = 0, y, si = 0, k = 0;
  char buf[20];

  PrefsInit ();
  MfArgsInit ();

  line_max = 0;
  for (i = 0, pl = plist; pl->name; ++i, ++pl)
    ++line_max;

  int itemCount = 100;
  pgcd = xcalloc (itemCount, sizeof (GGadgetCreateData));
  plabel = xcalloc (itemCount, sizeof (GTextInfo));
  hvarray = xcalloc ((itemCount) * 5, sizeof (GGadgetCreateData *));
  memset (&p, '\0', sizeof (p));
  memset (&wattrs, 0, sizeof (wattrs));
  memset (sgcd, 0, sizeof (sgcd));
  memset (slabel, 0, sizeof (slabel));
  memset (&sboxes, 0, sizeof (sboxes));
  memset (&boxes, 0, sizeof (boxes));
  memset (&label, 0, sizeof (label));
  memset (&gcd, 0, sizeof (gcd));
  memset (&aspects, '\0', sizeof (aspects));

  p.plist = plist;
  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_restrict
    | wam_isdlg;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.restrict_input_to_me = 1;
  wattrs.is_dlg = 1;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title = _("Arrow Options");
  pos.x = pos.y = 0;
  pos.width = GGadgetScale (GDrawPointsToPixels (NULL, 340));
  pos.height = GDrawPointsToPixels (NULL, line_max * 26 + 25);
  gw = GDrawCreateTopWindow (NULL, &pos, e_h, &p, &wattrs);

  for (i = 0, pl = plist; pl->name; ++i, ++pl)
    {
      plabel[gc].text = (uint32_t *) _(pl->name);
      plabel[gc].text_is_1byte = true;
      pgcd[gc].gd.label = &plabel[gc];
      pgcd[gc].gd.mnemonic = '\0';
      pgcd[gc].gd.popup_msg = (uint32_t *) 0;   //_(pl->popup);
      pgcd[gc].gd.pos.x = 8;
      pgcd[gc].gd.pos.y = y + 6;
      pgcd[gc].gd.flags = gg_visible | gg_enabled | gg_utf8_popup;
      pgcd[gc++].creator = GLabelCreate;
      hvarray[si++] = &pgcd[gc - 1];

      plabel[gc].text_is_1byte = true;
      pgcd[gc].gd.label = &plabel[gc];
      pgcd[gc].gd.mnemonic = '\0';
      pgcd[gc].gd.popup_msg = (uint32_t *) 0;   //_(pl->popup);
      pgcd[gc].gd.pos.x = 110;
      pgcd[gc].gd.pos.y = y;
      pgcd[gc].gd.flags = gg_visible | gg_enabled | gg_utf8_popup;
      pgcd[gc].data = pl;
      pgcd[gc].gd.cid = k * CID_PrefsOffset + CID_PrefsBase + i;
      switch (pl->type)
        {
        case pr_bool:
          plabel[gc].text = (uint32_t *) _("On");
          pgcd[gc].gd.pos.y += 3;
          pgcd[gc++].creator = GRadioCreate;
          hvarray[si++] = &pgcd[gc - 1];
          pgcd[gc] = pgcd[gc - 1];
          pgcd[gc].gd.pos.x += 50;
          pgcd[gc].gd.cid = 0;
          pgcd[gc].gd.label = &plabel[gc];
          plabel[gc].text = (uint32_t *) _("Off");
          plabel[gc].text_is_1byte = true;
          hvarray[si++] = &pgcd[gc];
          hvarray[si++] = GCD_Glue;
          if (*((int *) pl->val))
            pgcd[gc - 1].gd.flags |= gg_cb_on;
          else
            pgcd[gc].gd.flags |= gg_cb_on;
          ++gc;
          y += 22;
          break;
        case pr_real:
          sprintf (buf, "%g", *((float *) pl->val));
          plabel[gc].text = (uint32_t *) xstrdup_or_null (buf);
          pgcd[gc++].creator = GTextFieldCreate;
          hvarray[si++] = &pgcd[gc - 1];
          hvarray[si++] = GCD_Glue;
          hvarray[si++] = GCD_Glue;
          y += 26;
          break;
        }
      ++line;
      hvarray[si++] = NULL;

    }

  harray[4] = 0;
  harray[5] = 0;
  harray[6] = 0;
  harray[7] = 0;

  gcd[gc].gd.pos.x = 30 - 3;
  gcd[gc].gd.pos.y = y + 5 - 3;
  gcd[gc].gd.pos.width = -1;
  gcd[gc].gd.pos.height = 0;
  gcd[gc].gd.flags = gg_visible | gg_enabled | gg_but_default;
  label[gc].text = (uint32_t *) _("_OK");
  label[gc].text_is_1byte = true;
  label[gc].text_has_mnemonic = true;
  gcd[gc].gd.label = &label[gc];
  gcd[gc].gd.handle_controlevent = PointerDlg_Ok;
  gcd[gc++].creator = GButtonCreate;
  harray[0] = GCD_Glue;
  harray[1] = &gcd[gc - 1];
  harray[2] = GCD_Glue;
  harray[3] = GCD_Glue;

  memset (mboxes, 0, sizeof (mboxes));
  memset (mboxes2, 0, sizeof (mboxes2));

  mboxes[2].gd.pos.x = 2;
  mboxes[2].gd.pos.y = 20;
  mboxes[2].gd.flags = gg_enabled | gg_visible;
  mboxes[2].gd.u.boxelements = harray;
  mboxes[2].creator = GHBoxCreate;

  mboxes[0].gd.pos.x = mboxes[0].gd.pos.y = 2;
  mboxes[0].gd.flags = gg_enabled | gg_visible;
  mboxes[0].gd.u.boxelements = hvarray;
  mboxes[0].creator = GHVGroupCreate;

  varray[0] = &mboxes[0];
  varray[1] = &mboxes[2];
  varray[2] = 0;
  varray[3] = 0;
  varray[4] = 0;

  mboxes2[0].gd.pos.x = 4;
  mboxes2[0].gd.pos.y = 4;
  mboxes2[0].gd.flags = gg_enabled | gg_visible;
  mboxes2[0].gd.u.boxelements = varray;
  mboxes2[0].creator = GVBoxCreate;

  GGadgetsCreate (gw, mboxes2);


  GDrawSetVisible (gw, true);
  while (!p.done)
    GDrawProcessOneEvent (NULL);
  GDrawDestroyWindow (gw);
}
