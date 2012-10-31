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
#ifndef _PFAEDITUI_H_
#define _PFAEDITUI_H_

#include "fontforgevw.h"
#include <gprogress.h>

extern void help(char *filename);

# include "gdraw.h"
# include "gwidget.h"
# include "ggadget.h"
# include "views.h"

extern GCursor ct_magplus, ct_magminus, ct_mypointer,
	ct_circle, ct_square, ct_triangle, ct_pen, ct_hvcircle,
	ct_ruler, ct_knife, ct_rotate, ct_skew, ct_scale, ct_flip,
	ct_3drotate, ct_perspective,
	ct_updown, ct_leftright, ct_nesw, ct_nwse,
	ct_rect, ct_elipse, ct_poly, ct_star, ct_filledrect, ct_filledelipse,
	ct_pencil, ct_shift, ct_line, ct_myhand, ct_setwidth,
	ct_kerning, ct_rbearing, ct_lbearing, ct_eyedropper,
	ct_prohibition, ct_ddcursor, ct_spiroright, ct_spiroleft, ct_g2circle,
	ct_features;
extern GWindow logo_icon;

extern GMenuItem2 cvtoollist[], cvspirotoollist[];

extern GTextInfo encodingtypes[];
extern GTextInfo *EncodingTypesFindEnc(GTextInfo *encodingtypes, Encoding *enc);
extern Encoding *ParseEncodingNameFromList(GGadget *listfield);
extern GTextInfo *GetEncodingTypes(void);
extern void cvtoollist_check(GWindow gw,struct gmenuitem *mi,GEvent *e);

extern void InitCursors(void);

extern int ErrorWindowExists(void);
extern void ShowErrorWindow(void);
VISIBLE extern struct ui_interface gdraw_ui_interface;
VISIBLE extern struct prefs_interface gdraw_prefs_interface;
VISIBLE extern struct sc_interface gdraw_sc_interface;
VISIBLE extern struct cv_interface gdraw_cv_interface;
VISIBLE extern struct bc_interface gdraw_bc_interface;
VISIBLE extern struct mv_interface gdraw_mv_interface;
VISIBLE extern struct fv_interface gdraw_fv_interface;
extern struct fi_interface gdraw_fi_interface;
extern struct clip_interface gdraw_clip_interface;

extern int ItalicConstrained;
extern uint32_t *script_menu_names[SCRIPT_MENU_MAX];
extern char *script_filenames[SCRIPT_MENU_MAX];
extern char *RecentFiles[RECENT_MAX];

/* I would like these to be const ints, but gcc doesn't treat them as consts */
#define et_sb_halfup et_sb_thumbrelease+1
#define et_sb_halfdown  et_sb_thumbrelease+2

extern FontView *fv_list;

extern struct openfilefilters { char *name, *filter; } def_font_filters[], *user_font_filters;
extern int default_font_filter_index;

#if !defined( FONTFORGE_CONFIG_CAPSLOCK_FOR_ALT ) || FONTFORGE_CONFIG_CAPSLOCK_FOR_ALT==0
# define ksm_alt	ksm_meta
#elif FONTFORGE_CONFIG_CAPSLOCK_FOR_ALT-2 == 0	/* I use this peculiar construction just in case it is defined as the empty string */
# define ksm_alt	(ksm_meta|ksm_capslock)
#else
# define ksm_alt	ksm_capslock
#endif

#define SERIF_UI_FAMILIES	"dejavu serif,times,caslon,serif,clearlyu,unifont"
#define SANS_UI_FAMILIES	"dejavu sans,helvetica,caliban,sans,clearlyu,unifont"
#define MONO_UI_FAMILIES	"courier,monospace,clearlyu,unifont"
#define FIXED_UI_FAMILIES	"monospace,fixed,clearlyu,unifont"

#define isprivateuse(enc) ((enc)>=0xe000 && (enc)<=0xf8ff)
#endif
