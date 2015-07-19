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
#ifndef _VIEWS_H
#define _VIEWS_H

#include "baseviews.h"

#include <ggadget.h>
#include <usermenu.h>

struct gfi_data;
struct contextchaindlg;

struct cvshows
{
  int showfore;
  int showback;
  int showgrids;
  int showhhints;
  int showvhints;
  int showdhints;
  int showpoints;
  int showfilled;
  int showrulers;
  int showrounds;               /* 0=>no, 1=>auto, 2=>always */
  int showmdx;                  /* minimum distance x */
  int showmdy;                  /* minimum distance y */
  int showhmetrics;
  int showvmetrics;             /* show advance width, baseline, etc. */
  int markextrema;
  int markpoi;                  /* Points of inflection */
  int showblues;
  int showfamilyblues;
  int showanchor;
  int showcpinfo;
  int showtabs;                 /* with the names of former glyphs */
  int showsidebearings;
  int showrefnames;
  int snapoutlines;
  int showalmosthvlines;
  int showalmosthvcurves;
  int hvoffset;
  int checkselfintersects;      /* Not really something shown, but convenient to keep it here */
  int showdebugchanges;         /* Changes the way changing rasters are displayed in tt debug mode */
};

struct bvshows
{
  int showfore;
  int showoutline;
  int showgrid;
  int lastpixelsize;
};

extern struct cvshows CVShows;
extern struct bvshows BVShows;

enum debug_wins
{
  dw_registers = 0x1,
  dw_stack = 0x2,
  dw_storage = 0x4,
  dw_points = 0x8,
  dw_cvt = 0x10,
  dw_raster = 0x20,
  dw_gloss = 0x40
};

struct instrinfo
{
  int isel_pos;
  int16_t lheight, lpos;
  char *scroll, *offset;
  GWindow v;
  GGadget *vsb;
  int16_t sbw;
  int16_t vheight, vwidth;
  int16_t lstopped;
  int16_t as, fh;
  struct instrdata *instrdata;
  GFont *gfont;
  bool showaddr;
  bool showhex;
  bool mousedown;
  void *userdata;
  void (*selection_callback) (struct instrinfo *, int ip);
  int (*bpcheck) (struct instrinfo *, int ip);
  int (*handle_char) (struct instrinfo *, GEvent *e);
};

struct reflist
{
  RefChar *ref;
  struct reflist *parent;
};

typedef struct debugview
{
  struct debugger_context *dc;  /* Local to freetype.c */
  GWindow dv, v;
  /* Windows for twilight points, cvt, registers, stack, storage, stack gloss */
  GWindow regs, stack, storage, points, cvt, raster, gloss;     /* order matters */
  GWindow points_v;
  GGadget *cvtsb;
  GGadget *pts_vsb;
  GGadget *glosssb;
  GGadget *storagesb;
  GGadget *regsb;
  GGadget *stacksb;
  struct instrdata id;
  struct instrinfo ii;
  int dwidth, toph;
  struct charview *cv;
  double scalex, scaley;
  int pts_head;
  int cvt_offtop;
  int gloss_offtop;
  int storage_offtop;
  int stack_offtop;
  int reg_offtop;
  int points_offtop;

  int codeSize;
  uint8_t initialbytes[4];
  struct reflist *active_refs;
  int last_npoints;
  int layer;
} DebugView;

enum dv_coderange               /* cleverly chosen to match ttobjs.h */
{
  cr_none = 0,
  cr_fpgm,
  cr_prep,
  cr_glyph
};

struct freehand
{
  struct tracedata *head, *last;        /* for the freehand tool */
  SplinePointList *current_trace;
  int ignore_wobble;            /* Ignore wiggles smaller than this */
  int skip_cnt;
};

enum expandedge
{
  ee_none,
  ee_nw,
  ee_up,
  ee_ne,
  ee_right,
  ee_se,
  ee_down,
  ee_sw,
  ee_left,
  ee_leftright,
  ee_max
};

typedef struct charview
{
  CharViewBase b;
  uint32_t showback[BACK_LAYER_MAX / 32];
  bool showfore;
  bool showgrids;
  bool showhhints;
  bool showvhints;
  bool showdhints;
  bool showpoints;
  bool showfilled;
  bool showrulers;
  unsigned int showrounds:2;    /* 0=>no, 1=>auto, 2=>always */
  bool showmdx;
  bool showmdy;
  bool showhmetrics;
  bool showvmetrics;
  bool showblues;               /* 16 */
  bool showfamilyblues;
  bool showanchor;
  bool showpointnumbers;
  bool markextrema;
  bool markpoi;
  bool needsrasterize;          /* Rasterization (of fill or fontview) needed on mouse up */
  bool recentchange;            /* a change happened in the grids or background. don't need to rasterize */
  bool info_within;             /* cursor is within main window */
  bool cntrldown;
  bool joinvalid;
  bool widthsel;
  bool vwidthsel;
  bool icsel;
  bool tahsel;
  bool inactive;                /* When in a search view */
  bool show_ft_results;         /* 32 */
  unsigned int coderange:2;     /* For the debugger */
  bool autonomous_ruler_w;
  bool showcpinfo;
  bool showtabs;
  bool showsidebearings;
  bool showing_spiro_pt_menu;
  bool ruler_pressed;
  bool ruler_pressedv;
  bool showrefnames;
  bool snapoutlines;
  bool showalmosthvlines;
  bool showalmosthvcurves;
  bool checkselfintersects;
  bool showdebugchanges;
  bool inPreviewMode;
  int hvoffset;                 /* for showalmosthvlines */
  int layers_off_top;
  real scale;
  GWindow gw, v;
  GGadget *vsb, *hsb, *mb, *tabs;
  GFont *small, *normal;
  GWindow icon;
  GWindow ruler_w;
  int num_ruler_intersections;
  int allocated_ruler_intersections;
  BasePoint *ruler_intersections;
  GFont *rfont;
  GDTimer *pressed;
  GIC *gic;
  GIC *gwgic;
  int width, height;
  float xoff, yoff;             /* must be floating point, for precise zoom by scroll */
  int mbh, infoh, rulerh;
  int16_t sas, sfh, sdh, nas, nfh;
  BasePoint info;
  SplinePoint *info_sp;
  Spline *info_spline;
  real info_t;
  GPoint e;                     /* mouse location */
  GPoint olde;
  BasePoint last_c;
  BDFChar *filled;
  GImage gi;                    /* used for fill bitmap only */
  int enc;
  EncMap *map_of_enc;           /* Only use for comparison against fontview's map to see if our enc be valid */
  /*  Will not be updated when fontview is reencoded */
  PressedOn p;
  SplinePoint *lastselpt;
  spiro_cp *lastselcp;
  /*GWindow tools, layers; */
  int8_t b1_tool, cb1_tool, b2_tool, cb2_tool;  /* Button 3 does a popup */
  int8_t b1_tool_old;           /* Used by mingw port */
  int8_t s1_tool, s2_tool, er_tool;     /* Bindings for wacom stylus and eraser */
  int8_t showing_tool, pressed_tool, pressed_display, had_control, active_tool;
  int8_t spacebar_hold;         /* spacebar is held down */
  SplinePointList *active_spl;
  SplinePoint *active_sp;
  spiro_cp *active_cp;
  IPoint handscroll_base;
  uint16_t rfh, ras;
  BasePoint lastknife;
  struct freehand freehand;
  enum expandedge expandedge;
  BasePoint expandorigin;
  real expandwidth, expandheight;
  SplinePointList *active_shape;
  SplinePoint joinpos;
  spiro_cp joincp;
  SplineChar *template1, *template2;
  real oldwidth, oldvwidth;
  int16_t oldic, oldtah;
  PST *lcarets;
  int16_t nearcaret;
  /* freetype results display */
  int16_t ft_dpi, ft_ppemy, ft_ppemx, ft_depth;
  real ft_pointsizey, ft_pointsizex;
  struct freetype_raster *raster, *oldraster;
  DebugView *dv;
  uint32_t mmvisible;
  char *former_names[FORMER_MAX];
  int former_cnt;
  AnchorPoint *apmine, *apmatch;
  SplineChar *apsc;
  int guide_pos;
  struct qg_data *qg;
  int16_t note_x, note_y;
} CharView;

typedef struct bitmapview
{
  BDFChar *bc;
  BDFFont *bdf;
  struct fontview *fv;
  EncMap *map_of_enc;
  int enc;
  GWindow gw, v;
  GGadget *vsb, *hsb, *mb;
  GGadget *recalc;
  GFont *small;
  int xoff, yoff;
  int width, height;
  int infoh, mbh;
  int scale;
  real scscale;
  struct bitmapview *next;
  bool showfore;
  bool showoutline;
  bool showgrid;
  bool cntrldown;
  bool recentchange;
  bool clearing;
  bool shades_hidden;
  bool shades_down;
  /*GWindow tools, layers; */
  int8_t b1_tool, cb1_tool, b2_tool, cb2_tool;  /* Button 3 does a popup */
  int8_t s1_tool, s2_tool, er_tool;     /* Bindings for wacom stylus and eraser */
  int8_t showing_tool, pressed_tool, pressed_display, had_control, active_tool;
  int pressed_x, pressed_y;
  int info_x, info_y;
  int event_x, event_y;
  int16_t sas, sfh;
  int color;                    /* for greyscale fonts (between 0,255) */
  int color_under_cursor;
} BitmapView;

struct aplist
{
  AnchorPoint *ap;
  int connected_to, selected;
  struct aplist *next;
};

enum mv_grids
{
  mv_hidegrid,
  mv_showgrid,
  mv_partialgrid,
  mv_hidemovinggrid
};

enum mv_type
{
  mv_kernonly,
  mv_widthonly,
  mv_kernwidth
};

struct metricchar
{
  int16_t dx, dwidth;           /* position and width of the displayed char */
  int16_t dy, dheight;          /*  displayed info for vertical metrics */
  int xoff, yoff;
  int16_t mx, mwidth;           /* position and width of the text underneath */
  int16_t kernafter;
  bool selected;
  GGadget *width, *lbearing, *rbearing, *kern, *name;
  GGadget *updownkparray[10];   /* Cherry picked elements from width...kern allowing up/down key navigation */
};

typedef struct metricsview
{
  struct fontview *fv;
  SplineFont *sf;
  int pixelsize;                /* If the user has manually requested a pixelsize */
  /*  then rasterize at that size no matter how large */
  /*  the font is zoomed. For non-user requesed sizes */
  /*  this is the pixelsize * zoom-factor */
  BDFFont *bdf;                 /* We can also see metric info on a bitmap font */
  BDFFont *show;                /*  Or the rasterized version of the outline font */
  GWindow gw, v;
  GFont *font;
  GGadget *hsb, *vsb, *mb, *text, *script, *features, *subtable_list;
  GGadget *namelab, *widthlab, *lbearinglab, *rbearinglab, *kernlab;
  int16_t xstart;
  int16_t width, height, dwidth;
  int16_t vwidth, vheight;
  int16_t mbh, sbh;
  int16_t topend;               /* y value of the end of the region containing the text field */
  int16_t displayend;           /* y value of the end of the region showing filled characters */
  int16_t fh, as;
  int16_t cmax, clen;
  SplineChar **chars;           /* Character input stream */
  struct opentype_str *glyphs;  /* after going through the various gsub/gpos transformations */
  struct metricchar *perchar;   /* One for each glyph above */
  SplineChar **sstr;            /* Character input stream */
  int16_t mwidth, mbase;
  int16_t glyphcnt, max;
  int16_t pressed_x, pressed_y;
  int16_t activeoff;
  int xoff, coff, yoff;
  struct metricsview *next;
  bool right_to_left;
  bool pressed;
  bool pressedwidth;
  bool pressedkern;
  unsigned int showgrid:2;
  bool antialias;
  bool vertical;
  unsigned int type:2;          /* enum mv_type */
  unsigned int pixelsize_set_by_window;
  int xp, yp, ap_owner;
  BasePoint ap_start;
  int cursor;
  int scale_index;
  struct lookup_subtable *cur_subtable;
  GTextInfo *scriptlangs;
  int word_index;
  int layer;
  int fake_unicode_base;
  GIC *gwgic;
  int ptsize, dpi;
  int ybaseline;
  int oldscript, oldlang;
} MetricsView;

enum fv_metrics
{
  fvm_baseline = 1,
  fvm_origin = 2,
  fvm_advanceat = 4,
  fvm_advanceto = 8
};

typedef struct fontview
{
  FontViewBase b;
  BDFFont *show, *filled;
  GWindow gw, v;
  GFont **fontset;
  GGadget *vsb, *mb;
  GDTimer *pressed;
  GDTimer *resize;
  GEvent resize_event;
  GIC *gic;
  GIC *gwgic;
  int width, height;            /* of v */
  int16_t infoh, mbh;
  int16_t lab_height, lab_as;
  int16_t colcnt, rowcnt;       /* of display window */
  int32_t rowoff, rowltot;      /* Can be really big in full unicode */
  int16_t cbw, cbh;             /* width/height of a character box */
  int pressed_pos, end_pos;
  bool antialias;
  bool bbsized;                 /* displayed bitmap should be scaled by bounding box rather than emsize */
  bool wasonlybitmaps;
  /*unsigned int refstate: 3; *//* 0x1 => paste orig of all non exist refs, 0x2=>don't, 0x3 => don't warn about non-exist refs with no source font */
  bool touched;
  unsigned int showhmetrics:4;
  unsigned int showvmetrics:4;
  bool drag_and_drop;
  bool has_dd_no_cursor;
  bool any_dd_events_sent;
  bool resize_expected;
  /* Some window managers do not honour my resize requests (if window is */
  /*  maximized for example), but we depend on the resize request to    */
  /*  fix up the window. We do get a configure notify, but the window   */
  /*  stays the same size, so kludge things */
  unsigned int glyphlabel:2;
  bool notactive;               /* When embedded in a dlg */
  int16_t magnify;
  int16_t user_requested_magnify;
  struct searchview *sv;
  SplineChar *sc_near_top;
  int sel_index;
  struct lookup_subtable *cur_subtable;
  struct qg_data *qg;
} FontView;

typedef struct findsel
{
  GEvent *e;
  real fudge;                   /* One pixel fudge factor */
  real xl, xh, yl, yh;          /* One pixel fudge factor */
  real c_xl, c_xh, c_yl, c_yh;  /* fudge rectangle for control points, larger than above if alt is depressed */
  bool select_controls;         /* notice control points */
  bool seek_controls;           /* notice control points before base points */
  bool all_controls;            /* notice control points even if the base points aren't selected (in truetype point numbering mode where all cps are visible) */
  real scale;
  PressedOn *p;
} FindSel;

typedef struct searchview
{
  struct cvcontainer base;
  FontView dummy_fv;
  SplineFont dummy_sf;
  LayerInfo layerinfo[2];
  SplineChar *chars[2];
  EncMap dummy_map;
  uint8_t sel[2];
  CharView cv_srch, cv_rpl;
  CharView *lastcv;
/* ****** */
  GWindow gw;
  GGadget *mb;
  GFont *plain, *bold;
  int mbh;
  int fh, as;
  int rpl_x, cv_y;
  int cv_width, cv_height;
  short button_height, button_width;
/* ****** */
  SearchData sd;
  bool showsfindnext;
  bool findenabled;
  bool rplallenabled;
  bool rplenabled;
  bool isvisible;
} SearchView;

typedef struct mathkernview
{
  struct cvcontainer base;
  FontView dummy_fv;
  SplineFont dummy_sf;
  LayerInfo layerinfo[2];
  SplineChar sc_topright, sc_topleft, sc_bottomright, sc_bottomleft;
  SplineChar *chars[4];
  EncMap dummy_map;
  uint8_t sel[4];
  CharView cv_topright, cv_topleft, cv_bottomright, cv_bottomleft;
  CharView *lastcv;
/* ****** */
  GWindow gw;
  GWindow cvparent_w;
  GGadget *mb;
  GFont *plain, *bold;
  int mbh;
  int fh, as;
  int mid_space, cv_y;
  int cv_width, cv_height;
  short button_height, button_width;
/* ****** */
  SplineChar *cursc;
  int def_layer;
  struct mathkern *orig_mathkern;
  uint8_t saved_mathkern;       /* Can't just check if orig is non-NULL, because NULL is a perfectly valid initial state */
  uint8_t last_aspect;
  uint8_t done;
} MathKernDlg;

#ifdef FONTFORGE_CONFIG_TILEPATH

typedef struct tilepathdlg
{
  struct cvcontainer base;
  FontView dummy_fv;
  SplineFont dummy_sf;
  LayerInfo layerinfo[2];
  SplineChar sc_first, sc_medial, sc_final, sc_isolated;
  SplineChar *chars[4];
  EncMap dummy_map;
  uint8_t sel[4];
  CharView cv_first, cv_medial, cv_final, cv_isolated;
  CharView *lastcv;
/* ****** */
  GWindow gw;
  GGadget *mb;
  GFont *plain, *bold;
  int mbh;
  int fh, as;
  int mid_space, cv_y;
  int cv_width, cv_height;
/* ****** */
  struct tiledata *td;
  SplineFont *base_sf;
  uint8_t done;
  uint8_t oked;
} TilePathDlg;

void TPDCharViewInits (TilePathDlg *tpd, int cid);
void PTDCharViewInits (TilePathDlg *tpd, int cid);

#endif /* Tile Path */

typedef struct gradientdlg
{
  struct cvcontainer base;
  FontView dummy_fv;
  SplineFont dummy_sf;
  LayerInfo layerinfo[2];
  SplineChar sc_grad;
  SplineChar *chars[1];
  EncMap dummy_map;
  uint8_t sel[1];
  CharView cv_grad;
/* ****** */
  GWindow gw;
  GGadget *mb;
  GFont *plain, *bold;
  int mbh;
  int fh, as;
  int mid_space, cv_y;
  int cv_width, cv_height;
/* ****** */
  uint8_t done, oked;
  struct gradient *active;
} GradientDlg;

void GDDCharViewInits (GradientDlg *gdd, int cid);

typedef struct strokedlg
{
  struct cvcontainer base;
  FontView dummy_fv;
  SplineFont dummy_sf;
  LayerInfo layerinfo[2];
  SplineChar sc_stroke;
  SplineChar *chars[1];
  EncMap dummy_map;
  uint8_t sel[1];
  CharView cv_stroke;
  int cv_width, cv_height;
  GGadget *mb;
  int mbh;
  SplineSet *old_poly;
/* ****** */
  int done;
  GWindow gw;
  CharView *cv;
  FontView *fv;
  SplineFont *sf;
  void (*strokeit) (void *, StrokeInfo *, int);
  StrokeInfo *si;
  GRect r1, r2;
  int up[2];
  int dontexpand;
} StrokeDlg;

void StrokeCharViewInits (StrokeDlg *sd, int cid);

struct lksubinfo
{
  struct lookup_subtable *subtable;
  bool deleted;
  bool new;
  bool selected;
  bool moved;
};

struct lkinfo
{
  OTLookup *lookup;
  bool open;
  bool deleted;
  bool new;
  bool selected;
  bool moved;
  int16_t subtable_cnt, subtable_max;
  struct lksubinfo *subtables;
};

struct lkdata
{
  int cnt;
  int max;
  int off_top;
  int off_left;
  struct lkinfo *all;
};

struct anchor_shows
{
  CharView *cv;
  SplineChar *sc;
  int restart;
};

struct gfi_data
{                               /* FontInfo */
  SplineFont *sf;
  int def_layer;
  GWindow gw;
  int tn_active;
  int private_aspect;
  int ttfv_aspect;
  int tn_aspect;
  int tx_aspect;
  int unicode_aspect;
  int old_sel;
  int old_aspect;
  int old_lang;
  int old_strid;
  int ttf_set;
  int names_set;
  int tex_set;
  int langlocalecode;           /* MS code for the current locale */
  bool family_untitled;
  bool human_untitled;
  bool done;
  bool mpdone;
  bool lk_drag_and_drop;
  bool lk_dropablecursor;
  struct anchor_shows anchor_shows[2];
  struct texdata texdata;
  GFont *font;
  int as;
  int fh;
  struct lkdata tables[2];
  int lkwidth;
  int lkheight;
  int first_sel_lookup;
  int first_sel_subtable;
  int last_panose_family;
};

struct kf_dlg                   /* : fvcontainer */
{
  struct fvcontainer base;
  struct lookup_subtable *sub;
  GWindow gw, dw;
  GFont *plain, *bold;
  int fh, as;
  GGadget *mb, *guts, *topbox;
  int mbh, label2_y, infoh;

  SplineFont *sf;
  int def_layer;
  struct kf_results *results;
  int done;

  FontView *active;
  FontView *first_fv;
  FontView *second_fv;
};

enum genfam
{
  gf_none,
  gf_macfamily,
  gf_ttc
};

void FVMarkHintsOutOfDate (SplineChar *sc);
void FVRefreshChar (FontView *fv, int gid);
int _FVMenuSave (FontView *fv);
int _FVMenuSaveAs (FontView *fv);
int _FVMenuGenerate (FontView *fv, int family);
void _FVCloseWindows (FontView *fv);
char *GetFontNameDialog (char *defdir, int mult);
void MergeKernInfo (SplineFont *sf, EncMap *map);
#if FONTFORGE_CONFIG_WRITE_PFM
int WritePfmFile (char *filename, SplineFont *sf, int type0, EncMap *map);
#endif
int SFGenerateFont (SplineFont *sf, int layer, int family, EncMap *map);

void NonLinearDlg (FontView *fv, struct charview *cv);
void FVChangeChar (FontView *fv, int encoding);
VISIBLE void FVMergeFonts (FontView *fv);
VISIBLE void FVInterpolateFonts (FontView *fv);

void FVDeselectAll (FontView *fv);

VISIBLE void FVAutoWidth2 (FontView *fv);
/*void FVAutoKern(FontView *fv);*/
/*void FVAutoWidth(FontView *fv);*/

void SC_MarkInstrDlgAsChanged (SplineChar *sc);

void SCStroke (SplineChar *sc);

void PfaEditSetFallback (void);
void RecentFilesRemember (char *filename);


struct debugger_context;
void DebuggerTerminate (struct debugger_context *dc);
void DebuggerReset (struct debugger_context *dc, real pointsizey,
                    real pointsizex, int dpi, int dbg_fpgm, int is_bitmap);
struct debugger_context *DebuggerCreate (SplineChar *sc, int layer,
                                         real pointsizey,
                                         real pointsizex, int dpi,
                                         int dbg_fpgm, int is_bitmap);

enum debug_gotype
{
  dgt_continue,
  dgt_step,
  dgt_next,
  dgt_stepout
};

void DebuggerGo (struct debugger_context *dc, enum debug_gotype, DebugView *dv);
struct TT_ExecContextRec_ *DebuggerGetEContext (struct debugger_context *dc);
void DebuggerToggleBp (struct debugger_context *dc, int range, int ip);
int DebuggerBpCheck (struct debugger_context *dc, int range, int ip);
void DebuggerSetWatches (struct debugger_context *dc, int n, uint8_t *w);
uint8_t *DebuggerGetWatches (struct debugger_context *dc, int *n);
void DebuggerSetWatchStores (struct debugger_context *dc, int n, uint8_t *w);
uint8_t *DebuggerGetWatchStores (struct debugger_context *dc, int *n);
int DebuggerIsStorageSet (struct debugger_context *dc, int index);
void DebuggerSetWatchCvts (struct debugger_context *dc, int n, uint8_t *w);
uint8_t *DebuggerGetWatchCvts (struct debugger_context *dc, int *n);
int DebuggingFpgm (struct debugger_context *dc);


void PrintDlg (FontView *fv, SplineChar *sc, MetricsView *mv);
void PrintWindowClose (void);
void InsertTextDlg (CharView *cv);

char *Kern2Text (SplineChar *other, KernPair *kp, int isv);
char *PST2Text (PST *pst, SplineFont *sf);

void EmboldenDlg (FontView *fv, CharView *cv);
void CondenseExtendDlg (FontView *fv, CharView *cv);
void AddSmallCapsDlg (FontView *fv);
void AddSubSupDlg (FontView *fv);
void ObliqueDlg (FontView *fv, CharView *cv);
void GlyphChangeDlg (FontView *fv, CharView *cv, enum glyphchange_type gc);
void ItalicDlg (FontView *fv, CharView *cv);
VISIBLE void ChangeXHeightDlg (FontView *fv, CharView *cv);

int FVParseSelectByPST (FontView *fv, struct lookup_subtable *sub,
                        int search_type);
void DropChars2Text (GWindow gw, GGadget *glyphs, GEvent *event);

void FVReplaceOutlineWithReference (FontView *fv, double fudge);
void SVDestroy (struct searchview *sv);

int SLICount (SplineFont *sf);
PST *AddSubs (PST *last, uint32_t tag, char *name, uint16_t flags,
              uint16_t sli, SplineChar *sc);

void FVSetUIToMatch (FontView *destfv, FontView *srcfv);
void FVScrollToChar (FontView *fv, int i);
void FVRegenChar (FontView *fv, SplineChar *sc);
FontView *FontNew (void);
void _MenuWarnings (GWindow gw, struct gmenuitem *mi, GEvent *e);
void MenuPrefs (GWindow base, struct gmenuitem *mi, GEvent *e);
void MenuSaveAll (GWindow base, struct gmenuitem *mi, GEvent *e);
void MenuExit (GWindow base, struct gmenuitem *mi, GEvent *e);
void MenuOpen (GWindow base, struct gmenuitem *mi, GEvent *e);
void MenuHelp (GWindow base, struct gmenuitem *mi, GEvent *e);
void MenuIndex (GWindow base, struct gmenuitem *mi, GEvent *e);
void MenuAbout (GWindow base, struct gmenuitem *mi, GEvent *e);
void MenuLicense (GWindow base, struct gmenuitem *mi, GEvent *e);
void MenuNew (GWindow gw, struct gmenuitem *mi, GEvent *e);
void WindowMenuBuild (GWindow base, struct gmenuitem *mi, GEvent *);
void MenuRecentBuild (GWindow base, struct gmenuitem *mi, GEvent *);
void MenuScriptsBuild (GWindow base, struct gmenuitem *mi, GEvent *);
void mb2DoGetText (GMenuItem *mb);
void mbDoGetText (GMenuItem *mb);
int RecentFilesAny (void);
void _aplistbuild (struct gmenuitem *mi, SplineFont *sf,
                   void (*func) (GWindow, struct gmenuitem *, GEvent *));
int32_t *ParseBitmapSizes (GGadget *g, char *msg, int *err);
uint32_t *AskNameTag (char *title, uint32_t *def, uint32_t def_tag,
                      uint16_t flags, int script_lang_index,
                      enum possub_type type, SplineFont *sf,
                      SplineChar *default_script, int merge_with, int act_type);
uint32_t *ShowScripts (uint32_t *usedef);
GTextInfo *SFLangList (SplineFont *sf, int addfinal,
                       SplineChar *default_script);
GTextInfo **SFLangArray (SplineFont *sf, int addfinal);
int ScriptLangList (SplineFont *sf, GGadget *list, int sli);
void GListDelSelected (GGadget *list);
void GListMoveSelected (GGadget *list, int offset);
GTextInfo *GListChangeLine (GGadget *list, int pos, const uint32_t *line);
GTextInfo *GListAppendLine (GGadget *list, const uint32_t *line, int select);
GTextInfo *GListChangeLine8 (GGadget *list, int pos, const char *line);
GTextInfo *GListAppendLine8 (GGadget *list, const char *line, int select);
void CharInfoInit (void);
void SCLigCaretCheck (SplineChar *sc, int clean);
char *DevTab_Dlg (GGadget *g, int r, int c);
int DeviceTableOK (char *dvstr, int *_low, int *_high);
void VRDevTabParse (struct vr *vr, struct matrix_data *md);
DeviceTable *DeviceTableParse (DeviceTable *dv, char *dvstr);
void DevTabToString (char **str, DeviceTable *adjust);
void ValDevTabToStrings (struct matrix_data *mds, int first_offset,
                         ValDevTab * adjust);
void KpMDParse (SplineChar *sc, struct lookup_subtable *sub,
                struct matrix_data *possub, int rows, int cols, int i);
void GFI_LookupEnableButtons (struct gfi_data *gfi, int isgpos);
void GFI_LookupScrollbars (struct gfi_data *gfi, int isgpos, int refresh);
void FontInfo (SplineFont *sf, int layer, int aspect, int sync);
void FontInfoDestroy (SplineFont *sf);
void FontMenuFontInfo (void *fv);
struct enc *MakeEncoding (SplineFont *sf, EncMap *map);
void LoadEncodingFile (void);
void RemoveEncoding (void);
void SFPrivateInfo (SplineFont *sf);
void FVDelay (FontView *fv, void (*func) (FontView *));
void GFI_FinishContextNew (struct gfi_data *d, FPST *fpst, int success);
void SCPreparePopup (GWindow gw, SplineChar *sc, struct remap *remap,
                     int enc, int actualuni);

enum outlinesfm_flags
{
  sfm_stroke = 0x1,
  sfm_fill = 0x2,
  sfm_nothing = 0x4
};

void CVDrawSplineSetSpecialized (CharView *cv, GWindow pixmap,
                                 SplinePointList *set, Color fg,
                                 int dopoints, DRect *clip,
                                 enum outlinesfm_flags strokeFillMode);
void CVDrawSplineSet (CharView *cv, GWindow pixmap,
                      SplinePointList *set, Color fg, int dopoints,
                      DRect *clip);
void CVDrawSplineSetOutlineOnly (CharView *cv, GWindow pixmap,
                                 SplinePointList *set, Color fg,
                                 int dopoints, DRect *clip,
                                 enum outlinesfm_flags strokeFillMode);
GWindow CVMakeTools (CharView *cv);
GWindow CVMakeLayers (CharView *cv);
GWindow BVMakeTools (BitmapView *bv);
GWindow BVMakeLayers (BitmapView *bv);
void CVSetLayer (CharView *cv, int layer);
int CVPaletteMnemonicCheck (GEvent *event);
int TrueCharState (GEvent *event);
void CVToolsPopup (CharView *cv, GEvent *event);
void BVToolsPopup (BitmapView *bv, GEvent *event);
real CVRoundRectRadius (void);
int CVRectElipseCenter (void);
void CVRectEllipsePosDlg (CharView *cv);
real CVStarRatio (void);
int CVPolyStarPoints (void);
StrokeInfo *CVFreeHandInfo (void);
void BVToolsSetCursor (BitmapView *bv, int state, char *device);
void CVToolsSetCursor (CharView *cv, int state, char *device);
int CVPaletteIsVisible (CharView *cv, int which);
void CVPaletteSetVisible (CharView *cv, int which, int visible);
void CVPalettesRaise (CharView *cv);
void CVLayersSet (CharView *cv);
void _CVPaletteActivate (CharView *cv, int force);
void CVPaletteActivate (CharView *cv);
void CV_LayerPaletteCheck (SplineFont *sf);
void CVPalettesHideIfMine (CharView *cv);
int BVPaletteIsVisible (BitmapView *bv, int which);
void BVPaletteSetVisible (BitmapView *bv, int which, int visible);
void BVPaletteActivate (BitmapView *bv);
void BVPalettesHideIfMine (BitmapView *bv);
void BVPaletteColorChange (BitmapView *bv);
void BVPaletteColorUnderChange (BitmapView *bv, int color);
void BVPaletteChangedChar (BitmapView *bv);
void CVPaletteDeactivate (void);
void PalettesChangeDocking (void);
int CVPalettesWidth (void);
int BVPalettesWidth (void);

void CVDoTransform (CharView *cv, enum cvtools cvt);
void CVTransFunc (CharView *cv, real transform[6], enum fvtrans_flags);

enum transdlg_flags
{
  tdf_enableback = 0x1,
  tdf_enablekerns = 0x2,
  tdf_defaultkerns = 0x4,
  tdf_addapply = 0x8
};

void TransformDlgCreate (void *data,
                         void (*transfunc) (void *, real *, int,
                                            BVTFunc *,
                                            enum fvtrans_flags),
                         int (*getorigin) (void *, BasePoint *, int),
                         enum transdlg_flags flags, enum cvtools cvt);
void BitmapDlg (FontView *fv, SplineChar *sc, int isavail);
int SimplifyDlg (SplineFont *sf, struct simplifyinfo *smpl);
void CVReviewHints (CharView *cv);
void CVCreateHint (CharView *cv, int ishstem, int preserveundoes);
int CVExport (CharView *cv);
int BVExport (BitmapView *bv);

void DrawAnchorPoint (GWindow pixmap, int x, int y, int selected);
void DefaultY (GRect *pos);
void CVDrawRubberRect (GWindow pixmap, CharView *cv);
void CVInfoDraw (CharView *cv, GWindow pixmap);
void CVChar (CharView *cv, GEvent *event);
void PI_ShowHints (SplineChar *sc, GGadget *list, int set);
GTextInfo *SCHintList (SplineChar *sc, HintMask *);
void CVResize (CharView *cv);
CharView *CharViewCreate (SplineChar *sc, FontView *fv, int enc);
void CharViewFree (CharView *cv);
int CVValid (SplineFont *sf, SplineChar *sc, CharView *cv);
void CVSetCharChanged (CharView *cv, int changed);
int CVAnySel (CharView *cv, int *anyp, int *anyr, int *anyi, int *anya);
int CVAnySelPoints (CharView *cv);
void CVSelectPointAt (CharView *cv);
int CVClearSel (CharView *cv);
int CVSetSel (CharView *cv, int mask);
void CVInvertSel (CharView *cv);
int CVAllSelected (CharView *cv);
SplinePointList *CVAnySelPointList (CharView *cv);
int CVAnySelPoint (CharView *cv, SplinePoint **selsp, spiro_cp **selcp);
int CVOneThingSel (CharView *cv, SplinePoint **sp,
                   SplinePointList **spl, RefChar **ref,
                   ImageList **img, AnchorPoint **ap, spiro_cp **cp);
int CVOneContourSel (CharView *cv, SplinePointList **_spl,
                     RefChar **ref, ImageList **img);
void CVInfoDrawText (CharView *cv, GWindow pixmap);
void CVImport (CharView *cv);
void BVImport (BitmapView *bv);
void FVImport (FontView *bv);
void CVFindCenter (CharView *cv, BasePoint *bp, int nosel);
void CVStroke (CharView *cv);
void FVStroke (FontView *fv);
void FreeHandStrokeDlg (StrokeInfo *si);
void OutlineDlg (FontView *fv, CharView *cv, MetricsView *mv, int isinline);
void ShadowDlg (FontView *fv, CharView *cv, MetricsView *mv, int wireframe);
void CVTile (CharView *cv);
void FVTile (FontView *fv);
void CVPatternTile (CharView *cv);
void FVPatternTile (FontView *fv);
void SCCharInfo (SplineChar *sc, int deflayer, EncMap *map, int enc);
void CharInfoDestroy (struct charinfo *ci);
SplineChar *SuffixCheck (SplineChar *sc, char *suffix);
void SCSubtableDefaultSubsCheck (SplineChar *sc,
                                 struct lookup_subtable *sub,
                                 struct matrix_data *possub,
                                 int col_cnt, int r, int layer);
GImage *PST_GetImage (GGadget *pstk, SplineFont *sf, int def_layer,
                      struct lookup_subtable *sub, int popup_r, SplineChar *sc);
GImage *NameList_GetImage (SplineFont *sf, SplineChar *sc,
                           int def_layer, char *namelist, int isliga);
GImage *GV_GetConstructedImage (SplineChar *sc, int def_layer,
                                struct glyphvariants *gv, int is_horiz);
GImage *SC_GetLinedImage (SplineChar *sc, int def_layer, int pos,
                          int is_italic_cor);
struct glyphvariants *GV_ParseConstruction (struct glyphvariants *gv,
                                            struct matrix_data *stuff,
                                            int rows, int cols);
void GV_ToMD (GGadget *g, struct glyphvariants *gv);
void CVGetInfo (CharView *cv);
void CVPGetInfo (CharView *cv);
int SCUsedBySubs (SplineChar *sc);
void SCSubBy (SplineChar *sc);
void SCRefBy (SplineChar *sc);
void ApGetInfo (CharView *cv, AnchorPoint *ap);
void CVMakeClipPath (CharView *cv);
void CVAddAnchor (CharView *cv);
AnchorClass *AnchorClassUnused (SplineChar *sc, int *waslig);
void FVSetWidth (FontView *fv, enum widthtype wtype);
void CVSetWidth (CharView *cv, enum widthtype wtype);
void CVChangeSC (CharView *cv, SplineChar *sc);
Undoes *CVPreserveTState (CharView *cv);
void CVRestoreTOriginalState (CharView *cv);
void CVUndoCleanup (CharView *cv);

void CVAdjustPoint (CharView *cv, SplinePoint *sp);
void CVMergeSplineSets (CharView *cv, SplinePoint *active,
                        SplineSet *activess, SplinePoint *merge,
                        SplineSet *mergess);
void CVAdjustControl (CharView *cv, BasePoint *cp, BasePoint *to);
int CVMoveSelection (CharView *cv, real dx, real dy, uint32_t input_state);
int CVTestSelectFromEvent (CharView *cv, GEvent *event);
void CVMouseMovePen (CharView *cv, PressedOn *p, GEvent *event);
void CVMouseUpPoint (CharView *cv, GEvent *event);
int CVMouseMovePointer (CharView *cv, GEvent *event);
void CVMouseDownPointer (CharView *cv, FindSel *fs, GEvent *event);
void CVMouseDownRuler (CharView *cv, GEvent *event);
void CVMouseMoveRuler (CharView *cv, GEvent *event);
int CVMouseAtSpline (CharView *cv, GEvent *event);
void CVMouseUpRuler (CharView *cv, GEvent *event);
void CVMouseMoveHand (CharView *cv, GEvent *event);
void CVMouseDownFreeHand (CharView *cv, GEvent *event);
void CVMouseMoveFreeHand (CharView *cv, GEvent *event);
void CVMouseUpFreeHand (CharView *cv, GEvent *event);
void CVMouseDownShape (CharView *cv, GEvent *event);
void CPStartInfo (CharView *cv, GEvent *event);
void CPUpdateInfo (CharView *cv, GEvent *event);
void CPEndInfo (CharView *cv);
void BVChar (BitmapView *cv, GEvent *event);
void CVMouseDownPoint (CharView *cv, GEvent *event);
void CVMouseMovePoint (CharView *cv, PressedOn *);
void CVMouseUpPointer (CharView *cv);
void CVCheckResizeCursors (CharView *cv);
void CVMouseDownHand (CharView *cv);
void CVMouseUpHand (CharView *cv);
void CVMouseDownTransform (CharView *cv);
void CVMouseMoveTransform (CharView *cv);
void CVMouseUpTransform (CharView *cv);
void CVMouseDownKnife (CharView *cv);
void CVMouseMoveKnife (CharView *cv, PressedOn *);
void CVMouseUpKnife (CharView *cv, GEvent *event);
void CVMouseMoveShape (CharView *cv);
void CVMouseUpShape (CharView *cv);
void LogoExpose (GWindow pixmap, GEvent *event, GRect *r, enum drawmode dm);
void CVDebugPointPopup (CharView *cv);

int GotoChar (SplineFont *sf, EncMap *map, int *merge_with_selection);

void CVShowPoint (CharView *cv, BasePoint *me);

BitmapView *BitmapViewCreate (BDFChar *bc, BDFFont *bdf, FontView *fv, int enc);
BitmapView *BitmapViewCreatePick (int enc, FontView *fv);
void BitmapViewFree (BitmapView *bv);
void BVMenuRotateInvoked (GWindow gw, struct gmenuitem *mi, GEvent *e);
void BVRotateBitmap (BitmapView *bv, enum bvtools type);
int BVColor (BitmapView *bv);
void BCGeneralFunction (BitmapView *bv,
                        void (*SetPoint) (BitmapView *, int x, int y,
                                          void *data), void *data);
extern char *BVFlipNames[];
void BVChangeBC (BitmapView *bv, BDFChar *bc, int fitit);

void MVSetSCs (MetricsView *mv, SplineChar **scs);
void MVRefreshChar (MetricsView *mv, SplineChar *sc);
void MVRegenChar (MetricsView *mv, SplineChar *sc);
void MVReKern (MetricsView *mv);
MetricsView *MetricsViewCreate (FontView *fv, SplineChar *sc, BDFFont *bdf);
void MetricsViewFree (MetricsView *mv);
void MVRefreshAll (MetricsView *mv);
void MV_FriendlyFeatures (GGadget *g, int pos);
GTextInfo *SLOfFont (SplineFont *sf);

void DoPrefs (void);
void PointerDlg (CharView *cv);
void GListAddStr (GGadget *list, uint32_t *str, void *ud);
void GListReplaceStr (GGadget *list, int index, uint32_t *str, void *ud);

uint32_t *FVOpenFont (char *title, const char *defaultfile, int mult);

void DelayEvent (void (*func) (void *), void *data);

void FindProblems (FontView *fv, CharView *cv, SplineChar *sc);
void CVConstrainSelection (CharView *cv, int type);
void CVMakeParallel (CharView *cv);

void ScriptDlg (FontView *fv, CharView *cv);

SearchView *SVCreate (FontView *fv);
void SVCharViewInits (SearchView *sv);
void SV_DoClose (struct cvcontainer *cvc);
void SVMakeActive (SearchView *sv, CharView *cv);
int SVAttachFV (FontView *fv, int ask_if_difficult);
void SVDetachFV (FontView *fv);

void MKDMakeActive (MathKernDlg *mkd, CharView *cv);
void MKD_DoClose (struct cvcontainer *cvc);
void MKDCharViewInits (MathKernDlg *mkd);
void MathKernDialog (SplineChar *sc, int def_layer);

void ShowAtt (SplineFont *sf, int def_layer);
void FontCompareDlg (FontView *fv);
void SFShowKernPairs (SplineFont *sf, SplineChar *sc, AnchorClass * ac,
                      int layer);
void SFShowLigatures (SplineFont *sf, SplineChar *sc);

void SCEditInstructions (SplineChar *sc);
void SFEditTable (SplineFont *sf, uint32_t tag);
void IIScrollTo (struct instrinfo *ii, int ip, int mark_stop);
void IIReinit (struct instrinfo *ii, int ip);
int ii_v_e_h (GWindow gw, GEvent *event);
void instr_scroll (struct instrinfo *ii, struct sbevent *sb);

void CVGridFitChar (CharView *cv);
void CVFtPpemDlg (CharView *cv, int debug);
void SCDeGridFit (SplineChar *sc);
void SCReGridFit (SplineChar *sc, int layer);

void CVDebugReInit (CharView *cv, int restart_debug, int dbg_fpgm);
void CVDebugFree (DebugView *dv);
int DVChar (DebugView *dv, GEvent *e);

void KernClassD (KernClass *kc, SplineFont *sf, int layer, int isv);
void ShowKernClasses (SplineFont *sf, MetricsView *mv, int layer, int isv);
void KCLD_End (struct kernclasslistdlg *kcld);
void KCLD_MvDetach (struct kernclasslistdlg *kcld, MetricsView *mv);
void KernPairD (SplineFont *sf, SplineChar *sc1, SplineChar *sc2,
                int layer, int isv);
void KCD_DrawGlyph (GWindow pixmap, int x, int baseline, BDFChar *bdfc,
                    int mag);
GTextInfo *BuildFontList (FontView *except);
void TFFree (GTextInfo *tf);

void AnchorControl (SplineChar *sc, AnchorPoint *ap, int layer);
void AnchorControlClass (SplineFont *_sf, AnchorClass * ac, int layer);

void FVSelectByPST (FontView *fv);

enum hist_type
{
  hist_hstem,
  hist_vstem,
  hist_blues
};

struct psdict;

void SFHistogram (SplineFont *sf, int layer, struct psdict *private,
                  uint8_t *selected, EncMap *map, enum hist_type which);

void ContextChainEdit (SplineFont *sf, FPST *fpst,
                       struct gfi_data *gfi, uint32_t *newname, int layer);
char *cu_copybetween (const uint32_t *start, const uint32_t *end);

void MMChangeBlend (MMSet *mm, FontView *fv, int tonew);
void MMWizard (MMSet *mm);

int LayerDialog (Layer *layer, SplineFont *sf);
void CVLayerChange (CharView *cv);

int PointOfViewDlg (struct pov_data *pov, SplineFont *sf, int flags);

SplineChar *FVMakeChar (FontView *fv, int i);

void CVPointOfView (CharView *cv, struct pov_data *);

void DVCreateGloss (DebugView *dv);
void DVMarkPts (DebugView *dv, SplineSet *ss);
int CVXPos (DebugView *dv, int offset, int width);

GMenuItem *GetEncodingMenu (void (*func) (GWindow, GMenuItem *, GEvent *),
                            Encoding *current);

GTextInfo *TIFromName (const char *name);

enum subtable_data_flags
{
  /* I have flags for each alternative because I want "unspecified" to be */
  /*  an option */
  sdf_kernclass = 0x01,
  sdf_kernpair = 0x02,
  sdf_verticalkern = 0x04,
  sdf_horizontalkern = 0x08,
  sdf_dontedit = 0x10
};
struct subtable_data
{
  int flags;
  SplineChar *sc;
};

GTextInfo **SFLookupListFromType (SplineFont *sf, int lookup_type);
GTextInfo *SFLookupArrayFromType (SplineFont *sf, int lookup_type);
GTextInfo *SFLookupArrayFromMask (SplineFont *sf, int lookup_mask);
GTextInfo **SFSubtablesOfType (SplineFont *sf, int lookup_type,
                               int kernclass, int add_none);
GTextInfo *SFSubtableListOfType (SplineFont *sf, int lookup_type,
                                 int kernclass, int add_none);
struct lookup_subtable *SFNewLookupSubtableOfType (SplineFont *sf,
                                                   int lookup_type,
                                                   struct subtable_data
                                                   *sd, int def_layer);
int EditLookup (OTLookup *otl, int isgpos, SplineFont *sf);
int EditSubtable (struct lookup_subtable *sub, int isgpos,
                  SplineFont *sf, struct subtable_data *sd, int def_layer);
void _LookupSubtableContents (SplineFont *sf,
                              struct lookup_subtable *sub,
                              struct subtable_data *sd, int def_layer);
uint32_t **SFGlyphNameCompletion (SplineFont *sf, GGadget *t,
                                  int from_tab, int new_name_after_space);
void AddRmLang (SplineFont *sf, struct lkdata *lk, int add_lang);
void FVMassGlyphRename (FontView *fv);

void SFBdfProperties (SplineFont *sf, EncMap *map, BDFFont *thisone);



extern GMenuItem helplist[];
extern BasePoint last_ruler_offset[];

void CVCopyLayerToLayer (CharView *cv);
void FVCopyLayerToLayer (FontView *fv);
void CVCompareLayerToLayer (CharView *cv);
void FVCompareLayerToLayer (FontView *fv);

void MathInit (void);
void SFMathDlg (SplineFont *sf, int def_layer);

void SFValidationWindow (SplineFont *sf, int layer, enum fontformat format);
void ValidationDestroy (SplineFont *sf);



const char *UI_TTFNameIds (int id);
const char *UI_MSLangString (int language);
void FontInfoInit (void);
void LookupUIInit (void);
enum psstrokeflags Ps_StrokeFlagsDlg (void);
struct cidmap *AskUserForCIDMap (void);

void DefineGroups (struct fontview *fv);
void DisplayGroups (struct fontview *fv);

struct Base *SFBaselines (SplineFont *sf, struct Base *old, int is_vertical);
void JustifyDlg (SplineFont *sf);
char *GlyphListDlg (SplineFont *sf, char *glyphstr);

void DeltaSuggestionDlg (FontView *fv, CharView *cv);
void QGRmFontView (struct qg_data *qg, FontView *fv);
void QGRmCharView (struct qg_data *qg, CharView *cv);


struct hslrgb *SFFontCols (SplineFont *sf, struct hslrgb fontcols[6]);

extern Color view_bgcol;        /* Background color for views */
void MVColInit (void);
void CVColInit (void);

void FVChar (FontView *fv, GEvent *event);
void FVDrawInfo (FontView *fv, GWindow pixmap, GEvent *event);
void KFFontViewInits (struct kf_dlg *kf, GGadget *drawable);
char *GlyphSetFromSelection (SplineFont *sf, int def_layer, char *current);
void ME_ListCheck (GGadget *g, int r, int c, SplineFont *sf);
void ME_SetCheckUnique (GGadget *g, int r, int c, SplineFont *sf);
void ME_ClassCheckUnique (GGadget *g, int r, int c, SplineFont *sf);

#endif /* _VIEWS_H */
