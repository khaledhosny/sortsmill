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
#ifndef _BASEVIEWS_H
#define _BASEVIEWS_H

#include "splinefont.h"
#include <sortsmill/usermenu.h>

enum widthtype
{
  wt_width,
  wt_lbearing,
  wt_rbearing,
  wt_bearings,
  wt_vwidth
};

enum fvtrans_flags
{
  fvt_alllayers = 1,
  fvt_round_to_int = 2,
  fvt_dontsetwidth = 4,
  fvt_dontmovewidth = 8,
  fvt_scalekernclasses = 0x10,
  fvt_scalepstpos = 0x20,
  fvt_dogrid = 0x40,
  fvt_partialreftrans = 0x80,
  fvt_justapply = 0x100,
  fvt_revert = 0x200
};

typedef struct drect
{
  real x, y;
  real width, height;
} DRect;

typedef struct pressedOn
{
  int x, y;                     /* screen location of the press */
  real cx, cy;                  /* Translated into character space */
  SplinePoint *sp;
  bool nextcp;                  /* Is the cursor on the "next" control point of */
  bool prevcp;                  /*  the spline point, or the "prev" control point */
  bool anysel;                  /* did we hit anything? */
/*    bool width;	/ * we're moving the width rather than a spline */
/*    bool vwidth;	/ * we're moving the width rather than a spline */
  bool pressed;
  bool rubberbanding;
  bool rubberlining;
  bool transany;
  bool transanyrefs;
  Spline *spline;
  real t;                       /* location on the spline where we pressed */
  RefChar *ref;
  SplinePointList *spl;         /* containing spline or point */
  ImageList *img;
  AnchorPoint *ap;
  float ex, ey;                 /* end of last rubber band rectangle */
  BasePoint constrain;          /* Point to which we constrain movement */
  BasePoint cp;                 /* Original control point position */
  spiro_cp *spiro;              /* If they clicked on a spiro point */
  int spiro_index;              /* index of a clicked spiro_cp, or */
  /* if they clicked on the spline between spiros, */
  /* this is the spiro indexof the preceding spiro */
} PressedOn;

/* Note: These are ordered as they are displayed in the tools palette */
enum cvtools
{
  cvt_pointer,
  cvt_magnify,
  cvt_freehand,
  cvt_hand,
  cvt_curve,
  cvt_hvcurve,
  cvt_corner,
  cvt_tangent,
  cvt_pen,
  cvt_spiro,
  cvt_knife,
  cvt_ruler,
  cvt_scale,
  cvt_flip,
  cvt_rotate,
  cvt_skew,
  cvt_3d_rotate,
  cvt_perspective,
  cvt_rect,
  cvt_poly,
  cvt_elipse,
  cvt_star,
  cvt_minify,
  cvt_max = cvt_minify,
  cvt_none = -1,
  cvt_spirog4 = cvt_curve,
  cvt_spirog2 = cvt_hvcurve,
  cvt_spirocorner = cvt_corner,
  cvt_spiroleft = cvt_tangent,
  cvt_spiroright = cvt_pen
};

enum bvtools
{
  bvt_pointer,
  bvt_magnify,
  bvt_pencil,
  bvt_line,
  bvt_shift,
  bvt_hand,
  bvt_minify,
  bvt_max = bvt_minify,
  bvt_eyedropper,
  bvt_setwidth,
  bvt_setvwidth,
  bvt_rect,
  bvt_filledrect,
  bvt_elipse,
  bvt_filledelipse,
  bvt_max2 = bvt_filledelipse,
  bvt_none = -1,
  bvt_fliph = 0,
  bvt_flipv,
  bvt_rotate90cw,
  bvt_rotate90ccw,
  bvt_rotate180,
  bvt_skew,
  bvt_transmove
};

enum drawmode
{
  dm_grid,
  dm_back,
  dm_fore,
  dm_max
};

typedef struct bvtfunc
{
  enum bvtools func;
  int x, y;                     /* used by skew and move */
} BVTFunc;

struct freetype_raster
{
  int16_t rows, cols;
  int16_t as, lb;
  int16_t bytes_per_row;
  int16_t num_greys;
  uint8_t *bitmap;
};

struct cvcontainer
{
  struct cvcontainer_funcs *funcs;
};

enum nav_type
{
  nt_prevdef,
  nt_prev,
  nt_goto,
  nt_next,
  nt_nextdef
};

enum cv_container_type
{
  cvc_searcher,
  cvc_mathkern,
  cvc_tilepath,
  cvc_gradient,
  cvc_multiplepattern,
  cvc_stroke
};

struct cvcontainer_funcs
{
  enum cv_container_type type;
  void (*activateMe) (struct cvcontainer * cvc, struct charviewbase * cv);
  void (*charEvent) (struct cvcontainer * cvc, void *event);
  int (*canNavigate) (struct cvcontainer * cvc, enum nav_type type);
  void (*doNavigate) (struct cvcontainer * cvc, enum nav_type type);
  int (*canOpen) (struct cvcontainer * cvc);
  void (*doClose) (struct cvcontainer * cvc);
  SplineFont *(*sf_of_container) (struct cvcontainer * cvc);
};

typedef struct
{
  int tag;                      /* Identifies which kind of view base this is. */
} ViewBase;

typedef struct charviewbase
{
  /* WARNING: If you change the fields of CharViewBase, be sure to
     update the definition of minimalist_CharViewBase. */

  /* FIXME: Do we need a field to tell us whether or not this
     CharViewBase is embedded in a CharView? */
  
  int tag;                      /* == FF_GLYPH_WINDOW. This field must come first and must
                                   be set. */
  struct charviewbase *next;
  struct fontviewbase *fv; /* FIXME: Is this field redundant with
                              sc->parent->fv ? */
  SplineChar *sc;
  Layer *layerheads[dm_max];
  uint8_t drawmode;
  uint16_t ft_gridfitwidth;
  SplineSet *gridfit;
  struct cvcontainer *container;        /* The sv (or whatever) within which this view is embedded (if it is embedded) */
} CharViewBase;

CharViewBase minimalist_CharViewBase (SplineChar *sc);

struct fvcontainer
{
  struct fvcontainer_funcs *funcs;
};

enum fv_container_type
{
  fvc_kernformat,
  fvc_glyphset
};

struct fvcontainer_funcs
{
  enum fv_container_type type;
  int is_modal;                 /* If the fvc is in a modal dialog then we can't create modeless windows (like charviews, fontinfo, etc.) */
  void (*activateMe) (struct fvcontainer * fvc, struct fontviewbase * fv);
  void (*charEvent) (struct fvcontainer * fvc, void *event);
  void (*doClose) (struct fvcontainer * fvc);   /* Cancel the containing dlg? */
  void (*doResize) (struct fvcontainer * fvc, struct fontviewbase * fv,
                    int width, int height);
  /* Resize the container so that fv fits */
};

typedef struct fontviewbase
{
  int tag;                      /* == FF_FONT_WINDOW. This field must come first and must
                                   be set. */
  struct fontviewbase *next;    /* Next on list of open fontviews. */
  struct fontviewbase *nextsame;        /* Next fv looking at this font. */
  EncMap *map;                  /* Current encoding info. */
  EncMap *normal;               /* If this is not NULL then we have a
                                   compacted encoding in map, and this
                                   is the original. */
  SplineFont *sf;               /* Current font. */
  SplineFont *cidmaster;        /* If CID keyed, contains master font. */
  int active_layer;
  BDFFont *active_bitmap;       /* Set if the fontview displays a bitmap strike. */
  uint8_t *selected;            /* Current selection. */
  void *python_fv_object;       /* Used only if Python is supported. */
  struct fvcontainer *container;
} FontViewBase;

enum origins
{
  or_zero,
  or_center,
  or_lastpress,
  or_value,
  or_undefined
};

struct pov_data
{
  enum origins xorigin, yorigin;
  double x, y, z;
  double direction;             /* Direction of gaze projected into xy plane */
  double tilt;                  /* Angle which drawing plane is tilted with respect to projection plane */
  double d;                     /* Distance to projection plane */
  double sintilt;               /* Used internally */
};

enum counter_type
{
  ct_squish,
  ct_retain,
  ct_auto
};

struct lcg_zones
{
  /* info for unhinted processing */
  /* everything abvoe this should be moved down (default xheight/2) */
  int top_zone;
  /* everything below this should be moved up (default xheight/2) */
  /* anything in between should be stationary */
  int bottom_zone;

  /* info for hinted processing */
  /* everything above & at this should be moved down */
  /* also anything on the other side of a hint from this should be moved down */
  int top_bound;
  /* everything below & at this should be moved down */
  /* also anything on the other side of a hint from this should be moved down */
  int bottom_bound;

  enum counter_type counter_type;

  SplineSet *(*embolden_hook) (SplineSet *, struct lcg_zones *, SplineChar *,
                               int layer);
  int wants_hints;
  double serif_height, serif_fuzz;

  double stroke_width;          /* negative number to lighten, positive to embolden */
  int removeoverlap;

  BlueData bd;
  double stdvw;
};

/* This order is the same order as the radio buttons in the embolden dlg */
VISIBLE enum embolden_type
{
  embolden_lcg,
  embolden_cjk,
  embolden_auto,
  embolden_custom,
  embolden_error
};

struct ci_zones
{
  double start, width;
  double moveto, newwidth;      /* Only change width for diagonal stems */
};

struct counterinfo
{
  double c_factor, c_add;       /* For counters */
  double sb_factor, sb_add;     /* For side bearings */
  int correct_italic;

  BlueData bd;
  double stdvw;

  SplineChar *sc;
  int layer;
  DBounds bb;                   /* Value before change */
  double top_y, bottom_y, boundry;
  int has_two_zones;
#define TOP_Z	0
#define BOT_Z	1
  int cnts[2];
  int maxes[2];
  struct ci_zones *zones[2];
};

enum fvformats
{
  fv_bdf,
  fv_ttf,
  fv_pk,
  fv_pcf,
  fv_mac,
  fv_win,
  fv_palm,
  fv_image,
  fv_imgtemplate,
  fv_eps,
  fv_epstemplate,
  fv_pdf,
  fv_pdftemplate,
  fv_plate,
  fv_platetemplate,
  fv_svg,
  fv_svgtemplate,
  fv_fig,
  fv_pythonbase = 0x100
};

VISIBLE enum undotype CopyUndoType (void);
VISIBLE int CopyContainsSomething (void);
VISIBLE int CopyContainsBitmap (void);
int CopyContainsVectors (void);
const Undoes *CopyBufferGet (void);
VISIBLE RefChar *CopyContainsRef (SplineFont *);
char **CopyGetPosSubData (enum possub_type *type,
                          SplineFont **copied_from, int pst_depth);
VISIBLE void CopyReference (SplineChar *sc);
VISIBLE void SCCopyLookupData (SplineChar *sc);
void PasteRemoveSFAnchors (SplineFont *);
void PasteAnchorClassMerge (SplineFont *sf, AnchorClass * into,
                            AnchorClass * from);
void PasteRemoveAnchorClass (SplineFont *sf, AnchorClass * dying);
VISIBLE void ClipboardClear (void);
VISIBLE SplineSet *ClipBoardToSplineSet (void);
VISIBLE void BCCopySelected (BDFChar *bc, int pixelsize, int depth);
VISIBLE void BCCopyReference (BDFChar *bc, int pixelsize, int depth);
VISIBLE void PasteToBC (BDFChar *bc, int pixelsize, int depth);
VISIBLE void FVCopyWidth (FontViewBase *fv, enum undotype ut);
void FVCopyAnchors (FontViewBase *fv);
enum fvcopy_type
{
  ct_fullcopy,
  ct_reference,
  ct_lookups,
  ct_unlinkrefs
};

VISIBLE void FVCopy (FontViewBase *fv, enum fvcopy_type copytype);
VISIBLE void PasteIntoFV (FontViewBase *fv, int pasteinto, real trans[6]);
VISIBLE void FVCopyFgtoBg (FontViewBase *fv);
VISIBLE void FVSameGlyphAs (FontViewBase *fv);
VISIBLE void FVClearBackground (FontViewBase *fv);
VISIBLE void FVClear (FontViewBase *fv);
VISIBLE void FVUnlinkRef (FontViewBase *fv);
VISIBLE void FVUndo (FontViewBase *fv);
VISIBLE void FVRedo (FontViewBase *fv);
VISIBLE void FVJoin (FontViewBase *fv);
VISIBLE void FVBuildDuplicate (FontViewBase *fv);
VISIBLE void FVTrans (FontViewBase *fv, SplineChar *sc,
                      real transform[6], uint8_t *sel, enum fvtrans_flags);
VISIBLE void FVTransFunc (void *_fv, real transform[6], int otype,
                          BVTFunc *bvts, enum fvtrans_flags);
VISIBLE void FVReencode (FontViewBase *fv, Encoding *enc);
VISIBLE void FVOverlap (FontViewBase *fv, enum overlap_type ot);
VISIBLE void FVAddExtrema (FontViewBase *fv, int force_adding);
VISIBLE void FVCorrectDir (FontViewBase *fv);
VISIBLE void FVRound2Int (FontViewBase *fv, real factor);
VISIBLE void FVCanonicalStart (FontViewBase *fv);
VISIBLE void FVCanonicalContours (FontViewBase *fv);
VISIBLE void FVCluster (FontViewBase *fv);
VISIBLE void CIDSetEncMap (FontViewBase *fv, SplineFont *new);
VISIBLE void FVInsertInCID (FontViewBase *fv, SplineFont *sf);

VISIBLE void FVAutoHint (FontViewBase *fv);
VISIBLE void FVAutoHintSubs (FontViewBase *fv);
VISIBLE void FVAutoCounter (FontViewBase *fv);
VISIBLE void FVDontAutoHint (FontViewBase *fv);
VISIBLE void FVAutoInstr (FontViewBase *fv);
VISIBLE void FVClearInstrs (FontViewBase *fv);
VISIBLE void FVClearHints (FontViewBase *fv);
VISIBLE void SCAutoTrace (SplineChar *sc, int layer, int ask);
VISIBLE char *FindAutoTraceName (void);
VISIBLE void *GetAutoTraceArgs (void);
VISIBLE void SetAutoTraceArgs (void *a);
char *FindMFName (void);
VISIBLE char *ProgramExists (char *prog, char *buffer);
VISIBLE void MfArgsInit (void);
VISIBLE void FVAutoTrace (FontViewBase *fv, int ask);
void FVAddEncodingSlot (FontViewBase *fv, int gid);
VISIBLE int FVImportMult (FontViewBase *fv, char *filename, int toback, int bf);
VISIBLE int FVImportBDF (FontViewBase *fv, char *filename, int ispk,
                         int toback);
VISIBLE void MergeFont (FontViewBase *fv, SplineFont *other,
                        int preserveCrossFontKerning);
VISIBLE int FVImportImages (FontViewBase *fv, char *path, int isimage,
                            int toback, int flags);
VISIBLE int FVImportImageTemplate (FontViewBase *fv, char *path,
                                   int isimage, int toback, int flags);
VISIBLE int FVBParseSelectByPST (FontViewBase *fv,
                                 struct lookup_subtable *sub, int search_type);
VISIBLE int SFScaleToEm (SplineFont *sf, int ascent, int descent);
VISIBLE void TransHints (StemInfo * stem, real mul1, real off1,
                         real mul2, real off2, int round_to_int);
VISIBLE void TransDStemHints (DStemInfo * ds, real xmul, real xoff,
                              real ymul, real yoff, int round_to_int);
VISIBLE void VrTrans (ValueRecord *vr, real transform[6]);
int SFNLTrans (FontViewBase *fv, char *x_expr, char *y_expr);
int SSNLTrans (SplineSet *ss, char *x_expr, char *y_expr);
int SCNLTrans (SplineChar *sc, int layer, char *x_expr, char *y_expr);
VISIBLE void FVPointOfView (FontViewBase *fv, struct pov_data *);
VISIBLE void FVStrokeItScript (void *fv, StrokeInfo *si, int pointless);
VISIBLE void FVOutline (struct fontviewbase *fv, real width);
VISIBLE void FVInline (struct fontviewbase *fv, real width, real inset);
VISIBLE void FVShadow (struct fontviewbase *fv, real angle,
                       real outline_width, real shadow_length, int wireframe);
VISIBLE void CI_Init (struct counterinfo *ci, SplineFont *sf);
VISIBLE void FVEmbolden (struct fontviewbase *fv,
                         enum embolden_type type, struct lcg_zones *zones);
VISIBLE void FVCondenseExtend (struct fontviewbase *fv, struct counterinfo *ci);
void ScriptSCCondenseExtend (SplineChar *sc, struct counterinfo *ci);

struct smallcaps
{
  double lc_stem_width, uc_stem_width;
  double stem_factor, v_stem_factor;
  double xheight, scheight, capheight;
  double vscale, hscale;
  char *extension_for_letters, *extension_for_symbols;
  int dosymbols;
  SplineFont *sf;
  int layer;
  double italic_angle, tan_ia;
};

VISIBLE void SmallCapsFindConstants (struct smallcaps *small,
                                     SplineFont *sf, int layer);

enum glyphchange_type
{
  gc_generic,
  gc_smallcaps,
  gc_subsuper,
  gc_max
};

struct position_maps
{
  double current, desired;
  double cur_width, des_width;
  int overlap_index;
};

struct fixed_maps
{
  int cnt;
  struct position_maps *maps;
};

struct genericchange
{
  enum glyphchange_type gc;
  uint32_t feature_tag;
  char *glyph_extension;
  char *extension_for_letters, *extension_for_symbols;
  double stem_height_scale, stem_width_scale;
  double stem_height_add, stem_width_add;
  double stem_threshold;
  double serif_height_scale, serif_width_scale;
  double serif_height_add, serif_width_add;
  double hcounter_scale, hcounter_add;
  double lsb_scale, lsb_add;
  double rsb_scale, rsb_add;
  uint8_t center_in_hor_advance;
  uint8_t use_vert_mapping;
  uint8_t do_smallcap_symbols;
  uint8_t petite;               /* generate petite caps rather than smallcaps */
  double vcounter_scale, vcounter_add;  /* If not using mapping */
  double v_scale;               /* If using mapping */
  struct fixed_maps m;
  struct fixed_maps g;          /* Adjusted for each glyph */
  double vertical_offset;
  unsigned int dstem_control, serif_control;
  struct smallcaps *small;
/* Filled in by called routine */
  SplineFont *sf;
  int layer;
  double italic_angle, tan_ia;
};

VISIBLE void FVAddSmallCaps (FontViewBase *fv, struct genericchange *genchange);
VISIBLE void FVGenericChange (FontViewBase *fv,
                              struct genericchange *genchange);
VISIBLE void CVGenericChange (CharViewBase *cv,
                              struct genericchange *genchange);

struct xheightinfo
{
  double xheight_current, xheight_desired;
  double serif_height;
};

VISIBLE void InitXHeightInfo (SplineFont *sf, int layer,
                              struct xheightinfo *xi);
VISIBLE void ChangeXHeight (FontViewBase *fv, CharViewBase *cv,
                            struct xheightinfo *xi);
SplineSet *SSControlStems (SplineSet *ss, double stemwidthscale,
                           double stemheightscale, double hscale,
                           double vscale, double xheight);
VISIBLE void MakeItalic (FontViewBase *fv, CharViewBase *cv, ItalicInfo * ii);
int FVReplaceAll (FontViewBase *fv, SplineSet *find, SplineSet *rpl,
                  double fudge, int flags);
VISIBLE void FVBReplaceOutlineWithReference (FontViewBase *fv, double fudge);
VISIBLE void FVCorrectReferences (FontViewBase *fv);
VISIBLE void _FVSimplify (FontViewBase *fv, struct simplifyinfo *smpl);
VISIBLE void UnlinkThisReference (FontViewBase *fv, SplineChar *sc, int layer);
VISIBLE FontViewBase *ViewFont (char *filename, int openflags);
VISIBLE void FVBuildAccent (FontViewBase *fv, int onlyaccents);
VISIBLE void FVRemoveKerns (FontViewBase *fv);
VISIBLE void FVRemoveVKerns (FontViewBase *fv);
VISIBLE void FVVKernFromHKern (FontViewBase *fv);
VISIBLE void FVAddUnencoded (FontViewBase *fv, int cnt);
VISIBLE void FVRemoveUnused (FontViewBase *fv);
VISIBLE void FVCompact (FontViewBase *fv);
VISIBLE void FVDetachGlyphs (FontViewBase *fv);
VISIBLE void FVDetachAndRemoveGlyphs (FontViewBase *fv);
int AutoWidthScript (FontViewBase *fv, int spacing);
int AutoKernScript (FontViewBase *fv, int spacing, int threshold,
                    struct lookup_subtable *sub, char *kernfile);

void BCTrans (BDFFont *bdf, BDFChar *bc, BVTFunc *bvts, FontViewBase *fv);
VISIBLE void BCSetPoint (BDFChar *bc, int x, int y, int color);
VISIBLE void BCTransFunc (BDFChar *bc, enum bvtools type, int xoff, int yoff);
VISIBLE void skewselect (BVTFunc *bvtf, real t);

VISIBLE BDFFloat *BDFFloatCreate (BDFChar *bc, int xmin, int xmax,
                                  int ymin, int ymax, int clear);
BDFFloat *BDFFloatCopy (BDFFloat * sel);
BDFFloat *BDFFloatConvert (BDFFloat * sel, int newdepth, int olddepth);
VISIBLE void BDFFloatFree (BDFFloat * sel);

VISIBLE void BCMergeReferences (BDFChar *base, BDFChar *cur,
                                int8_t xoff, int8_t yoff);
VISIBLE BDFChar *BDFGetMergedChar (BDFChar *bc);
VISIBLE void BCUnlinkThisReference (struct fontviewbase *fv, BDFChar *bc);

VISIBLE int CVLayer (CharViewBase *cv);
VISIBLE Undoes *CVPreserveStateHints (CharViewBase *cv);
VISIBLE Undoes *CVPreserveState (CharViewBase *cv);
VISIBLE Undoes *_CVPreserveTState (CharViewBase *cv, PressedOn *);
Undoes *CVPreserveWidth (CharViewBase *cv, int width);
Undoes *CVPreserveVWidth (CharViewBase *cv, int vwidth);
VISIBLE void CVDoRedo (CharViewBase *cv);
VISIBLE void CVDoUndo (CharViewBase *cv);
VISIBLE void _CVRestoreTOriginalState (CharViewBase *cv, PressedOn *p);
VISIBLE void _CVUndoCleanup (CharViewBase *cv, PressedOn *p);
VISIBLE void CVRemoveTopUndo (CharViewBase *cv);
VISIBLE void CopySelected (CharViewBase *cv, int doanchors);
VISIBLE void CVCopyGridFit (CharViewBase *cv);
VISIBLE void CopyWidth (CharViewBase *cv, enum undotype);
VISIBLE void PasteToCV (CharViewBase *cv);
VISIBLE void CVYPerspective (CharViewBase *cv, bigreal x_vanish,
                             bigreal y_vanish);
void ScriptSCEmbolden (SplineChar *sc, int layer,
                       enum embolden_type type, struct lcg_zones *zones);
VISIBLE void CVEmbolden (CharViewBase *cv, enum embolden_type type,
                         struct lcg_zones *zones);
VISIBLE void SCCondenseExtend (struct counterinfo *ci, SplineChar *sc,
                               int layer, int do_undoes);
void SCClearSelPt (SplineChar *sc);
void SC_MoreLayers (SplineChar *, Layer *old);
void SCLayersChange (SplineChar *sc);
void SFLayerChange (SplineFont *sf);
void SCTile (SplineChar *sc, int layer);
VISIBLE void _CVMenuMakeLine (CharViewBase *cv, int do_arc,
                              int ellipse_to_back);
    /* Ellipse to back is a debugging flag and adds the generated ellipse to */
    /*  the background layer so we can look at it. I thought it might actually */
    /*  be useful, so I left it in. Activated with the Alt key in the menu */

VISIBLE void MVCopyChar (FontViewBase *fv, BDFFont *bdf,
                         SplineChar *sc, enum fvcopy_type fullcopy);
VISIBLE void PasteIntoMV (FontViewBase *fv, BDFFont *bdf,
                          SplineChar *sc, int doclear);

enum search_flags
{
  sv_reverse = 0x1,
  sv_flips = 0x2,
  sv_rotate = 0x4,
  sv_scale = 0x8,
  sv_endpoints = 0x10
};

enum flipset
{
  flip_none = 0,
  flip_x,
  flip_y,
  flip_xy
};

typedef struct searchdata
{
  SplineChar sc_srch, sc_rpl;
  SplineSet *path, *revpath, *replacepath, *revreplace;
  int pointcnt, rpointcnt;
  real fudge;
  real fudge_percent;           /* a value of .05 here represents 5% (we don't store the integer) */
  bool tryreverse;
  bool tryflips;
  bool tryrotate;
  bool tryscale;
  bool endpoints;               /* Don't match endpoints, use them for direction only */
  bool onlyselected;
  bool subpatternsearch;
  bool doreplace;
  bool replaceall;
  bool findall;
  bool searchback;
  bool wrap;
  bool wasreversed;
  bool replacewithref;
  bool already_complained;      /* User has already been alerted to the fact that we've converted splines to refs and lost the instructions */
  SplineSet *matched_spl;
  SplinePoint *matched_sp, *last_sp;
  real matched_rot, matched_scale;
  real matched_x, matched_y;
  double matched_co, matched_si;        /* Precomputed sin, cos */
  enum flipset matched_flip;
#ifdef HAVE_LONG_LONG_INT
  unsigned long long matched_refs;      /* Bit map of which refs in the char were matched */
  unsigned long long matched_ss;        /* Bit map of which splines in the char were matched */
  /* In multi-path mode */
  unsigned long long matched_ss_start;  /* Bit map of which splines we tried to start matches with */
#else
  unsigned long matched_refs;
  unsigned long matched_ss;
  unsigned long matched_ss_start;
#endif
  FontViewBase *fv;
  SplineChar *curchar;
  int last_gid;
} SearchData;

struct searchdata *SDFromContour (FontViewBase *fv, SplineSet *find,
                                  double fudge, int flags);
SplineChar *SDFindNext (struct searchdata *sv);

struct python_import_export
{
  struct _object *import;       /* None becomes NULL */
  struct _object *export;       /* None becomes NULL */
  struct _object *data;         /* None stays None */
  char *name;
  char *extension;
  char *all_extensions;
};

extern struct python_import_export *py_ie;

VISIBLE void PyFF_SCExport (SplineChar *sc, int ie_index,
                            char *filename, int layer);
VISIBLE void PyFF_SCImport (SplineChar *sc, int ie_index,
                            char *filename, int layer, int clear);
VISIBLE void PyFF_InitFontHook (FontViewBase *fv);

VISIBLE void LookupInit (void);

enum byte_types
{
  bt_instr,
  bt_cnt,
  bt_byte,
  bt_wordhi,
  bt_wordlo,
  bt_impliedreturn
};

struct instrdata
{
  uint8_t *instrs;
  int instr_cnt, max;
  uint8_t *bts;
  bool changed;
  bool in_composit;
  SplineFont *sf;
  SplineChar *sc;
  uint32_t tag;
  struct instrdlg *id;
  struct instrdata *next;
};

VISIBLE uint8_t *_IVParse (SplineFont *sf, char *text, int *len,
                           void (*IVError) (void *, char *, int), void *iv);
VISIBLE char *_IVUnParseInstrs (uint8_t *instrs, int instr_cnt);

int BitmapControl (FontViewBase *fv, int32_t *sizes, int isavail,
                   int rasterize);
void FVSetWidthScript (FontViewBase *fv, enum widthtype wtype,
                       int val, int incr);
VISIBLE void FVMetricsCenter (FontViewBase *fv, int docenter);
VISIBLE void FVRevert (FontViewBase *fv);
VISIBLE void FVRevertBackup (FontViewBase *fv);
VISIBLE void FVRevertGlyph (FontViewBase *fv);
VISIBLE int MMReblend (FontViewBase *fv, MMSet *mm);
VISIBLE FontViewBase *MMCreateBlendedFont (MMSet *mm,
                                           FontViewBase *fv,
                                           real blends[MmMax], int tonew);
VISIBLE void FVB_MakeNamelist (FontViewBase *fv, FILE *file);

VISIBLE void AutoWidth2 (FontViewBase *fv, int separation,
                         int min_side, int max_side, int chunk_height,
                         int loop_cnt);
VISIBLE void GuessOpticalOffset (SplineChar *sc, int layer,
                                 real *_loff, real *_roff, int chunk_height);
VISIBLE void AutoKern2 (SplineFont *sf, int layer, SplineChar **left,
                        SplineChar **right,
                        struct lookup_subtable *into, int separation,
                        int min_kern, int from_closest_approach,
                        int only_closer, int chunk_height,
                        void (*addkp) (void *data, SplineChar *left,
                                       SplineChar *r, int off), void *data);
VISIBLE void AutoKern2NewClass (SplineFont *sf, int layer,
                                char **leftnames, char **rightnames,
                                int lcnt, int rcnt,
                                void (*kcAddOffset) (void *data,
                                                     int left_index,
                                                     int right_index,
                                                     int offset),
                                void *data, int separation,
                                int min_kern,
                                int from_closest_approach,
                                int only_closer, int chunk_height);
VISIBLE void AutoKern2BuildClasses (SplineFont *sf, int layer,
                                    SplineChar **leftglyphs,
                                    SplineChar **rightglyphs,
                                    struct lookup_subtable *sub,
                                    int separation, int min_kern,
                                    int touching, int only_closer,
                                    int autokern, real good_enough);

#endif
