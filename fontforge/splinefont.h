// Copyright (C) 2013, 2014 Khaled Hosny and Barry Schwartz
//
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
#ifndef _SPLINEFONT_H
#define _SPLINEFONT_H

#include <config.h>

#include <basics.h>
#include <real_types.h>
#include <iconv.h>
#include <sortsmill/nearness.h>
#include <sortsmill/guile/core.h>
#include <assert.h>

#define CHR(ch1,ch2,ch3,ch4) (((ch1)<<24)|((ch2)<<16)|((ch3)<<8)|(ch4))

#define MmMax		16      /* PS says at most this many instances for type1/2 mm fonts */

typedef struct ipoint
{
  int x;
  int y;
} IPoint;

#define IPOINT_EMPTY { 0, 0 }


typedef struct basepoint
{
  real x;
  real y;
} BasePoint;

#define BASEPOINT_EMPTY { (real)0.0, (real)0.0 }


typedef struct dbasepoint
{
  bigreal x;
  bigreal y;
} DBasePoint;

#define DBASEPOINT_EMPTY { (bigreal)0.0, (bigreal)0.0 }


typedef struct tpoint
{
  real x;
  real y;
  real t;
} TPoint;

#define TPOINT_EMPTY { (real)0.0, (real)0.0, (real)0.0 }


typedef struct dbounds
{
  real minx, maxx;
  real miny, maxy;
} DBounds;

#define DBOUNDS_EMPTY { (real)0.0, (real)0.0, (real)0.0, (real)0.0 }


typedef struct ibounds
{
  int minx;
  int maxx;
  int miny;
  int maxy;
} IBounds;

#define IBOUNDS_EMPTY { 0, 0, 0, 0 }


enum val_type
{
  v_int,
  v_real,
  v_str,
  v_unicode,
  v_lval,
  v_arr,
  v_arrfree,
  v_int32pt,
  v_int16pt,
  v_int8pt,
  v_void
};

typedef struct val
{
  enum val_type type;
  union
  {
    int ival;
    real fval;
    char *sval;
    struct val *lval;
    struct array *aval;
    uint32_t *u32ptval;
    uint16_t *u16ptval;
    uint8_t *u8ptval;
  } u;
} Val;                          /* Used by scripting */

struct psdict
{
  int cnt;
  int next;
  char **keys;
  char **values;
};

struct pschars
{
  int cnt;
  int next;
  char **keys;
  uint8_t **values;
  int *lens;
  int bias;                     /* for type2 strings */
};

enum linejoin
{
  lj_miter,                     /* Extend lines until they meet */
  lj_round,                     /* circle centered at the join of expand radius */
  lj_bevel,                     /* Straight line between the ends of next and prev */
  lj_inherited
};

enum linecap
{
  lc_butt,                      /* equiv to lj_bevel, straight line extends from one side to other */
  lc_round,                     /* semi-circle */
  lc_square,                    /* Extend lines by radius, then join them */
  lc_inherited
};

enum spreadMethod
{
  sm_pad,
  sm_reflect,
  sm_repeat
};

#define COLOR_INHERITED	0xfffffffe

struct grad_stops
{
  real offset;
  uint32_t col;
  real opacity;
};

struct gradient
{
  BasePoint start;              /* focal of a radial gradient, start of a linear */
  BasePoint stop;               /* center of a radial gradient, end of a linear */
  real radius;                  /* 0=>linear gradient, else radius of a radial gradient */
  enum spreadMethod sm;
  int stop_cnt;
  struct grad_stops *grad_stops;
};

struct pattern
{
  char *pattern;
  real width, height;           /* Pattern is scaled to be repeated every width/height (in user coordinates) */
  real transform[6];
  /* Used during rasterization process */
  struct bdfchar *pat;
  real invtrans[6];
  int bminx, bminy, bwidth, bheight;    /* of the pattern at bdfchar scale */
};

struct brush
{
  uint32_t col;
  float opacity;                /* number between [0,1], only for svg/pdf */
  struct pattern *pattern;      /* A pattern to be tiled */
  struct gradient *gradient;    /* A gradient fill */
};

#define WIDTH_INHERITED	(-1)
#define DASH_INHERITED	255     /* if the dashes[0]==0 && dashes[1]==DASH_INHERITED */
#define DASH_MAX	8

typedef unsigned char DashType;

struct pen
{
  struct brush brush;
  uint8_t linejoin;
  uint8_t linecap;
  float width;
  real trans[4];
  DashType dashes[DASH_MAX];
};

struct spline;

enum si_type
{
  si_std,
  si_caligraphic,
  si_poly,
  si_centerline
};

/* If you change this structure you may need to update MakeStrokeDlg */
/*  and cvpalettes.c both contain statically initialized StrokeInfos */
typedef struct strokeinfo
{
  real radius;                  /* or major axis of pen */
  enum linejoin join;
  enum linecap cap;
  enum si_type stroke_type;
  bool removeinternal;
  bool removeexternal;
  bool leave_users_center;      /* Don't move the pen so its center is at the origin */
  real penangle;
  real minorradius;
  struct splinepointlist *poly;
  real resolution;
/* For freehand tool */
  real radius2;
  int pressure1, pressure2;
/* End freehand tool */
  void *data;
  bigreal (*factor) (void *data, struct spline * spline, real t);
} StrokeInfo;

enum PolyType
{
  Poly_Convex,
  Poly_Concave,
  Poly_PointOnEdge,
  Poly_TooFewPoints,
  Poly_Line
};


enum overlap_type
{
  over_remove,
  over_rmselected,
  over_intersect,
  over_intersel,
  over_exclude,
  over_findinter,
  over_fisel
};

enum simpify_flags
{
  sf_cleanup = -1,
  sf_normal = 0,
  sf_ignoreslopes = 1,
  sf_ignoreextremum = 2,
  sf_smoothcurves = 4,
  sf_choosehv = 8,
  sf_forcelines = 0x10,
  sf_nearlyhvlines = 0x20,
  sf_mergelines = 0x40,
  sf_setstart2extremum = 0x80,
  sf_rmsingletonpoints = 0x100
};

struct simplifyinfo
{
  int flags;
  bigreal err;
  bigreal tan_bounds;
  bigreal linefixup;
  bigreal linelenmax;           /* Don't simplify any straight lines longer than this */
  int set_as_default;
  int check_selected_contours;
};

struct hsquash
{
  double lsb_percent;
  double stem_percent;
  double counter_percent;
  double rsb_percent;
};

/*-----------------------------------------------------------------------*/

/* |    | (flat)    |   | (simple)     |    | (complex) */
/* |    |           |  /               |   /            */
/* |    |           | /                |  /             */
/* +----+           |/                 \ /              */

enum serif_type
{
  srf_flat,
  srf_simpleslant,
  srf_complexslant
};

/*-----------------------------------------------------------------------*/

typedef struct italicinfo
{
  double italic_angle;
  double xheight_percent;
  struct hsquash lc, uc, neither;
  enum serif_type secondary_serif;

  bool transform_bottom_serifs;
  bool transform_top_xh_serifs; /* Those at x-height */
  bool transform_top_as_serifs; /* Those at ascender-height */
  bool transform_diagon_serifs; /* Those at baseline/xheight */

  bool a_from_d;                /* replace the "a" glyph with the variant which looks like a "d" without an ascender */
  /* When I say "f" I also mean "f_f" ligature, "longs", cyrillic phi and other things shaped like "f" */
  bool f_long_tail;             /* Some Italic fonts have the "f" grow an extension of the main stem below the baseline */
  bool f_rotate_top;            /* Most Italic fonts take the top curve of the "f", rotate it 180 and attach to the bottom */
  bool pq_deserif;              /* Remove a serif from the descender of p or q and replace with a secondary serif as above */

  /* Unsupported */
  /* e becomes rounder, cross bar slightly slanted */
  /* g closed counter at bottom */
  /* k closed counter at top */
  /* v-z diagonal stems become more curvatious */

  bool cyrl_phi;                /* Gains an "f" like top, bottom treated like "f" */
  bool cyrl_i;                  /* Turns into a latin u */
  bool cyrl_pi;                 /* Turns into a latin n */
  bool cyrl_te;                 /* Turns into a latin m */
  bool cyrl_sha;                /* Turns into a latin m rotated 180 */
  bool cyrl_dje;                /* Turns into a latin smallcaps T */
  bool cyrl_dzhe;               /* Turns into a latin u */
  /* Is there a difference between dzhe and i? both look like u to me */

  /* Unsupported */
  /* u432 curved B */
  /* u433 strange gamma */
  /* u434 normal delta */
  /* u436 */
  /* u43b lambda ? */
  /* u43c */
  /* u446 */
  /* u449 */
  /* u449 */
  /* u44a */

/* This half of the structure gets filled in later - see ITALICINFO_REMAINDER */
  double tan_ia;
  double x_height;
  double pq_depth;
  double ascender_height;
  double emsize;
  int order2;
  struct splinefont *sf;
  int layer;
  double serif_extent, serif_height;
  struct splinepoint *f_start, *f_end;  /* start has next pointing into the f head and up */
  struct splinepoint *ff_start1, *ff_end1, *ff_start2, *ff_end2;
  double f_height, ff_height;
} ItalicInfo;

#define ITALICINFO_REMAINDER 0, 0, 0, 0, 0, 0, NULL, 0, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, 0, 0


typedef struct bluedata
{
  real xheight, xheighttop;     /* height of "x" and "o" (u,v,w,x,y,z) */
  real caph, caphtop;           /* height of "I" and "O" */
  real base, basebelow;         /* bottom of "I" and "O" */
  real ascent;                  /* height of "l" */
  real descent;                 /* depth of "p" */
  real numh, numhtop;           /* height of "7" and "8" *//* numbers with ascenders */
  int bluecnt;                  /* If the private dica contains bluevalues... */
  real blues[12][2];            /* 7 pairs from bluevalues, 5 from otherblues */
} BlueData;

#define BLUEDATA_EMPTY { \
    0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0, \
    { { 0.0, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0 }, \
      { 0.0, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0 }, { 0.0, 0.0 }  \
    } \
}


typedef struct bdffloat
{
  int16_t xmin, xmax, ymin, ymax;
  int16_t bytes_per_line;
  bool byte_data;
  uint8_t depth;
  uint8_t *bitmap;
} BDFFloat;

/* OpenType does not document 'dflt' as a language, but we'll use it anyway. */
/* (Adobe uses it too) we'll turn it into a default entry when we output it. */
#define DEFAULT_LANG		CHR('d','f','l','t')

/* The OpenType spec says in one place that the default script is 'dflt' and */
/*  in another that it is 'DFLT'. 'DFLT' is correct */
#define DEFAULT_SCRIPT		CHR('D','F','L','T')

#define REQUIRED_FEATURE	CHR(' ','R','Q','D')

typedef enum otlookup_type
{
  ot_undef = 0,                 // Not really a lookup type.

  // otlookup >> 8 == 0 if GSUB, 1 if GPOS.
  //
  // otlookup & 0xff == lookup type for the appropriate table.

  gsub_start = 0x000,           // Not really a lookup type.
  gsub_single = 0x001,
  gsub_multiple = 0x002,
  gsub_alternate = 0x003,
  gsub_ligature = 0x004,
  gsub_context = 0x005,
  gsub_contextchain = 0x006,
  gsub_extension = 0x007,
  gsub_reversecchain = 0x008,

  gpos_start = 0x100,           // Not really a lookup type.
  gpos_single = 0x101,
  gpos_pair = 0x102,
  gpos_cursive = 0x103,
  gpos_mark2base = 0x104,
  gpos_mark2ligature = 0x105,
  gpos_mark2mark = 0x106,
  gpos_context = 0x107,
  gpos_contextchain = 0x108,
  gpos_extension = 0x109
} OTLookupType;

enum otlookup_typemasks
{
  gsub_single_mask = 0x00001,
  gsub_multiple_mask = 0x00002,
  gsub_alternate_mask = 0x00004,
  gsub_ligature_mask = 0x00008,
  gsub_context_mask = 0x00010,
  gsub_contextchain_mask = 0x00020,
  gsub_reversecchain_mask = 0x00040,
  /* ********************* */
  gpos_single_mask = 0x00400,
  gpos_pair_mask = 0x00800,
  gpos_cursive_mask = 0x01000,
  gpos_mark2base_mask = 0x02000,
  gpos_mark2ligature_mask = 0x04000,
  gpos_mark2mark_mask = 0x08000,
  gpos_context_mask = 0x10000,
  gpos_contextchain_mask = 0x20000,
};

#define MAX_LANG 		4       /* If more than this we allocate more_langs in chunks of MAX_LANG */

struct scriptlanglist
{
  uint32_t script;
  uint32_t langs[MAX_LANG];
  uint32_t *morelangs;
  int lang_cnt;
  struct scriptlanglist *next;
};

extern struct opentype_feature_friendlynames
{
  uint32_t tag;
  char *tagstr;
  char *friendlyname;
  int masks;
} friendlies[];

#define OPENTYPE_FEATURE_FRIENDLYNAMES_EMPTY { 0, NULL, NULL, 0 }


typedef struct featurescriptlanglist
{
  uint32_t featuretag;
  struct scriptlanglist *scripts;
  struct featurescriptlanglist *next;
} FeatureScriptLangList;

enum pst_flags
{
  pst_r2l = 1,
  pst_ignorebaseglyphs = 2,
  pst_ignoreligatures = 4,
  pst_ignorecombiningmarks = 8,
  pst_usemarkfilteringset = 0x10,
  pst_markclass = 0xff00,
  pst_markset = 0xffff0000
};

typedef struct lookup_subtable
{
  char *subtable_name;
  char *suffix;                 /* for gsub_single, used to find a default replacement */
  int16_t separation, minkern;  /* for gpos_pair, used to guess default kerning values */
  struct otlookup *lookup;
  bool unused;
  bool per_glyph_pst_or_kern;
  bool anchor_classes;
  bool vertical_kerning;
  bool ticked;
  bool kerning_by_touch;        /* for gpos_pair, calculate kerning so that glyphs will touch */
  bool onlyCloser;              /* for kerning classes */
  bool dontautokern;            /* for kerning classes */
  struct kernclass *kc;
  struct generic_fpst *fpst;
  /* Each time an item is added to a lookup we must place it into a */
  /*  subtable. If it's a kerning class, fpst or state machine it has */
  /*  a subtable all to itself. If it's an anchor class it can share */
  /*  a subtable with other anchor classes (merge with). If it's a glyph */
  /*  PST it may share a subtable with other PSTs */
  /* Note items may only be placed in lookups in which they fit. Can't */
  /*  put kerning data in a gpos_single lookup, etc. */
  struct lookup_subtable *next;
  int32_t subtable_offset;
  int32_t *extra_subtables;
  /* If a kerning subtable has too much stuff in it, we are prepared to */
  /*  break it up into several smaller subtables, each of which has */
  /*  an offset in this list (extra-subtables[0]==subtable_offset) */
  /*  the list is terminated by an entry of -1 */
} LookupSubtable;

typedef struct otlookup
{
  struct otlookup *next;
  enum otlookup_type lookup_type;
  uint32_t lookup_flags;        /* Low order: traditional flags, High order: markset index, only meaningful if pst_usemarkfilteringset set */
  char *lookup_name;
  FeatureScriptLangList *features;
  struct lookup_subtable *subtables;
  bool unused;                  /* No subtable is used (call SFFindUnusedLookups before examining) */
  bool empty;                   /* No subtable is used, and no anchor classes are used */
  bool store_in_afm;            /* Used for ligatures, some get stored */
  /*  'liga' generally does, but 'frac' doesn't */
  bool needs_extension;         /* Used during opentype generation */
  bool temporary_kern;          /* Used when decomposing kerning classes into kern pairs for older formats */
  bool def_lang_checked;
  bool def_lang_found;
  bool ticked;
  bool in_gpos;
  bool in_jstf;
  bool only_jstf;
  int16_t subcnt;               /* Actual number of subtables we will output */
  /* Some of our subtables may contain no data */
  /* Some may be too big and need to be broken up. */
  /* So this field may be different than just counting the subtables */
  int lookup_index;             /* used during opentype generation */
  uint32_t lookup_offset;
  uint32_t lookup_length;
  char *tempname;
} OTLookup;

#define LOOKUP_SUBTABLE_EMPTY { NULL, NULL, 0, 0, NULL, 0, 0, 0, 0, 0, 0, 0, 0, NULL, NULL, NULL, 0, NULL }
#define OTLOOKUP_EMPTY { NULL, 0, 0, NULL, NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NULL }


typedef struct devicetab
{
  /* A range of point sizes to which this table applies. */
  uint16_t first_pixel_size;
  uint16_t last_pixel_size;

  /* A set of pixel corrections, one for each point size. */
  int8_t *corrections;
} DeviceTable;

typedef struct valdev
{
  /* Value records can have four associated device tables. */
  DeviceTable xadjust;
  DeviceTable yadjust;
  DeviceTable xadv;
  DeviceTable yadv;
} ValDevTab;

typedef struct anchorclass
{
  char *name;                   /* in utf8 */
  struct lookup_subtable *subtable;
  bool has_base;
  bool processed;
  bool has_mark;
  bool matches;
  uint8_t ac_num;
  bool ticked;
  struct anchorclass *next;
} AnchorClass;

static inline OTLookupType
AnchorClass_lookup_type (const AnchorClass *ac)
{
  return ac->subtable->lookup->lookup_type;
}

typedef enum anchor_type
{
  at_mark = 0,
  at_basechar = 1,
  at_baselig = 2,
  at_basemark = 3,
  at_centry = 4,
  at_cexit = 5,
  at_max = 6,
  at_illegal = 15
} AnchorPointType;

typedef struct anchorpoint
{
  AnchorClass *anchor;
  BasePoint me;
  DeviceTable xadjust;
  DeviceTable yadjust;
  uint8_t type;
  bool selected;
  bool ticked;
  bool has_ttf_pt;
  uint16_t ttf_pt_index;
  int16_t lig_index;
  struct anchorpoint *next;
} AnchorPoint;

typedef struct kernpair
{
  struct lookup_subtable *subtable;
  struct splinechar *sc;
  int16_t off;
  uint16_t kcid;                /* temporary value */
  DeviceTable *adjust;          /* Only adjustment in one dimen, if more needed use pst */
  struct kernpair *next;
} KernPair;

typedef struct kernclass
{
  int first_cnt, second_cnt;    /* Count of classes for first and second chars */
  char **firsts;                /* list of a space separated list of char names */
  char **seconds;               /*  one entry for each class. Entry 0 is null */
  /*  and means everything not specified elsewhere */
  struct lookup_subtable *subtable;
  uint16_t kcid;                /* Temporary value, used for many things briefly */
  int16_t *offsets;             /* array of first_cnt*second_cnt entries */
  DeviceTable *adjusts;         /* array of first_cnt*second_cnt entries */
  struct kernclass *next;
} KernClass;

enum possub_type
{
  pst_null,
  pst_position,
  pst_pair,
  pst_substitution,
  pst_alternate,
  pst_multiple,
  pst_ligature,
  pst_lcaret /* must be pst_max-1, see charinfo.c */ ,
  pst_max,

  /* These are not psts but are related so it's handly to have values for them */
  pst_kerning = pst_max,
  pst_vkerning,
  pst_anchors,

  /* And these are fpsts */
  pst_contextpos,
  pst_contextsub,
  pst_chainpos,
  pst_chainsub,
  pst_reversesub,
  fpst_max,

  /* And these are used to specify a kerning pair where the current */
  /*  char is the final glyph rather than the initial one */
  /* A kludge used when cutting and pasting features */
  pst_kernback,
  pst_vkernback
};

struct vr
{
  int16_t xoff, yoff, h_adv_off, v_adv_off;
  ValDevTab *adjust;
};

typedef struct generic_pst
{
  bool ticked;
  bool temporary;               /* Used in afm ligature closure */
  /* enum possub_type */ uint8_t type;
  struct lookup_subtable *subtable;
  struct generic_pst *next;
  union
  {
    struct vr pos;
    struct
    {
      char *paired;
      struct vr *vr;
    } pair;
    struct
    {
      char *variant;
    } subs;
    struct
    {
      char *components;
    } mult, alt;
    struct
    {
      char *components;
      struct splinechar *lig;
    } lig;
    struct
    {
      int16_t *carets;
      int cnt;
    } lcaret;                   /* Ligature caret positions */
  } u;
} PST;

typedef struct liglist
{
  PST *lig;
  struct splinechar *first;     /* First component */
  struct splinecharlist *components;    /* Other than the first */
  struct liglist *next;
  int ccnt;                     /* Component count. (includes first component) */
} LigList;

enum fpossub_format
{
  pst_glyphs,
  pst_class,
  pst_coverage,
  pst_reversecoverage,
  pst_formatmax
};

struct seqlookup
{
  int seq;
  struct otlookup *lookup;
};

struct fpg
{
  char *names, *back, *fore;
};

struct fpc
{
  int ncnt, bcnt, fcnt;
  uint16_t *nclasses, *bclasses, *fclasses, *allclasses;
};

struct fpv
{
  int ncnt, bcnt, fcnt;
  char **ncovers, **bcovers, **fcovers;
};

struct fpr
{
  int always1, bcnt, fcnt;
  char **ncovers, **bcovers, **fcovers;
  char *replacements;
};

struct fpst_rule
{
  union
  {
    /* Note: Items in backtrack area are in reverse order because that's how the OT wants them */
    /*  they need to be reversed again to be displayed to the user */
    struct fpg glyph;
    struct fpc class;
    struct fpv coverage;
    struct fpr rcoverage;
  } u;
  int lookup_cnt;
  struct seqlookup *lookups;
};

typedef struct generic_fpst
{
  uint16_t /*enum possub_type */ type;
  uint16_t /*enum fpossub_format */ format;
  struct lookup_subtable *subtable;
  struct generic_fpst *next;
  uint16_t nccnt, bccnt, fccnt;
  uint16_t rule_cnt;
  char **nclass, **bclass, **fclass;
  struct fpst_rule *rules;
  uint8_t ticked;
  uint8_t effectively_by_glyphs;
  char **nclassnames, **bclassnames, **fclassnames;
} FPST;

enum asm_type
{
  asm_indic,
  asm_context,
  asm_lig,
  asm_simple = 4,
  asm_insert,
  asm_kern = 0x11
};

enum asm_flags
{
  asm_vert = 0x8000,
  asm_descending = 0x4000,
  asm_always = 0x2000
};

struct asm_state
{
  uint16_t next_state;
  uint16_t flags;
  union
  {
    struct
    {
      struct otlookup *mark_lookup;     /* for contextual glyph subs (tag of a nested lookup) */
      struct otlookup *cur_lookup;      /* for contextual glyph subs */
    } context;
    struct
    {
      char *mark_ins;
      char *cur_ins;
    } insert;
    struct
    {
      int16_t *kerns;
      int kcnt;
    } kern;
  } u;
};

struct jstf_prio
{
  OTLookup **enableShrink;      /* Points to an array of lookups (GSUB or GPOS) */
  OTLookup **disableShrink;     /* NULL terminated */
  OTLookup **maxShrink;         /* Array of GPOS like lookups */
  OTLookup **enableExtend;
  OTLookup **disableExtend;
  OTLookup **maxExtend;
};

struct jstf_lang
{
  uint32_t lang;
  struct jstf_lang *next;
  int cnt;
  struct jstf_prio *prios;
};

typedef struct jstf_script
{
  uint32_t script;
  struct jstf_script *next;
  char *extenders;              /* list of glyph names */
  struct jstf_lang *langs;
} Justify;

struct opentype_str
{
  struct splinechar *sc;
  struct vr vr;                 /* Scaled and rounded gpos modifications (device table info included in xoff, etc. not in adjusts) */
  struct kernpair *kp;
  struct kernclass *kc;
  bool prev_kc0;
  bool next_kc0;
  int16_t advance_width;        /* Basic advance, modifications in vr, scaled and rounded */
  /* Er... not actually set by ApplyLookups, but somewhere the caller */
  /*  can stash info. (Extract width from hinted bdf if possible, tt */
  /*  instructions can change it from the expected value) */
  int16_t kc_index;
  int16_t lig_pos;              /* when skipping marks to form a ligature keep track of what ligature element a mark was attached to */
  int16_t context_pos;          /* When doing a contextual match remember which glyphs are used, and where in the match they occur. Skipped glyphs have -1 */
  int32_t orig_index;
  void *fl;
  bool line_break_after;
  bool r2l;
  int16_t bsln_off;
};

/* Wow, the GPOS 'size' feature stores a string in the name table just as mac */
/*  features do */
/* And now (OTF 1.6) GSUB 'ss01'-'ss20' do too */
struct otfname
{
  struct otfname *next;
  uint16_t lang;                /* windows language code */
  char *name;                   /* utf8 */
};

struct otffeatname
{
  uint32_t tag;                 /* Feature tag */
  struct otfname *names;
  struct otffeatname *next;
  uint16_t nid;                 /* temporary value */
};

typedef struct refbdfc
{
  bool checked;
  bool selected;
  int8_t xoff;
  int8_t yoff;
  uint16_t gid;
  struct refbdfc *next;
  struct bdfchar *bdfc;
} BDFRefChar;

struct bdfcharlist
{
  struct bdfchar *bc;
  struct bdfcharlist *next;
};

typedef struct bdfchar
{
  struct splinechar *sc;
  int16_t xmin, xmax, ymin, ymax;
  int16_t width;
  int16_t bytes_per_line;
  uint8_t *bitmap;
  struct refbdfc *refs;
  int orig_pos;
  int16_t pixelsize;            /* for undoes */
  struct bitmapview *views;
  struct undoes *undoes;
  struct undoes *redoes;
  bool changed;
  bool byte_data;               /* for anti-aliased chars entries are grey-scale bytes not bw bits */
  bool widthgroup;              /* for ttf bitmap output */
  bool isreference;             /* for ttf bitmap input, */
  bool ticked;
  uint8_t depth;                /* for ttf bitmap output */
  uint16_t vwidth;
  BDFFloat *selection;
  BDFFloat *backup;
  struct bdfcharlist *dependents;
} BDFChar;

enum undotype
{
  ut_none = 0,
  ut_state,
  ut_tstate,
  ut_statehint,
  ut_statename,
  ut_statelookup,
  ut_anchors,
  ut_width,
  ut_vwidth,
  ut_lbearing,
  ut_rbearing,
  ut_possub,
  ut_hints,
  ut_bitmap,
  ut_bitmapsel,
  ut_composit,
  ut_multiple,
  ut_layers,
  ut_noop
};

typedef struct undoes
{
  struct undoes *next;
  enum undotype undotype;
  bool was_modified;
  bool was_order2;
  union
  {
    struct
    {
      int16_t width, vwidth;
      int16_t lbearingchange;
      int unicodeenc;           /* only for ut_statename */
      char *charname;           /* only for ut_statename */
      char *comment;            /* in utf8 */
      PST *possub;              /* only for ut_statename */
      struct splinepointlist *splines;
      struct refchar *refs;

      struct imagelist *images;
      void *hints;              /* ut_statehint, ut_statename */
      uint8_t *instrs;
      int instrs_len;
      AnchorPoint *anchor;
      struct brush fill_brush;
      struct pen stroke_pen;
      bool dofill;
      bool dostroke;
      bool fillfirst;
    } state;
    int width;                  /* used by both ut_width and ut_vwidth */
    int lbearing;               /* used by ut_lbearing */
    int rbearing;               /* used by ut_rbearing */
    BDFChar bmpstate;
    struct
    {                           /* copy contains an outline state and a set of bitmap states */
      struct undoes *state;
      struct undoes *bitmaps;
    } composit;
    struct
    {
      struct undoes *mult;      /* copy contains several sub copies (composits, or states or widths or...) */
      /* Also used for ut_layers, each sub copy is a state (first is ly_fore, next ly_fore+1...) */
    } multiple;
    struct
    {
      enum possub_type pst;
      char **data;              /* First 4 bytes is tag, then space then data */
      struct undoes *more_pst;
      short cnt, max;           /* Not always set */
    } possub;
    uint8_t *bitmap;
  } u;
  struct splinefont *copied_from;
} Undoes;

typedef struct enc
{
  char *enc_name;
  int char_cnt;                 /* Size of the next two arrays */
  int32_t *unicode;             /* unicode value for each encoding point */
  char **psnames;               /* optional postscript name for each encoding point */
  struct enc *next;
  bool builtin;
  bool hidden;
  bool only_1byte;
  bool has_1byte;
  bool has_2byte;
  bool is_unicodebmp;
  bool is_unicodefull;
  bool is_custom;
  bool is_original;
  bool is_compact;
  bool is_japanese;
  bool is_korean;
  bool is_tradchinese;
  bool is_simplechinese;
  char iso_2022_escape[8];
  int iso_2022_escape_len;
  int low_page, high_page;
  char *iconv_name;             /* For compatibility to old versions we might use a different name from that used by iconv. */
  iconv_t *tounicode;
  iconv_t *fromunicode;
  int (*tounicode_func) (int);
  int (*fromunicode_func) (int);
  bool is_temporary;            /* freed when the map gets freed */
  int char_max;                 /* Used by temporary encodings */
} Encoding;

struct renames
{
  char *from;
  char *to;
};

typedef struct namelist
{
  struct namelist *basedon;
  char *title;
  const char ***unicode[17];
  struct namelist *next;
  struct renames *renames;
  int uses_unicode;
  char *a_utf8_name;
} NameList;

enum uni_interp
{
  ui_unset = -1,
  ui_none,
  ui_adobe,
  ui_greek,
  ui_japanese,
  ui_trad_chinese,
  ui_simp_chinese,
  ui_korean,
  ui_ams
};

struct remap
{
  uint32_t firstenc, lastenc;
  int32_t infont;
};

typedef struct encmap
{
  /* A per-font map of encoding to glyph id. */

  SCM _enc_to_gid;              /* A map from encoding point to glyph ID. */
  SCM _gid_to_enc;              /* A map from glyph ID to encoding point. */

  int enc_limit;                /* One more than the highest encoding point.
                                   Strictly speaking, this might include
                                   glyphs that are not encoded, but which
                                   are displayed after the encoding
                                   proper. */

  /* FIXME: We also need a SplineChar-to-GID mapping (which would NOT
     be part of EncMap), or replace the GIDs with SplineChars
     themselves. */

  struct remap *remap;
  Encoding *enc;
  bool ticked;
} EncMap;

typedef struct
{
  SCM _enc_to_gid;
  scm_t_rbmapssz_iter _iter;
} enc_iter_t;

typedef struct
{
  SCM _gid_to_enc;
  scm_t_rbmapssz_iter _iter;
} gid_iter_t;

inline void make_enc_to_gid (EncMap *map);
inline void release_enc_to_gid (EncMap *map);
inline void clear_enc_to_gid (EncMap *map);
inline void set_enc_to_gid (EncMap *map, ssize_t enc, ssize_t gid);
inline void remove_enc_to_gid (EncMap *map, ssize_t enc);
inline ssize_t enc_to_gid (EncMap *map, ssize_t enc);
inline bool enc_to_gid_is_set (EncMap *map, ssize_t enc);
void copy_enc_to_gid_contents (EncMap *new, EncMap *old);

inline enc_iter_t enc_iter (EncMap *map);
inline enc_iter_t enc_iter_last (EncMap *map);
inline bool enc_done (enc_iter_t iter);
inline enc_iter_t enc_next (enc_iter_t iter);
inline enc_iter_t enc_prev (enc_iter_t iter);
inline ssize_t enc_enc (enc_iter_t iter);
inline ssize_t enc_gid (enc_iter_t iter);

inline void make_gid_to_enc (EncMap *map);
inline void release_gid_to_enc (EncMap *map);
inline void clear_gid_to_enc (EncMap *map);
void build_gid_to_enc (EncMap *map);
void rebuild_gid_to_enc (EncMap *map);
inline void set_gid_to_enc (EncMap *map, ssize_t gid, ssize_t enc);
void add_gid_to_enc (EncMap *map, ssize_t gid, ssize_t enc);
void remove_gid_to_enc (EncMap *map, ssize_t gid, ssize_t enc);
inline void remove_all_gid_to_enc (EncMap *map, ssize_t gid);
inline ssize_t gid_to_enc (EncMap *map, ssize_t gid);   /* Returns the
                                                           first code
                                                           point. */
inline SCM gid_to_enc_list (EncMap *map, ssize_t gid);
inline bool gid_to_enc_is_set (EncMap *map, ssize_t gid);
void copy_gid_to_enc_contents (EncMap *new, EncMap *old);

inline gid_iter_t gid_iter (EncMap *map);
inline gid_iter_t gid_iter_last (EncMap *map);
inline bool gid_done (gid_iter_t iter);
inline gid_iter_t gid_next (gid_iter_t iter);
inline gid_iter_t gid_prev (gid_iter_t iter);
inline ssize_t gid_gid (gid_iter_t iter);
inline ssize_t gid_enc (gid_iter_t iter);
inline SCM gid_enc_list (gid_iter_t iter);

inline void
make_enc_to_gid (EncMap *map)
{
  // FIXME: Eliminate the need to protect these objects, or at least
  // keep track of the protected objects for release when we leave the
  // GUI, etc. Likely there are minor memory leaks.
  map->_enc_to_gid = scm_gc_protect_object (scm_make_rbmapssz ());
}

inline void
release_enc_to_gid (EncMap *map)
{
  scm_gc_unprotect_object (map->_enc_to_gid);
}

inline void
clear_enc_to_gid (EncMap *map)
{
  release_enc_to_gid (map);
  make_enc_to_gid (map);
}

inline void
set_enc_to_gid (EncMap *map, ssize_t enc, ssize_t gid)
{
  SCM key = scm_from_ssize_t (enc);
  if (gid == -1)
    scm_rbmapssz_delete_x (map->_enc_to_gid, key);
  else
    scm_rbmapssz_set_x (map->_enc_to_gid, key, scm_from_ssize_t (gid));
}

inline void
remove_enc_to_gid (EncMap *map, ssize_t enc)
{
  scm_rbmapssz_delete_x (map->_enc_to_gid, scm_from_ssize_t (enc));
}

inline ssize_t
enc_to_gid (EncMap *map, ssize_t enc)
{
  return
    scm_to_ssize_t (scm_rbmapssz_ref
                    (map->_enc_to_gid, scm_from_ssize_t (enc),
                     scm_from_int (-1)));
}

inline bool
enc_to_gid_is_set (EncMap *map, ssize_t enc)
{
  return enc_to_gid (map, enc) != -1;
}

inline enc_iter_t
enc_iter (EncMap *map)
{
  enc_iter_t iter = {
    ._enc_to_gid = map->_enc_to_gid,
    ._iter = scm_c_rbmapssz_first (map->_enc_to_gid)
  };
  return iter;
}

inline enc_iter_t
enc_iter_last (EncMap *map)
{
  enc_iter_t iter = {
    ._enc_to_gid = map->_enc_to_gid,
    ._iter = scm_c_rbmapssz_last (map->_enc_to_gid)
  };
  return iter;
}

inline bool
enc_done (enc_iter_t iter)
{
  return iter._iter == NULL;
}

inline enc_iter_t
enc_next (enc_iter_t iter)
{
  iter._iter = scm_c_rbmapssz_next (iter._enc_to_gid, iter._iter);
  return iter;
}

inline enc_iter_t
enc_prev (enc_iter_t iter)
{
  iter._iter = scm_c_rbmapssz_prev (iter._enc_to_gid, iter._iter);
  return iter;
}

inline ssize_t
enc_enc (enc_iter_t iter)
{
  return (ssize_t) scm_rbmapssz_iter_key (iter._iter);
}

inline ssize_t
enc_gid (enc_iter_t iter)
{
  return scm_to_ssize_t (scm_rbmapssz_iter_value (iter._iter));
}

inline void
make_gid_to_enc (EncMap *map)
{
  // FIXME: Eliminate the need to protect these objects, or at least
  // keep track of the protected objects for release when we leave the
  // GUI, etc. Likely there are minor memory leaks.
  map->_gid_to_enc = scm_gc_protect_object (scm_make_rbmapssz ());
}

inline void
release_gid_to_enc (EncMap *map)
{
  scm_gc_unprotect_object (map->_gid_to_enc);
}

inline void
clear_gid_to_enc (EncMap *map)
{
  release_gid_to_enc (map);
  make_gid_to_enc (map);
}

inline void
set_gid_to_enc (EncMap *map, ssize_t gid, ssize_t enc)
{
  SCM key = scm_from_ssize_t (gid);
  if (enc == -1)
    scm_rbmapssz_delete_x (map->_gid_to_enc, key);
  else
    scm_rbmapssz_set_x (map->_gid_to_enc, key, scm_from_ssize_t (enc));
}

inline void
remove_all_gid_to_enc (EncMap *map, ssize_t gid)
{
  scm_rbmapssz_delete_x (map->_gid_to_enc, scm_from_ssize_t (gid));
}

inline ssize_t
gid_to_enc (EncMap *map, ssize_t gid)
{
  SCM value = scm_rbmapssz_ref (map->_gid_to_enc, scm_from_ssize_t (gid),
                                scm_from_int (-1));
  return scm_to_ssize_t ((scm_is_pair (value)) ? SCM_CAR (value) : value);
}

inline SCM
gid_to_enc_list (EncMap *map, ssize_t gid)
{
  SCM value = scm_rbmapssz_ref (map->_gid_to_enc, scm_from_ssize_t (gid),
                                scm_from_int (-1));
  SCM lst;
  if (scm_is_pair (value))
    lst = value;
  else if (scm_to_ssize_t (value) != -1)
    lst = scm_list_1 (value);
  else
    lst = SCM_EOL;
  return lst;
}

inline bool
gid_to_enc_is_set (EncMap *map, ssize_t gid)
{
  SCM value = scm_rbmapssz_ref (map->_gid_to_enc, scm_from_ssize_t (gid),
                                SCM_BOOL_F);
  return scm_is_true (value);
}

inline gid_iter_t
gid_iter (EncMap *map)
{
  gid_iter_t iter = {
    ._gid_to_enc = map->_gid_to_enc,
    ._iter = scm_c_rbmapssz_first (map->_gid_to_enc)
  };
  return iter;
}

inline gid_iter_t
gid_iter_last (EncMap *map)
{
  gid_iter_t iter = {
    ._gid_to_enc = map->_gid_to_enc,
    ._iter = scm_c_rbmapssz_last (map->_gid_to_enc)
  };
  return iter;
}

inline bool
gid_done (gid_iter_t iter)
{
  return iter._iter == NULL;
}

inline gid_iter_t
gid_next (gid_iter_t iter)
{
  iter._iter = scm_c_rbmapssz_next (iter._gid_to_enc, iter._iter);
  return iter;
}

inline gid_iter_t
gid_prev (gid_iter_t iter)
{
  iter._iter = scm_c_rbmapssz_prev (iter._gid_to_enc, iter._iter);
  return iter;
}

inline ssize_t
gid_gid (gid_iter_t iter)
{
  return (ssize_t) scm_rbmapssz_iter_key (iter._iter);
}

inline ssize_t
gid_enc (gid_iter_t iter)
{
  return scm_to_ssize_t (scm_rbmapssz_iter_value (iter._iter));
}

inline SCM
gid_enc_list (gid_iter_t iter)
{
  SCM value = scm_rbmapssz_iter_value (iter._iter);
  SCM lst;
  if (scm_is_pair (value))
    lst = value;
  else if (scm_to_ssize_t (value) != -1)
    lst = scm_list_1 (value);
  else
    lst = SCM_EOL;
  return lst;
}

enum property_type
{
  prt_string,
  prt_atom,
  prt_int,
  prt_uint,
  prt_property = 0x10
};

typedef struct bdfprops
{
  char *name;                   /* These include both properties (like SLANT) and non-properties (like FONT) */
  int type;
  union
  {
    char *str;
    char *atom;
    int val;
  } u;
} BDFProperties;

typedef struct bdffont
{
  struct splinefont *sf;
  int glyphcnt, glyphmax;       /* used & allocated sizes of glyphs array */
  BDFChar **glyphs;             /* an array of charcnt entries */
  int16_t pixelsize;
  int16_t ascent, descent;
  int16_t layer;                /* for piecemeal fonts */
  bool piecemeal;
  bool bbsized;
  bool ticked;
  bool unhinted_freetype;
  bool recontext_freetype;
  struct bdffont *next;
  struct clut *clut;
  char *foundry;
  int res;
  void *freetype_context;
  uint16_t truesize;            /* for bbsized fonts */
  int16_t prop_cnt;
  int16_t prop_max;             /* only used within bdfinfo dlg */
  BDFProperties *props;
  uint16_t ptsize, dpi;         /* for piecemeal fonts */
} BDFFont;

#define HntMax	96              /* PS says at most 96 hints */
typedef uint8_t HintMask[HntMax / 8];

enum pointtype
{
  pt_curve,
  pt_corner,
  pt_tangent,
  pt_hvcurve
};

typedef struct splinepoint
{
  BasePoint me;
  BasePoint nextcp;             /* control point */
  BasePoint prevcp;             /* control point */
  bool nonextcp;
  bool noprevcp;
  bool nextcpdef;
  bool prevcpdef;
  bool selected;                /* for UI */
  uint8_t pointtype;
  bool isintersection;
  bool flexy;                   /* When "freetype_markup" is on in charview.c:DrawPoint */
  bool flexx;                   /* flexy means select nextcp, and flexx means draw circle around nextcp */
  bool roundx;                  /* For true type hinting */
  bool roundy;                  /* For true type hinting */
  bool dontinterpolate;         /* in ttf, don't imply point by interpolating between cps */
  bool ticked;
  bool watched;
  uint16_t ptindex;             /* Temporary value used by metafont routine */
  uint16_t ttfindex;            /* Truetype point index */
  /* Special values 0xffff => point implied by averaging control points */
  /*                0xfffe => point created with no real number yet */
  /* (or perhaps point in context where no number is possible as in a glyph with points & refs) */
  uint16_t nextcpindex;         /* Truetype point index */
  struct spline *next;
  struct spline *prev;
  HintMask *hintmask;
} SplinePoint;

enum linelist_flags
{
  cvli_onscreen = 0x1,
  cvli_clipped = 0x2
};

typedef struct linelist
{
  IPoint here;
  struct linelist *next;
  /* The first two fields are constant for the linelist, the next ones */
  /*  refer to a particular screen. If some portion of the line from */
  /*  this point to the next one is on the screen then set cvli_onscreen */
  /*  if this point needs to be clipped then set cvli_clipped */
  /*  asend and asstart are the actual screen locations where this point */
  /*  intersects the clip edge. */
  enum linelist_flags flags;
  IPoint asend, asstart;
} LineList;

typedef struct linearapprox
{
  real scale;
  bool oneline;
  bool onepoint;
  bool any;                     /* refers to a particular screen */
  struct linelist *lines;
  struct linearapprox *next;
} LinearApprox;

/* FIXME: This type, I believe, is used to store polynomial splines in
   the monomial basis, {1, t, t^2, t^3}. Where you see it, there lies
   trouble, and the algorithms (not merely code) there probably should
   be replaced. */
typedef struct spline1d
{
  real a;
  real b;
  real c;
  real d;
} Spline1D;

typedef struct spline
{
  bool islinear;                /* No control points */
  bool isquadratic;             /* probably read in from ttf */
  bool isticked;
  bool isneeded;                /* Used in remove overlap */
  bool isunneeded;              /* Used in remove overlap */
  bool exclude;                 /* Used in remove overlap varient: exclude */
  bool ishorvert;
  bool knowncurved;             /* We know that it curves */
  bool knownlinear;             /* it might have control points, but still traces out a line */
  /* If neither knownlinear nor curved then we haven't checked */
  bool order2;                  /* It's a bezier curve with only one cp */
  bool touched;
  bool leftedge;
  bool rightedge;
  bool acceptableextrema;       /* This spline has extrema, but we don't care */
  SplinePoint *from, *to;
  Spline1D splines[2];          /* splines[0] is the x spline, splines[1] is y */
  struct linearapprox *approx;
  /* Possible optimizations:
     Precalculate bounding box.
     Precalculate min/max points of inflection.
   */
} Spline;

#include "spiroentrypoints.h"

#define SPIRO_SELECTED(cp)	((cp)->ty&0x80)
#define SPIRO_DESELECT(cp)	((cp)->ty&=~0x80)
#define SPIRO_SELECT(cp)	((cp)->ty|=0x80)
#define SPIRO_SPL_OPEN(spl)	((spl)->spiro_cnt>1 && ((spl)->spiros[0].ty&0x7f)==SPIRO_OPEN_CONTOUR)

#define SPIRO_NEXT_CONSTRAINT	SPIRO_RIGHT     /* The curve is on the next side of the constraint point */
#define SPIRO_PREV_CONSTRAINT	SPIRO_LEFT      /* The curve is on the prev side of the constraint point */

typedef struct splinepointlist
{
  SplinePoint *first, *last;
  struct splinepointlist *next;
  spiro_cp *spiros;
  uint16_t spiro_cnt, spiro_max;
  uint8_t ticked;
  uint8_t beziers_need_optimizer;       /* If the spiros have changed in spiro mode, then reverting to bezier mode might, someday, run a simplifier */
  uint8_t is_clip_path;         /* In type3/svg fonts */
  char *contour_name;
} SplinePointList, SplineSet;

typedef struct imagelist
{
  struct gimage *image;
  real xoff, yoff;              /* position in character space of upper left corner of image */
  real xscale, yscale;          /* scale to convert one pixel of image to one unit of character space */
  DBounds bb;
  struct imagelist *next;
  bool selected;
} ImageList;

struct reflayer
{
  bool background;
  bool order2;
  bool anyflexes;
  bool dofill;
  bool dostroke;
  bool fillfirst;
  struct brush fill_brush;
  struct pen stroke_pen;
  SplinePointList *splines;
  ImageList *images;            /* Only in background or type3 layer(s) */
};

typedef struct refchar
{
  bool checked;
  bool selected;
  bool point_match;             /* match_pt* are point indexes */
  /*  and need to be converted to a */
  /*  translation after truetype readin */
  bool encoded;                 /* orig_pos is actually an encoded value, used for old sfd files */
  bool justtranslated;          /* The transformation matrix specifies a translation (or is identity) */
  bool use_my_metrics;          /* Retain the ttf "use_my_metrics" info. */
  /* important for glyphs with instructions which change the width used */
  /* inside composites */
  bool round_translation_to_grid;       /* Retain the ttf "round_to_grid" info. */
  bool point_match_out_of_date; /* Someone has edited a base glyph */
  int16_t adobe_enc;
  int orig_pos;
  int unicode_enc;              /* used by paste */
  real transform[6];            /* transformation matrix (first 2 rows of a 3x3 matrix, missing row is 0,0,1) */
  struct reflayer *layers;
  int layer_cnt;
  struct refchar *next;
  DBounds bb;
  struct splinechar *sc;
  BasePoint top;
  uint16_t match_pt_base, match_pt_ref;
} RefChar;

/* Some stems may appear, disappear, reapear several times */
/* Serif stems on I which appear at 0, disappear, reappear at top */
/* Or the major vertical stems on H which disappear at the cross bar */
typedef struct hintinstance
{
  real begin;                   /* location in the non-major direction */
  real end;                     /* width/height in non-major direction */
  bool closed;
  short int counternumber;
  struct hintinstance *next;
} HintInstance;

enum hinttypes
{
  ht_unspecified = 0,
  ht_h,
  ht_v,
  ht_d
};

typedef real _MMArray[2][MmMax];

typedef struct steminfo
{
  struct steminfo *next;
  unsigned int hinttype:2;      /* Only used by undoes */
  bool ghost;                   /* this is a ghost stem hint. As such truetype should ignore it, type2 output should negate it, and type1 should use as is */
  /* stored width will be either 20 or 21 */
  /* Type2 says: -20 is "width" of top edge, -21 is "width" of bottom edge, type1 accepts either */
  bool haspointleft;
  bool haspointright;
  bool hasconflicts;            /* Does this stem have conflicts within its cluster? */
  bool used;                    /* Temporary for counter hints or hint substitution */
  bool tobeused;                /* Temporary for counter hints or hint substitution */
  bool active;                  /* Currently active hint in Review Hints dlg */
  /*  displayed differently in char display */
  bool enddone;                 /* Used by ttf instructing, indicates a prev */
  /*  hint had the same end as this one (so */
  /*  the points on the end line have been */
  /*  instructed already */
  bool startdone;               /* Used by ttf instructing */
  /*bool backwards; *//* If we think this hint is better done with a negative width */
  bool reordered;               /* In AutoHinting. Means we changed the start of the hint, need to test for out of order */
  bool pendingpt;               /* A pending stem creation, not a true stem */
  bool linearedges;             /* If we have a nice rectangle then we aren't */
  /*  interested in the orientation which is */
  /*  wider than long */
  int16_t hintnumber;           /* when dumping out hintmasks we need to know */
  /*  what bit to set for this hint */
  union
  {
    int mask;                   /* Mask of all references that use this hint */
    /*  in type2 output */
    _MMArray *unblended /*[2][MmMax] */ ;       /* Used when reading in type1 mm hints */
  } u;
  real start;                   /* location at which the stem starts */
  real width;                   /* or height */
  HintInstance *where;          /* location(s) in the other coord */
} StemInfo;

typedef struct dsteminfo
{
  struct dsteminfo *next;       /* First two fields match those in steminfo */
  unsigned int hinttype:2;      /* Only used by undoes */
  bool used;                    /* used only by tottf.c:gendinstrs, metafont.c to mark a hint that has been dealt with */
  BasePoint left, right, unit;
  HintInstance *where;          /* location(s) along the unit vector */
} DStemInfo;

typedef struct layer            /* : reflayer */
{
  bool background;
  bool order2;
  bool anyflexes;
  bool dofill;
  bool dostroke;
  bool fillfirst;
  struct brush fill_brush;
  struct pen stroke_pen;
  SplinePointList *splines;
  ImageList *images;            /* Only in background or type3 layer(s) */
  RefChar *refs;                /* Only in foreground layer(s) */
  Undoes *undoes;
  Undoes *redoes;
  uint32_t validation_state;
  uint32_t old_vs;
} Layer;

enum layer_type
{
  ly_all = -2,
  ly_grid = -1,
  ly_back = 0,
  ly_fore = 1,
  /* Possibly other foreground layers for type3 things */
  /* Possibly other background layers for normal fonts */
  ly_none = -3
};

struct gv_part
{
  char *component;
  bool is_extender;             /* This component may be skipped or repeated */
  uint16_t startConnectorLength;
  uint16_t endConnectorLength;
  uint16_t fullAdvance;
};

/* For the 'MATH' table (and for TeX) */
struct glyphvariants
{
  char *variants;               /* Space separated list of glyph names */
/* Glyph assembly */
  int16_t italic_correction;    /* Of the composed glyph */
  DeviceTable *italic_adjusts;
  int part_cnt;
  struct gv_part *parts;
};

struct mathkerndata
{
  int16_t height, kern;
  DeviceTable *height_adjusts;
  DeviceTable *kern_adjusts;
};

/* For the 'MATH' table */
struct mathkernvertex
{
  int cnt;                      /* There is one more kern entry than height entry */
  /* So the last mkd should have its height ignored */
  /* The MATH table stores the height count, I think the kern count */
  /*  is more useful (and that's what I use here). They differ by 1 */
  struct mathkerndata *mkd;
};

struct mathkern
{
  struct mathkernvertex top_right;
  struct mathkernvertex top_left;
  struct mathkernvertex bottom_right;
  struct mathkernvertex bottom_left;
};

enum privatedict_state
{
  pds_odd = 0x1,                /* Odd number of entries */
  pds_outoforder = 0x2,         /* Bluevalues should be listed in order */
  pds_toomany = 0x4,            /* arrays are of limited sizes */
  pds_tooclose = 0x8,           /* adjacent zones must not be within 2*bluefuzz+1 (or 3, if bluefuzz omitted) */
  pds_notintegral = 0x10,       /* Must be integers */
  pds_toobig = 0x20,            /* within pair difference have some relation to BlueScale but the docs make no sense to me */
  pds_shift = 8,                /* BlueValues/OtherBlues, unshifted, FamilyBlues/FamilyOtherBlues shifted once */

  pds_missingblue = 0x010000,
  pds_badbluefuzz = 0x020000,
  pds_badbluescale = 0x040000,
  pds_badstdhw = 0x080000,
  pds_badstdvw = 0x100000,
  pds_badstemsnaph = 0x200000,
  pds_badstemsnapv = 0x400000,
  pds_stemsnapnostdh = 0x0800000,
  pds_stemsnapnostdv = 0x1000000,
  pds_badblueshift = 0x2000000
};

enum validation_state
{
  vs_unknown = 0,
  vs_known = 0x01,              /* It has been validated */
  vs_opencontour = 0x02,
  vs_selfintersects = 0x04,
  vs_wrongdirection = 0x08,
  vs_flippedreferences = 0x10,  /* special case of wrong direction */
  vs_missingextrema = 0x20,
  vs_missingglyphnameingsub = 0x40,
  /* Next few are postscript only */
  vs_toomanypoints = 0x80,
  vs_toomanyhints = 0x100,
  vs_badglyphname = 0x200,
  /* Next few are only for fontlint */
  /* These are relative to maxp values which ff would fix on generating a font */
  vs_maxp_toomanypoints = 0x400,
  vs_maxp_toomanypaths = 0x800,
  vs_maxp_toomanycomppoints = 0x1000,
  vs_maxp_toomanycomppaths = 0x2000,
  vs_maxp_instrtoolong = 0x4000,
  vs_maxp_toomanyrefs = 0x8000,
  vs_maxp_refstoodeep = 0x10000,
  /* vs_maxp_prepfpgmtoolong=0x20000, *//* I think I was wrong about this "error" */
  /* Oops, we need another one, two, for the glyphs */
  vs_pointstoofarapart = 0x40000,
  vs_nonintegral = 0x80000,     /* This will never be interesting in a real font, but might be in an sfd file */
  vs_missinganchor = 0x100000,
  vs_dupname = 0x200000,
  vs_dupunicode = 0x400000,
  vs_overlappedhints = 0x800000,

  vs_last = vs_overlappedhints,
  vs_maskps =
    0x3fe | vs_pointstoofarapart | vs_missinganchor | vs_dupname |
    vs_dupunicode | vs_overlappedhints,
  vs_maskcid =
    0x1fe | vs_pointstoofarapart | vs_missinganchor | vs_dupname |
    vs_overlappedhints,
  vs_maskttf =
    0x7e | vs_pointstoofarapart | vs_nonintegral | vs_missinganchor |
    vs_dupunicode,
  vs_maskfindproblems =
    0x1be | vs_pointstoofarapart | vs_nonintegral | vs_missinganchor |
    vs_overlappedhints
};

struct splinecharlist
{
  struct splinechar *sc;
  struct splinecharlist *next;
};

struct altuni
{
  struct altuni *next;
  int unienc;

  /* vs is the "variation selector" a unicode codepoint which modifieds */
  /*  the code point before it. If vs is -1 then unienc is just an */
  /*  alternate encoding (greek Alpha and latin A), but if vs is one */
  /*  of unicode's variation selectors then this glyph is somehow a */
  /*  variant shape. The specifics depend on the selector and script */
  /*  fid is currently unused, but may, someday, be used to do ttcs */
  /* NOTE: GlyphInfo displays vs==-1 as vs==0, and fixes things up */
  int vs;

  int fid;
};

typedef struct splinechar
{
  char *name;
  int unicodeenc;
  int orig_pos;                 /* Original position in the glyph list. */
  int16_t width;
  int16_t vwidth;
  int16_t lsidebearing;         /* Only used when reading in a Type1
                                   font, or an OTF font where it is
                                   the subr number of a referred
                                   character, or a ttf font without
                                   bit 1 of head.flags set, or (once
                                   upon a time, but no longer) a TTF
                                   font with vert metrics where it is
                                   the ymax value when we had a
                                   font-wide vertical offset.
                                   Always a temporary value */
  int ttf_glyph;                /* only used when writing out a ttf or otf font */
  Layer *layers;                /* layer[0] is background, layer[1]
                                   foreground. In type3 fonts 2-n are
                                   also foreground, otherwise also
                                   background. */
  int layer_cnt;
  StemInfo *hstem;              /* hstem hints have a vertical offset but run horizontally */
  StemInfo *vstem;              /* vstem hints have a horizontal offset but run vertically */
  DStemInfo *dstem;             /* diagonal hints for ttf */
  struct charviewbase *views;
  struct charinfo *charinfo;
  struct splinefont *parent;
  bool changed;
  bool changedsincelasthinted;
  bool manualhints;
  bool ticked;                  /* For reference character processing
                                   and fontview processing. */
  bool changed_since_autosave;
  bool widthset;                /* Needed so an emspace char doesn't disappear. */
  bool vconflicts;              /* Any hint overlaps in the vstem list? */
  bool hconflicts;              /* Any hint overlaps in the hstem list? */
  bool searcherdummy;
  bool changed_since_search;
  bool wasopen;
  bool namechanged;
  bool blended;                 /* An MM blended character */
  bool ticked2;
  unsigned int glyph_class;     /* 0=> fontforge determines class
                                   automagically, else one more than
                                   the class value in gdef so
                                   2+1=>lig, 3+1=>mark */
  bool numberpointsbackards;
  bool instructions_out_of_date;
  bool complained_about_ptnums;
  bool vs_open;
  bool unlink_rm_ovrlp_generate_undo;
  bool inspiro;
  bool lig_caret_cnt_fixed;
  struct splinecharlist *dependents;    /* The dependents list is a list
                                           of all characters which
                                           reference the current
                                           character directly. */
  KernPair *kerns;
  KernPair *vkerns;
  PST *possub;                  /* If we are a ligature then this
                                   tells us what; it may also contain
                                   a bunch of other stuff now. */
  LigList *ligofme;             /* If this is the first character of a
                                   ligature then this gives us the
                                   list of possible ones. This field
                                   must be regenerated before the font
                                   is saved. */
  char *comment;                /* in utf8 */
  uint32_t /*Color */ color;
  AnchorPoint *anchor;
  uint8_t *ttf_instrs;
  int16_t ttf_instrs_len;
  int16_t countermask_cnt;
  HintMask *countermasks;
  struct altuni *altuni;

  /* For TeX */
  int16_t tex_height;
  int16_t tex_depth;

  /* For the 'MATH' table and for TeX */
  bool is_extended_shape;
  int16_t italic_correction;
  int16_t top_accent_horiz;     /* MATH table allows you to specific a
                                   horizontal anchor for accent
                                   attachments, vertical positioning
                                   is done elsewhere. */
  DeviceTable *italic_adjusts;
  DeviceTable *top_accent_adjusts;
  struct glyphvariants *vert_variants;
  struct glyphvariants *horiz_variants;
  struct mathkern *mathkern;

  /* End of MATH/TeX fields */

  void *python_sc_object;       /* Used only if Python is supported. */
  void *python_temporary;       /* Used only if Python is supported. */

  void *python_persistent;      /* If python this will hold a python
                                   object, if not python this will
                                   hold a string containing a pickled
                                   object. We do nothing with it (if
                                   not python) except save it back out
                                   unchanged. */

  /* If the glyph is used as a tile pattern, then the next two values
     determine the amount of white space around the tile. If extra is
     non-zero then we add it to the max components of the bbox and
     subtract it from the min components. If extra is 0 then
     tile_bounds will be used. If tile_bounds is all zeros then the
     glyph's bbox will be used. */
  real tile_margin;             /* If the glyph is used as a tile. */
  DBounds tile_bounds;

} SplineChar;

#define TEX_UNDEF 0x7fff

enum ttfnames
{
  ttf_copyright = 0,
  ttf_family,
  ttf_subfamily,
  ttf_uniqueid,
  ttf_fullname,
  ttf_version,
  ttf_postscriptname,
  ttf_trademark,
  ttf_manufacturer,
  ttf_designer,
  ttf_descriptor,
  ttf_venderurl,
  ttf_designerurl,
  ttf_license,
  ttf_licenseurl,
  ttf_idontknow /*reserved */ ,
  ttf_preffamilyname,
  ttf_prefmodifiers,
  ttf_compatfull,
  ttf_sampletext,
  ttf_cidfindfontname,
  ttf_wwsfamily,
  ttf_wwssubfamily,
  ttf_namemax
};

struct ttflangname
{
  int lang;
  char *names[ttf_namemax];     /* in utf8 */
  int frommac[(ttf_namemax + 31) / 32]; /* Used when parsing the 'name' table */
  struct ttflangname *next;
};

struct MATH
{
/* From the MATH Constants subtable (constants for positioning glyphs. Not PI)*/
  int16_t ScriptPercentScaleDown;
  int16_t ScriptScriptPercentScaleDown;
  uint16_t DelimitedSubFormulaMinHeight;
  uint16_t DisplayOperatorMinHeight;
  int16_t MathLeading;
  DeviceTable *MathLeading_adjust;
  int16_t AxisHeight;
  DeviceTable *AxisHeight_adjust;
  int16_t AccentBaseHeight;
  DeviceTable *AccentBaseHeight_adjust;
  int16_t FlattenedAccentBaseHeight;
  DeviceTable *FlattenedAccentBaseHeight_adjust;
  int16_t SubscriptShiftDown;
  DeviceTable *SubscriptShiftDown_adjust;
  int16_t SubscriptTopMax;
  DeviceTable *SubscriptTopMax_adjust;
  int16_t SubscriptBaselineDropMin;
  DeviceTable *SubscriptBaselineDropMin_adjust;
  int16_t SuperscriptShiftUp;
  DeviceTable *SuperscriptShiftUp_adjust;
  int16_t SuperscriptShiftUpCramped;
  DeviceTable *SuperscriptShiftUpCramped_adjust;
  int16_t SuperscriptBottomMin;
  DeviceTable *SuperscriptBottomMin_adjust;
  int16_t SuperscriptBaselineDropMax;
  DeviceTable *SuperscriptBaselineDropMax_adjust;
  int16_t SubSuperscriptGapMin;
  DeviceTable *SubSuperscriptGapMin_adjust;
  int16_t SuperscriptBottomMaxWithSubscript;
  DeviceTable *SuperscriptBottomMaxWithSubscript_adjust;
  int16_t SpaceAfterScript;
  DeviceTable *SpaceAfterScript_adjust;
  int16_t UpperLimitGapMin;
  DeviceTable *UpperLimitGapMin_adjust;
  int16_t UpperLimitBaselineRiseMin;
  DeviceTable *UpperLimitBaselineRiseMin_adjust;
  int16_t LowerLimitGapMin;
  DeviceTable *LowerLimitGapMin_adjust;
  int16_t LowerLimitBaselineDropMin;
  DeviceTable *LowerLimitBaselineDropMin_adjust;
  int16_t StackTopShiftUp;
  DeviceTable *StackTopShiftUp_adjust;
  int16_t StackTopDisplayStyleShiftUp;
  DeviceTable *StackTopDisplayStyleShiftUp_adjust;
  int16_t StackBottomShiftDown;
  DeviceTable *StackBottomShiftDown_adjust;
  int16_t StackBottomDisplayStyleShiftDown;
  DeviceTable *StackBottomDisplayStyleShiftDown_adjust;
  int16_t StackGapMin;
  DeviceTable *StackGapMin_adjust;
  int16_t StackDisplayStyleGapMin;
  DeviceTable *StackDisplayStyleGapMin_adjust;
  int16_t StretchStackTopShiftUp;
  DeviceTable *StretchStackTopShiftUp_adjust;
  int16_t StretchStackBottomShiftDown;
  DeviceTable *StretchStackBottomShiftDown_adjust;
  int16_t StretchStackGapAboveMin;
  DeviceTable *StretchStackGapAboveMin_adjust;
  int16_t StretchStackGapBelowMin;
  DeviceTable *StretchStackGapBelowMin_adjust;
  int16_t FractionNumeratorShiftUp;
  DeviceTable *FractionNumeratorShiftUp_adjust;
  int16_t FractionNumeratorDisplayStyleShiftUp;
  DeviceTable *FractionNumeratorDisplayStyleShiftUp_adjust;
  int16_t FractionDenominatorShiftDown;
  DeviceTable *FractionDenominatorShiftDown_adjust;
  int16_t FractionDenominatorDisplayStyleShiftDown;
  DeviceTable *FractionDenominatorDisplayStyleShiftDown_adjust;
  int16_t FractionNumeratorGapMin;
  DeviceTable *FractionNumeratorGapMin_adjust;
  int16_t FractionNumeratorDisplayStyleGapMin;
  DeviceTable *FractionNumeratorDisplayStyleGapMin_adjust;
  int16_t FractionRuleThickness;
  DeviceTable *FractionRuleThickness_adjust;
  int16_t FractionDenominatorGapMin;
  DeviceTable *FractionDenominatorGapMin_adjust;
  int16_t FractionDenominatorDisplayStyleGapMin;
  DeviceTable *FractionDenominatorDisplayStyleGapMin_adjust;
  int16_t SkewedFractionHorizontalGap;
  DeviceTable *SkewedFractionHorizontalGap_adjust;
  int16_t SkewedFractionVerticalGap;
  DeviceTable *SkewedFractionVerticalGap_adjust;
  int16_t OverbarVerticalGap;
  DeviceTable *OverbarVerticalGap_adjust;
  int16_t OverbarRuleThickness;
  DeviceTable *OverbarRuleThickness_adjust;
  int16_t OverbarExtraAscender;
  DeviceTable *OverbarExtraAscender_adjust;
  int16_t UnderbarVerticalGap;
  DeviceTable *UnderbarVerticalGap_adjust;
  int16_t UnderbarRuleThickness;
  DeviceTable *UnderbarRuleThickness_adjust;
  int16_t UnderbarExtraDescender;
  DeviceTable *UnderbarExtraDescender_adjust;
  int16_t RadicalVerticalGap;
  DeviceTable *RadicalVerticalGap_adjust;
  int16_t RadicalDisplayStyleVerticalGap;
  DeviceTable *RadicalDisplayStyleVerticalGap_adjust;
  int16_t RadicalRuleThickness;
  DeviceTable *RadicalRuleThickness_adjust;
  int16_t RadicalExtraAscender;
  DeviceTable *RadicalExtraAscender_adjust;
  int16_t RadicalKernBeforeDegree;
  DeviceTable *RadicalKernBeforeDegree_adjust;
  int16_t RadicalKernAfterDegree;
  DeviceTable *RadicalKernAfterDegree_adjust;
  uint16_t RadicalDegreeBottomRaisePercent;
/* Global constants from other subtables */
  uint16_t MinConnectorOverlap; /* in the math variants sub-table */
};

enum backedup_state
{
  bs_dontknow = 0,
  bs_not = 1,
  bs_backedup = 2
};

enum loadvalidation_state
{
  lvs_bad_ps_fontname = 0x001,
  lvs_bad_glyph_table = 0x002,
  lvs_bad_cff_table = 0x004,
  lvs_bad_metrics_table = 0x008,
  lvs_bad_cmap_table = 0x010,
  lvs_bad_bitmaps_table = 0x020,
  lvs_bad_gx_table = 0x040,
  lvs_bad_ot_table = 0x080,
  lvs_bad_os2_version = 0x100,
  lvs_bad_sfnt_header = 0x200
};

typedef struct layerinfo
{
  char *name;
  bool background;              /* Layer is to be treated as background: No width, images, not worth outputting */
  bool order2;                  /* Layer's data are order 2 bezier splines (truetype) rather than order 3 (postscript) */
  /* In all glyphs in the font */
  bool ticked;
} LayerInfo;

/* Baseline data from the 'BASE' table */
struct baselangextent
{
  uint32_t lang;                /* also used for feature tag */
  struct baselangextent *next;
  int16_t ascent, descent;
  struct baselangextent *features;
};

struct basescript
{
  uint32_t script;
  struct basescript *next;
  int def_baseline;             /* index [0-baseline_cnt) */
  int16_t *baseline_pos;        /* baseline_cnt of these */
  struct baselangextent *langs; /* Language specific extents (may be NULL) */
  /* The default one has the tag DEFAULT_LANG */
};

struct Base
{
  int baseline_cnt;
  uint32_t *baseline_tags;
  /* A font does not need to provide info on all baselines, but if one script */
  /*  talks about a baseline, then all must. So the set of baselines is global */
  struct basescript *scripts;
};

struct pfminfo
{                               /* A misnomer now. OS/2 info would be more accurate, but that's stuff in here from all over ttf files */
  bool pfmset;
  bool winascent_add;
  bool windescent_add;
  bool hheadascent_add;
  bool hheaddescent_add;
  bool typoascent_add;
  bool typodescent_add;
  bool subsuper_set;
  bool panose_set;
  bool hheadset;
  bool vheadset;
  bool hascodepages;
  bool hasunicoderanges;
  unsigned char pfmfamily;
  int16_t weight;
  int16_t width;
  char panose[10];
  int16_t fstype;
  int16_t linegap;              /* from hhea */
  int16_t vlinegap;             /* from vhea */
  int16_t hhead_ascent, hhead_descent;
  int16_t os2_typoascent, os2_typodescent, os2_typolinegap;
  int16_t os2_winascent, os2_windescent;
  int16_t os2_subxsize, os2_subysize, os2_subxoff, os2_subyoff;
  int16_t os2_supxsize, os2_supysize, os2_supxoff, os2_supyoff;
  int16_t os2_strikeysize, os2_strikeypos;
  char os2_vendor[4];
  int16_t os2_family_class;
  uint32_t codepages[2];
  uint32_t unicoderanges[4];
  float os2_loweropticalsize;
  float os2_upperopticalsize;
};

struct ttf_table
{
  uint32_t tag;
  int32_t len, maxlen;
  uint8_t *data;
  struct ttf_table *next;
  FILE *temp;                   /* Temporary storage used during generation */
};

enum texdata_type
{
  tex_unset,
  tex_text,
  tex_math,
  tex_mathext
};

struct texdata
{
  enum texdata_type type;
  int32_t params[22];           /* param[6] has different meanings in normal and math fonts */
};

struct gasp
{
  uint16_t ppem;
  uint16_t flags;
};

// FIXME FIXME FIXME: This is a temporary macro for sometime use
// during the transition to a new implementation of
// SplineFont::glyphs.
#define __glyphs glyphs

typedef struct splinefont
{
  char *fontname;
  char *fullname;
  char *familyname;
  char *weight;
  char *copyright;
  char *filename;               /* sfd name. NULL if we open a font, that's origname */
  char *defbasefilename;
  char *version;
  real italicangle;
  real upos;
  real uwidth;                  /* In font info */
  int ascent;
  int descent;
  int uniqueid;                 /* Not copied when reading in!!!! */
  int glyphcnt;
  int glyphmax;                 /* allocated size of glyphs array */
  SplineChar **__glyphs;
  bool changed;
  bool changed_since_autosave;
  bool changed_since_xuidchanged;
  bool display_antialias;
  bool display_bbsized;
  bool dotlesswarn;             /* User warned that font doesn't have a dotless i character */
  bool onlybitmaps;             /* it's a bdf editor, not a postscript editor */
  bool serifcheck;              /* Have we checked to see if we have serifs? */
  bool issans;                  /* We have no serifs */
  bool isserif;                 /* We have serifs. If neither set then we don't know. */
  bool hasvmetrics;             /* We've got vertical metric data and should output vhea/vmtx/VORG tables */
  bool loading_cid_map;
  bool dupnamewarn;             /* Warn about duplicate names when loading bdf font */
  bool encodingchanged;         /* Font's encoding has changed since it was loaded */
  bool multilayer;              /* Only applies if TYPE3 is set, means
                                   this font can contain strokes &
                                   fills. I leave it in so as to avoid
                                   cluttering up code with #ifdefs. */
  bool strokedfont;
  bool new;                     /* A new and unsaved font */
  bool compacted;               /* only used when opening a font */
  unsigned int backedup:2;      /* 0=>don't know, 1=>no, 2=>yes */
  bool use_typo_metrics;        /* The standard says to. But MS seems
                                   to feel that isn't good enough and
                                   has created a bit to mean "really
                                   use them". */
  bool weight_width_slope_only; /* This bit seems stupid to me. */
  bool save_to_dir;             /* Loaded from an sfdir collection
                                   rather than a simple sfd file. */
  bool head_optimized_for_cleartype;    /* Bit in the 'head' flags field,
                                           if unset "East Asian fonts in
                                           the Windows Presentation
                                           Framework (Avalon) will not be
                                           hinted". */
  bool ticked;
  bool internal_temp;           /* Internal temporary font to be
                                   passed to freetype for
                                   rasterizing. Don't complain about
                                   oddities. Don't generate GPOS/GSUB
                                   tables, etc. */
  bool complained_about_spiros;
  bool use_xuid;                // Adobe has deprecated these two
  bool use_uniqueid;            // fields. Mostly we don't want to use them.
  struct fontviewbase *fv;
  struct metricsview *metrics;
  enum uni_interp uni_interp;
  NameList *for_new_glyphs;
  EncMap *map;                  /* Only used when opening a font, to
                                   provide original default
                                   encoding. */
  Layer grid;
  BDFFont *bitmaps;
  char *origname;               /* Filename of font file (ie. if not
                                   an sfd). */
  char *autosavename;
  int display_size;             /* A val <0 => Generate our own images
                                   from splines, a value >0 => find a
                                   bdf font of that size. */
  struct psdict *private;       /* read in from type1 file or provided by user */
  char *xuid;
  struct pfminfo pfminfo;
  struct ttflangname *names;    // FIXME: This is to be replaced with the
  // name_table field.
  SCM name_table;
  char *cidregistry, *ordering;
  int supplement;
  int subfontcnt;
  struct splinefont **subfonts;
  struct splinefont *cidmaster; /* Top level cid font */
  float cidversion;
  char *comments;               /* Used to be restricted to ASCII, now utf8 */
  char *fontlog;
  int tempuniqueid;
  int top_enc;
  uint16_t desired_row_cnt, desired_col_cnt;
  struct glyphnamehash *glyphnames;
  struct ttf_table *ttf_tables, *ttf_tab_saved;
  /* We copy: fpgm, prep, cvt, maxp (into ttf_tables) user can ask for others, into saved */
  char **cvt_names;
  /* The end of this array is marked by a special entry: */
#define END_CVT_NAMES ((char *) (~(intptr_t) 0))
  struct instrdata *instr_dlgs; /* Pointer to all table and character instruction dlgs in this font */
  struct shortview *cvt_dlg;
  struct kernclasslistdlg *kcld, *vkcld;
  struct kernclassdlg *kcd;
  struct texdata texdata;
  OTLookup *gsub_lookups, *gpos_lookups;
  AnchorClass *anchor;
  KernClass *kerns, *vkerns;
  FPST *possub;
  char *chosenname;             /* Set for files with multiple fonts in them */
  struct mmset *mm;             /* If part of a multiple master set */
  int16_t macstyle;
  char *fondname;               /* For use in generating mac families */
  /* from the GPOS 'size' feature. design_size, etc. are measured in tenths of a point */
  /*  bottom is exclusive, top is inclusive */
  /*  if any field is 0, it is undefined. All may be undefined, All may be */
  /*  defined, or design_size may be defined without any of the others */
  /*  but we can't define the range without defining the other junk */
  /*  Name must contain an English language name, may contain others */
  uint16_t design_size;
  uint16_t fontstyle_id;
  struct otfname *fontstyle_name;
  uint16_t design_range_bottom, design_range_top;
  struct otffeatname *feat_names;
  real strokewidth;
/* For GDEF Mark Attachment Class -- used in lookup flags */
/* As usual, class 0 is unused */
  int mark_class_cnt;
  char **mark_classes;          /* glyph name list */
  char **mark_class_names;      /* used within ff, utf8 (the name we've given to this class of marks) */
/* For GDEF Mark Attachment Sets -- used in lookup flags */
/* but here, set 0 is meaningful, since pst_usemarkfilteringset tells us */
  int mark_set_cnt;
  char **mark_sets;             /* glyph name list */
  char **mark_set_names;        /* used within ff, utf8 (the name we've given to this class of marks) */
#ifdef HAVE_LONG_LONG_INT
  long long creationtime;       /* seconds since 1970 */
  long long modificationtime;
#else
  long creationtime;
  long modificationtime;
#endif
  short os2_version;            /* 0 means default rather than the real version 0 */
  short compression;            /* If we opened a compressed sfd file, then save it out compressed too */
  short gasp_version;           /* 0/1 currently */
  short gasp_cnt;
  struct gasp *gasp;
  struct MATH *MATH;
  float sfd_version;            /* Used only when reading in an sfd file */
  struct gfi_data *fontinfo;
  struct val_data *valwin;
#if !defined(_NO_PYTHON)
  void *python_temporary;
#endif
  void *python_persistent;      /* If python this will hold a python object, if not python this will hold a string containing a pickled object. We do nothing with it (if not python) except save it back out unchanged */
  enum loadvalidation_state loadvalidation_state;
  LayerInfo *layers;
  int layer_cnt;
  int display_layer;
  struct Base *horiz_base, *vert_base;
  Justify *justify;
  int extrema_bound;            /* Splines do not count for extrema
                                   complaints when the distance
                                   between the endpoints is less than
                                   or equal to this. */
  int width_separation;
  int sfntRevision;
#define sfntRevisionUnset	0x44445555
  int woffMajor;
#define woffUnset		0x4455
  int woffMinor;
  char *woffMetadata;
  real ufo_ascent, ufo_descent; /* I don't know what these mean; they
                                   don't seem to correspond to any
                                   other ascent/descent pair, but
                                   retain them, so round-trip ufo
                                   input/output leaves them unchanged.
                                   ufo_descent is negative. */
} SplineFont;

inline SplineChar *sfglyph (SplineFont *sf, ssize_t i);
inline void set_sfglyph (SplineFont *sf, ssize_t i, SplineChar *sc);

// FIXME: Reimplement as a SCM data structure.
inline SplineChar *
sfglyph (SplineFont *sf, ssize_t i)
{
  return (0 <= i && i < sf->glyphcnt) ? sf->__glyphs[i] : NULL;
}

void resize_sfglyph_array (SplineFont *sf, size_t min_size);

// FIXME: Reimplement as a SCM data structure.
inline void
set_sfglyph (SplineFont *sf, ssize_t i, SplineChar *sc)
{
  assert (0 <= i);
  if (sf->glyphmax <= i)
    resize_sfglyph_array (sf, i);
  sf->__glyphs[i] = sc;
}

struct axismap
{
  int points;                   /* size of the next two arrays */
  real *blends;                 /* between [0,1] ordered so that blend[0]<blend[1]<... */
  real *designs;                /* between the design ranges for this axis, typically [1,999] or [6,72] */
  real min, def, max;           /* For mac */
};

/* I am going to simplify my life and not encourage intermediate designs */
/*  this means I can easily calculate ConvertDesignVector, and don't have */
/*  to bother the user with specifying it. */
/* (NormalizeDesignVector is fairly basic and shouldn't need user help ever) */
/*  (As long as they want piecewise linear) */
typedef struct mmset
{
  int axis_count;
  char *axes[4];
  int instance_count;
  SplineFont **instances;
  SplineFont *normal;
  real *positions;              /* array[instance][axis] saying where each instance lies on each axis */
  real *defweights;             /* array[instance] saying how much of each instance makes the normal font */
  struct axismap *axismaps;     /* array[axis] */
  char *cdv, *ndv;              /* for adobe */
  bool changed;
} MMSet;

/* mac styles. Useful idea we'll just steal it */
enum style_flags
{
  sf_bold = 1,
  sf_italic = 2,
  sf_underline = 4,
  sf_outline = 8,
  sf_shadow = 0x10,
  sf_condense = 0x20,
  sf_extend = 0x40
};

struct sflist
{
  SplineFont *sf;
  int32_t *sizes;
  FILE *tempttf;                /* For ttf */
  int id;                       /* For ttf */
  int *ids;                     /* One for each size */
  BDFFont **bdfs;               /* Ditto */
  EncMap *map;
  struct sflist *next;
  char **former_names;
  int len;
};

    /* Used for drawing text with mark to base anchors */
typedef struct anchorpos
{
  SplineChar *sc;               /* This is the mark being positioned */
  int x, y;                     /* Its origin should be shifted this much relative to that of the original base char */
  AnchorPoint *apm;             /* The anchor point in sc used to position it */
  AnchorPoint *apb;             /* The anchor point in the base character against which we are positioned */
  int base_index;               /* Index in this array to the base character (-1=> original base char) */
  bool ticked;                  /* Used as a mark to mark */
} AnchorPos;

enum ttf_flags
{
  ttf_flag_shortps = 1,
  ttf_flag_nohints = 2,
  ttf_flag_applemode = 4,
  ttf_flag_pfed_comments = 8,
  ttf_flag_pfed_colors = 0x10,
  ttf_flag_otmode = 0x20,
  ttf_flag_glyphmap = 0x40,
  ttf_flag_TeXtable = 0x80,
  ttf_flag_ofm = 0x100,
  ttf_flag_oldkern = 0x200,     /* never set in conjunction with applemode */
  ttf_flag_pfed_lookupnames = 0x800,
  ttf_flag_pfed_guides = 0x1000,
  ttf_flag_pfed_layers = 0x2000,
  ttf_flag_symbol = 0x4000,
  ttf_flag_dummyDSIG = 0x8000
};

enum ttc_flags
{
  ttc_flag_trymerge = 0x1,
  ttc_flag_cff = 0x2
};

enum openflags
{
  of_fstypepermitted = 1,
  of_askcmap = 2,
  of_all_glyphs_in_ttc = 4,
  of_fontlint = 8,
  of_hidewindow = 0x10
};

enum ps_flags
{
  ps_flag_nohintsubs = 0x10000,
  ps_flag_noflex = 0x20000,
  ps_flag_nohints = 0x40000,
  ps_flag_restrict256 = 0x80000,
  ps_flag_afm = 0x100000,
  ps_flag_pfm = 0x200000,
  ps_flag_tfm = 0x400000,
  ps_flag_round = 0x800000,
/* CFF fonts are wrapped up in some postscript sugar -- unless they are to */
/*  go into a pdf file or an otf font */
  ps_flag_nocffsugar = 0x1000000,
/* in type42 cid fonts we sometimes want an identity map from gid to cid */
  ps_flag_identitycidmap = 0x2000000,
  ps_flag_afmwithmarks = 0x4000000,
  ps_flag_noseac = 0x8000000,
  ps_flag_outputfontlog = 0x10000000,
  ps_flag_mask =
    (ps_flag_nohintsubs | ps_flag_noflex | ps_flag_afm | ps_flag_pfm |
     ps_flag_tfm | ps_flag_round)
};

struct compressors
{
  char *ext, *decomp, *recomp;
};

#define COMPRESSORS_EMPTY { NULL, NULL, NULL }

VISIBLE extern struct compressors compressors[];

enum archive_list_style
{
  ars_tar,
  ars_zip
};

struct archivers
{
  char *ext, *unarchive, *archive, *listargs, *extractargs, *appendargs;
  enum archive_list_style ars;
};

#define ARCHIVERS_EMPTY { NULL, NULL, NULL, NULL, NULL, NULL, 0 }

struct fontdict;
struct pschars;
struct findsel;
struct charprocs;
struct enc;

VISIBLE char *strconcat (const char *str, const char *str2);
VISIBLE char *strconcat3 (const char *str, const char *str2, const char *str3);

char *XUIDFromFD (int xuid[20]);
SplineFont *SplineFontFromPSFont (struct fontdict *fd);
int CheckAfmOfPostScript (SplineFont *sf, char *psname, EncMap *map);
int LoadKerningDataFromAmfm (SplineFont *sf, char *filename, EncMap *map);
int LoadKerningDataFromAfm (SplineFont *sf, char *filename, EncMap *map);
int LoadKerningDataFromTfm (SplineFont *sf, char *filename, EncMap *map);
int LoadKerningDataFromOfm (SplineFont *sf, char *filename, EncMap *map);
int LoadKerningDataFromPfm (SplineFont *sf, char *filename, EncMap *map);
int LoadKerningDataFromMacFOND (SplineFont *sf, char *filename, EncMap *map);
VISIBLE int LoadKerningDataFromMetricsFile (SplineFont *sf,
                                            char *filename, EncMap *map);
VISIBLE void FeatDumpFontLookups (FILE *out, SplineFont *sf);
VISIBLE void FeatDumpOneLookup (FILE *out, SplineFont *sf, OTLookup *otl);
void SFApplyFeatureFile (SplineFont *sf, FILE *file, char *filename);
void SFApplyFeatureFilename (SplineFont *sf, char *filename);
void SFApplyFeatureString (SplineFont *sf, char *features);
void SubsNew (SplineChar *to, enum possub_type type, int tag,
              char *components, SplineChar *default_script);
void PosNew (SplineChar *to, int tag, int dx, int dy, int dh, int dv);
int SFOneWidth (SplineFont *sf);
int CIDOneWidth (SplineFont *sf);
int SFOneHeight (SplineFont *sf);
int SFIsCJK (SplineFont *sf, EncMap *map);
VISIBLE void CIDMasterAsDes (SplineFont *sf);

typedef enum fontformat
{
  // If you change this enum, please also make any appropriate changes
  // fontforge/guile_fonts_font_formats.c.
  ff_pfa = 0,
  ff_pfb,
  ff_pfbmacbin,
  ff_multiple,
  ff_mma,
  ff_mmb,
  ff_ptype3,
  ff_ptype0,
  ff_cid,
  ff_cff,
  ff_cffcid,
  ff_type42,
  ff_type42cid,
  ff_ttf,
  ff_ttfsym,
  ff_ttfmacbin,
  ff_ttc,
  ff_ttfdfont,
  ff_otf,
  ff_otfdfont,
  ff_otfcid,
  ff_otfciddfont,
  ff_svg,
  ff_ufo,
  ff_woff,
  ff_none                       // ff_none comes last.
} FontFormat;

VISIBLE int CanWoff (void);
struct pschars *SplineFont2ChrsSubrs (SplineFont *sf, int iscjk,
                                      struct pschars *subrs, int flags,
                                      enum fontformat format, int layer);
int CanonicalCombiner (int uni);
struct cidbytes;
struct fd2data;
struct ttfinfo;
struct alltabs;

typedef struct growbuf
{
  unsigned char *pt;
  unsigned char *base;
  unsigned char *end;
} GrowBuf;

void GrowBuffer (GrowBuf * gb);
void GrowBufferAdd (GrowBuf * gb, int ch);
VISIBLE void GrowBufferAddStr (GrowBuf * gb, char *str);

struct glyphdata;
int UnitsParallel (BasePoint *u1, BasePoint *u2, int strict);
int CvtPsStem3 (struct growbuf *gb, SplineChar *scs[MmMax],
                int instance_count, int ishstem, int round);
struct pschars *CID2ChrsSubrs (SplineFont *cidmaster,
                               struct cidbytes *cidbytes, int flags, int layer);
struct pschars *SplineFont2ChrsSubrs2 (SplineFont *sf, int nomwid,
                                       int defwid, const int *bygid,
                                       int cnt, int flags,
                                       struct pschars **_subrs, int layer);
struct pschars *CID2ChrsSubrs2 (SplineFont *cidmaster,
                                struct fd2data *fds, int flags,
                                struct pschars **_glbls, int layer);
enum bitmapformat
{
  bf_bdf,
  bf_ttf,
  bf_sfnt_dfont,
  bf_sfnt_ms,
  bf_otb,
  bf_nfntmacbin,
  /*bf_nfntdfont, */
  bf_fon,
  bf_fnt,
  bf_palm,
  bf_ptype3,
  bf_none
};

int32_t filechecksum (FILE *file);
VISIBLE const char *GetAuthor (void);
SplineChar *SFFindExistingCharMac (SplineFont *, EncMap *map, int unienc);
void SC_PSDump (void (*dumpchar) (int ch, void *data), void *data,
                SplineChar *sc, int refs_to_splines, int pdfopers, int layer);
int _WritePSFont (FILE *out, SplineFont *sf, enum fontformat format,
                  int flags, EncMap *enc, SplineFont *fullsf, int layer);
int WritePSFont (char *fontname, SplineFont *sf,
                 enum fontformat format, int flags, EncMap *enc,
                 SplineFont *fullsf, int layer);
int WriteMacPSFont (char *fontname, SplineFont *sf,
                    enum fontformat format, int flags, EncMap *enc, int layer);
int _WriteWOFFFont (FILE *ttf, SplineFont *sf, enum fontformat format,
                    int32_t *bsizes, enum bitmapformat bf, int flags,
                    EncMap *enc, int layer);
int WriteWOFFFont (char *fontname, SplineFont *sf,
                   enum fontformat format, int32_t *bsizes,
                   enum bitmapformat bf, int flags, EncMap *enc, int layer);
int _WriteTTFFont (FILE *ttf, SplineFont *sf, enum fontformat format,
                   int32_t *bsizes, enum bitmapformat bf, int flags,
                   EncMap *enc, int layer);
int WriteTTFFont (char *fontname, SplineFont *sf,
                  enum fontformat format, int32_t *bsizes,
                  enum bitmapformat bf, int flags, EncMap *enc, int layer);
int _WriteType42SFNTS (FILE *type42, SplineFont *sf,
                       enum fontformat format, int flags, EncMap *enc,
                       int layer);
int WriteMacTTFFont (char *fontname, SplineFont *sf,
                     enum fontformat format, int32_t *bsizes,
                     enum bitmapformat bf, int flags, EncMap *enc, int layer);
int WriteMacBitmaps (char *filename, SplineFont *sf, int32_t *sizes,
                     int is_dfont, EncMap *enc);
int WritePalmBitmaps (char *filename, SplineFont *sf, int32_t *sizes,
                      EncMap *enc);
VISIBLE int WriteMacFamily (char *filename, struct sflist *sfs,
                            enum fontformat format,
                            enum bitmapformat bf, int flags, int layer);
VISIBLE int WriteTTC (char *filename, struct sflist *sfs,
                      enum fontformat format, enum bitmapformat bf,
                      int flags, int layer, enum ttc_flags ttcflags);
long mactime (void);
int WriteSVGFont (char *fontname, SplineFont *sf,
                  enum fontformat format, int flags, EncMap *enc, int layer);
int _WriteSVGFont (FILE *file, SplineFont *sf, enum fontformat format,
                   int flags, EncMap *enc, int layer);
int WriteUFOFont (char *fontname, SplineFont *sf,
                  enum fontformat format, int flags, EncMap *enc, int layer);
VISIBLE void SfListFree (struct sflist *sfs);
VISIBLE void TTF_PSDupsDefault (SplineFont *sf);
void DefaultTTFEnglishNames (struct ttflangname *dummy, SplineFont *sf);
VISIBLE void TeXDefaultParams (SplineFont *sf);
VISIBLE int AlreadyMSSymbolArea (SplineFont *sf, EncMap *map);
VISIBLE void OS2FigureCodePages (SplineFont *sf, uint32_t CodePage[2]);
VISIBLE void OS2FigureUnicodeRanges (SplineFont *sf, uint32_t Ranges[4]);
void SFDefaultOS2 (SplineFont *sf);
VISIBLE void SFDefaultOS2Info (struct pfminfo *pfminfo, SplineFont *sf,
                               char *fontname);
void SFDefaultOS2Simple (struct pfminfo *pfminfo, SplineFont *sf);
VISIBLE void SFDefaultOS2SubSuper (struct pfminfo *pfminfo, int emsize,
                                   double italicangle);
uint16_t head_table_flags (SplineFont *sf, enum fontformat format,
                           int32_t *bsizes, bool arabic, bool rl);
void VerifyLanguages (SplineFont *sf);
VISIBLE int ScriptIsRightToLeft (uint32_t script);
void ScriptMainRange (uint32_t script, int *start, int *end);
VISIBLE uint32_t ScriptFromUnicode (int u, SplineFont *sf);
VISIBLE uint32_t SCScriptFromUnicode (SplineChar *sc);
VISIBLE int SCRightToLeft (SplineChar *sc);
int SLIContainsR2L (SplineFont *sf, int sli);
void SFFindNearTop (SplineFont *);
void SFRestoreNearTop (SplineFont *);
VISIBLE int SFForceEncoding (SplineFont *sf, EncMap *old, Encoding *new_map);
int CountOfEncoding (Encoding *encoding_name);
void SFMatchGlyphs (SplineFont *sf, SplineFont *target, int addempties);
VISIBLE void MMMatchGlyphs (MMSet *mm);
char *_GetModifiers (char *fontname, char *familyname, char *weight);
char *SFGetModifiers (SplineFont *sf);
VISIBLE const uint32_t *_uGetModifiers (const uint32_t *fontname,
                                        const uint32_t *familyname,
                                        const uint32_t *weight);
void SFSetFontName (SplineFont *sf, char *family, char *mods, char *full);
void ttfdumpbitmap (SplineFont *sf, struct alltabs *at, int32_t *sizes);
void ttfdumpbitmapscaling (SplineFont *sf, struct alltabs *at, int32_t *sizes);
VISIBLE void SplineFontSetUnChanged (SplineFont *sf);

VISIBLE int PointsDiagonalable (SplineFont *sf, BasePoint **bp,
                                BasePoint *unit);
VISIBLE int MergeDStemInfo (SplineFont *sf, DStemInfo ** ds, DStemInfo * test);

void LineListFree (LineList *ll);
void LinearApproxFree (LinearApprox * la);
VISIBLE void SplineFree (Spline *spline);
VISIBLE SplinePoint *SplinePointCreate (real x, real y);
VISIBLE void SplinePointFree (SplinePoint *sp);
void SplinePointsFree (SplinePointList *spl);
VISIBLE void SplinePointListFree (SplinePointList *spl);
VISIBLE void SplinePointListsFree (SplinePointList *head);
VISIBLE void SplineSetSpirosClear (SplineSet *spl);
void SplineSetBeziersClear (SplineSet *spl);
VISIBLE void RefCharFree (RefChar *ref);
VISIBLE void RefCharsFree (RefChar *ref);
VISIBLE void RefCharsFreeRef (RefChar *ref);
void CopyBufferFree (void);
void CopyBufferClearCopiedFrom (SplineFont *dying);
VISIBLE void UndoesFree (Undoes *undo);
VISIBLE void StemInfosFree (StemInfo * h);
VISIBLE void StemInfoFree (StemInfo * h);
VISIBLE void DStemInfosFree (DStemInfo * h);
VISIBLE void DStemInfoFree (DStemInfo * h);
VISIBLE void KernPairsFree (KernPair *kp);
VISIBLE void SCOrderAP (SplineChar *sc);
void AnchorPointsFree (AnchorPoint *ap);
AnchorPoint *AnchorPointsCopy (AnchorPoint *alist);
AnchorPoint *AnchorPointsSort (AnchorClass *ac_list, AnchorPoint *ap_list);
VISIBLE void SFRemoveAnchorClass (SplineFont *sf, AnchorClass *an);
int AnchorClassesNextMerge (AnchorClass *ac);
VISIBLE int IsAnchorClassUsed (SplineChar *sc, AnchorClass *an);
AnchorPoint *APAnchorClassMerge (AnchorPoint *anchors,
                                 AnchorClass *into, AnchorClass *from);
void AnchorClassMerge (SplineFont *sf, AnchorClass *into, AnchorClass *from);
void AnchorClassesFree (AnchorClass *kp);
VISIBLE void TtfTablesFree (struct ttf_table *tab);
void SFRemoveSavedTable (SplineFont *sf, uint32_t tag);
VISIBLE AnchorClass *AnchorClassMatch (SplineChar *sc1,
                                       SplineChar *sc2,
                                       AnchorClass *restrict_,
                                       AnchorPoint **_ap1, AnchorPoint **_ap2);
AnchorClass *AnchorClassMkMkMatch (SplineChar *sc1, SplineChar *sc2,
                                   AnchorPoint **_ap1, AnchorPoint **_ap2);
AnchorClass *AnchorClassCursMatch (SplineChar *sc1, SplineChar *sc2,
                                   AnchorPoint **_ap1, AnchorPoint **_ap2);
void SCInsertPST (SplineChar *sc, PST *new_);
VISIBLE void ValDevFree (ValDevTab *adjust);
VISIBLE ValDevTab *ValDevTabCopy (ValDevTab *orig);
VISIBLE void DeviceTableFree (DeviceTable *adjust);
VISIBLE DeviceTable *DeviceTableCopy (DeviceTable *orig);
VISIBLE void DeviceTableSet (DeviceTable *adjust, int size, int correction);
VISIBLE void PSTFree (PST *lig);
uint16_t PSTDefaultFlags (enum possub_type type, SplineChar *sc);
VISIBLE int PSTContains (const char *components, const char *name);
VISIBLE StemInfo *StemInfoCopy (StemInfo * h);
VISIBLE DStemInfo *DStemInfoCopy (DStemInfo * h);
VISIBLE void SPChangePointType (SplinePoint *sp, int pointtype);

static inline SplineFont *
optional_cidmaster (SplineFont *sf)
{
  return (sf->cidmaster == NULL) ? sf : sf->cidmaster;
}

struct lookup_cvt
{
  OTLookup *from, *to;
  int old;
};

struct sub_cvt
{
  struct lookup_subtable *from, *to;
  int old;
};

struct ac_cvt
{
  AnchorClass *from, *to;
  int old;
};

struct sfmergecontext
{
  SplineFont *sf_from, *sf_to;
  int lcnt;
  struct lookup_cvt *lks;
  int scnt;
  struct sub_cvt *subs;
  int acnt;
  struct ac_cvt *acs;
  char *prefix;
  int preserveCrossFontKerning;
  int lmax;
};

PST *PSTCopy (PST *base, SplineChar *sc, struct sfmergecontext *mc);
struct lookup_subtable *MCConvertSubtable (struct sfmergecontext *mc,
                                           struct lookup_subtable *sub);
AnchorClass *MCConvertAnchorClass (struct sfmergecontext *mc, AnchorClass *ac);
void SFFinishMergeContext (struct sfmergecontext *mc);
VISIBLE SplineChar *SplineCharCopy (SplineChar *sc, SplineFont *into,
                                    struct sfmergecontext *);
BDFChar *BDFCharCopy (BDFChar *bc);
VISIBLE void BCFlattenFloat (BDFChar *bc);
void BitmapsCopy (SplineFont *to, SplineFont *from, int to_index,
                  int from_index);
struct gimage *ImageAlterClut (struct gimage *image);
VISIBLE void ImageListsFree (ImageList *imgs);
VISIBLE void TTFLangNamesFree (struct ttflangname *l);
VISIBLE void AltUniFree (struct altuni *altuni);
void AltUniFigure (SplineFont *sf, EncMap *map, int check_dups);
void AltUniRemove (SplineChar *sc, int uni);
void AltUniAdd (SplineChar *sc, int uni);
void AltUniAdd_DontCheckDups (SplineChar *sc, int uni);
VISIBLE void LayerDefault (Layer *);
SplineChar *SplineCharCreate (int layer_cnt);
VISIBLE SplineChar *SFSplineCharCreate (SplineFont *sf);
VISIBLE RefChar *RefCharCreate (void);
VISIBLE RefChar *RefCharsCopy (RefChar *ref);   /* Still needs to be instanciated and have the dependency list adjusted */
VISIBLE struct altuni *AltUniCopy (struct altuni *altuni,
                                   SplineFont *noconflicts);
void SCAddRef (SplineChar *sc, SplineChar *rsc, int layer, real xoff,
               real yoff);
void _SCAddRef (SplineChar *sc, SplineChar *rsc, int layer, real transform[6]);
KernClass *KernClassCopy (KernClass *kc);
void KernClassFreeContents (KernClass *kc);
VISIBLE void KernClassListFree (KernClass *kc);
int KernClassContains (KernClass *kc, char *name1, char *name2, int ordered);
void OTLookupFree (OTLookup *lookup);
void OTLookupListFree (OTLookup *lookup);
FPST *FPSTCopy (FPST *fpst);
VISIBLE void FPSTRuleContentsFree (struct fpst_rule *r,
                                   enum fpossub_format format);
VISIBLE void FPSTClassesFree (FPST *fpst);
VISIBLE void FPSTRulesFree (struct fpst_rule *r,
                            enum fpossub_format format, int rcnt);
VISIBLE void FPSTFree (FPST *fpst);
VISIBLE void GlyphVariantsFree (struct glyphvariants *gv);
VISIBLE struct glyphvariants *GlyphVariantsCopy (struct glyphvariants *gv);
void MathKernVContentsFree (struct mathkernvertex *mk);
VISIBLE void MathKernFree (struct mathkern *mk);
VISIBLE struct mathkern *MathKernCopy (struct mathkern *mk);
void SplineCharListsFree (struct splinecharlist *dlist);
void LayerFreeContents (SplineChar *sc, int layer);
VISIBLE void SplineCharFreeContents (SplineChar *sc);
VISIBLE void SplineCharFree (SplineChar *sc);
VISIBLE void EncMapFree (EncMap *map);
VISIBLE EncMap *EncMapFromEncoding (SplineFont *sf, Encoding *enc);
VISIBLE EncMap *CompactEncMap (EncMap *map, SplineFont *sf);
VISIBLE EncMap *EncMapNew (int encmax, Encoding *enc);
VISIBLE EncMap *EncMap1to1 (int enc_limit);
VISIBLE EncMap *EncMapCopy (EncMap *map);
void SFExpandGlyphCount (SplineFont *sf, int newcnt);
void ScriptLangListFree (struct scriptlanglist *sl);
VISIBLE void FeatureScriptLangListFree (FeatureScriptLangList *fl);
VISIBLE void SFBaseSort (SplineFont *sf);
VISIBLE struct baselangextent *BaseLangCopy (struct baselangextent *extent);
VISIBLE void BaseLangFree (struct baselangextent *extent);
void BaseScriptFree (struct basescript *bs);
VISIBLE void BaseFree (struct Base *base);
VISIBLE void SplineFontFree (SplineFont *sf);
VISIBLE struct jstf_lang *JstfLangsCopy (struct jstf_lang *jl);
VISIBLE void JstfLangFree (struct jstf_lang *jl);
VISIBLE void JustifyFree (Justify * just);
VISIBLE void MATHFree (struct MATH *math);
VISIBLE struct MATH *MathTableNew (SplineFont *sf);
VISIBLE void OtfNameListFree (struct otfname *on);
VISIBLE void OtfFeatNameListFree (struct otffeatname *fn);
struct otffeatname *findotffeatname (uint32_t tag, SplineFont *sf);
VISIBLE void MarkSetFree (int cnt, char **classes, char **names);
VISIBLE void MarkClassFree (int cnt, char **classes, char **names);
VISIBLE void MMSetFreeContents (MMSet *mm);
void MMSetFree (MMSet *mm);
VISIBLE void SFRemoveUndoes (SplineFont *sf, uint8_t *selected, EncMap *map);
VISIBLE void SplineRefigure3 (Spline *spline);
VISIBLE void SplineRefigure (Spline *spline);
VISIBLE Spline *SplineMake3 (SplinePoint *from, SplinePoint *to);
LinearApprox *SplineApproximate (Spline *spline, real scale);
VISIBLE int SplinePointListIsClockwise (const SplineSet *spl);
VISIBLE void SplineSetFindBounds (const SplinePointList *spl, DBounds *bounds);
VISIBLE void SplineCharLayerFindBounds (SplineChar *sc, int layer,
                                        DBounds *bounds);
VISIBLE void SplineCharFindBounds (SplineChar *sc, DBounds *bounds);
void SplineFontLayerFindBounds (SplineFont *sf, int layer, DBounds *bounds);
void SplineFontFindBounds (SplineFont *sf, DBounds *bounds);
VISIBLE void CIDLayerFindBounds (SplineFont *sf, int layer, DBounds *bounds);
VISIBLE void SplineSetQuickBounds (SplineSet *ss, DBounds *b);
VISIBLE void SplineCharLayerQuickBounds (SplineChar *sc, int layer,
                                         DBounds *bounds);
VISIBLE void SplineCharQuickBounds (SplineChar *sc, DBounds *b);
VISIBLE void SplineSetQuickConservativeBounds (SplineSet *ss, DBounds *b);
VISIBLE void SplineCharQuickConservativeBounds (SplineChar *sc, DBounds *b);
VISIBLE void SplineCharLayerQuickConservativeBounds (SplineChar *sc, int layer,
                                                     DBounds *bounds);
void SplineFontQuickConservativeBounds (SplineFont *sf, DBounds *b);
VISIBLE void SplinePointCategorize (SplinePoint *sp);
int SplinePointIsACorner (SplinePoint *sp);
VISIBLE void SPLCategorizePoints (SplinePointList *spl);
void SCCategorizePoints (SplineChar *sc);
VISIBLE SplinePointList *SplinePointListCopy1 (const SplinePointList *spl);
VISIBLE SplinePointList *SplinePointListCopy (const SplinePointList *base);
VISIBLE SplinePointList *SplinePointListCopySelected (SplinePointList *base);
VISIBLE SplinePointList
  *SplinePointListCopySpiroSelected (SplinePointList *base);
ImageList *ImageListCopy (ImageList *cimg);
VISIBLE ImageList *ImageListTransform (ImageList *cimg,
                                       real transform[6], int everything);
void BpTransform (BasePoint *to, BasePoint *from, real transform[6]);
VISIBLE void ApTransform (AnchorPoint *ap, real transform[6]);

/* The order of the enum elements below doesn't make much sense, but
   it's done this way to preserve binary compatibility */
enum transformPointType
{
  tpt_OnlySelected,
  tpt_AllPoints,
  tpt_OnlySelectedInterpCPs
};

VISIBLE SplinePointList *SplinePointListTransform (SplinePointList
                                                   *base,
                                                   real transform[6],
                                                   enum
                                                   transformPointType
                                                   allpoints);
VISIBLE SplinePointList *SplinePointListSpiroTransform (SplinePointList
                                                        *base,
                                                        real
                                                        transform[6],
                                                        int allpoints);
SplinePointList *SplinePointListShift (SplinePointList *base,
                                       real xoff,
                                       enum transformPointType allpoints);
HintMask *HintMaskFromTransformedRef (RefChar *ref, BasePoint *trans,
                                      SplineChar *basesc, HintMask * hm);
SplinePointList *SPLCopyTranslatedHintMasks (SplinePointList *base,
                                             SplineChar *basesc,
                                             SplineChar *subsc,
                                             BasePoint *trans);
SplinePointList *SPLCopyTransformedHintMasks (RefChar *r,
                                              SplineChar *basesc,
                                              BasePoint *trans, int layer);
VISIBLE SplinePointList *SplinePointListRemoveSelected (SplineChar *sc,
                                                        SplinePointList *base);
void SplinePointListSet (SplinePointList *tobase, SplinePointList *frombase);
void SplinePointListSelect (SplinePointList *spl, int sel);
VISIBLE void SCRefToSplines (SplineChar *sc, RefChar *rf, int layer);
VISIBLE void RefCharFindBounds (RefChar *rf);
VISIBLE void SCReinstanciateRefChar (SplineChar *sc, RefChar *rf, int layer);
VISIBLE void SCReinstanciateRef (SplineChar *sc, SplineChar *rsc, int layer);
VISIBLE void SFReinstanciateRefs (SplineFont *sf);
void SFInstanciateRefs (SplineFont *sf);
SplineChar *MakeDupRef (SplineChar *base, int local_enc, int uni_enc);
VISIBLE void SCRemoveDependent (SplineChar *dependent, RefChar *rf, int layer);
void SCRemoveLayerDependents (SplineChar *dependent, int layer);
void SCRemoveDependents (SplineChar *dependent);
VISIBLE int SCDependsOnSC (SplineChar *parent, SplineChar *child);
VISIBLE void BCCompressBitmap (BDFChar *bdfc);
void BCRegularizeBitmap (BDFChar *bdfc);
void BCRegularizeGreymap (BDFChar *bdfc);
VISIBLE void BCPasteInto (BDFChar *bc, BDFChar *rbc, int ixoff,
                          int iyoff, int invert, int cleartoo);
void BCRotateCharForVert (BDFChar *bc, BDFChar *from, BDFFont *frombdf);
int GradientHere (bigreal scale, DBounds *bbox, int iy, int ix,
                  struct gradient *grad, struct pattern *pat, int defgrey);
void PatternPrep (SplineChar *sc, struct brush *brush, bigreal scale);
VISIBLE BDFChar *SplineCharRasterize (SplineChar *sc, int layer,
                                      bigreal pixelsize);
BDFFont *SplineFontToBDFHeader (SplineFont *_sf, int pixelsize, int indicate);
BDFFont *SplineFontRasterize (SplineFont *sf, int layer, int pixelsize,
                              int indicate);
void BDFCAntiAlias (BDFChar *bc, int linear_scale);
VISIBLE BDFChar *SplineCharAntiAlias (SplineChar *sc, int layer,
                                      int pixelsize, int linear_scale);
BDFFont *SplineFontAntiAlias (SplineFont *sf, int layer, int pixelsize,
                              int linear_scale);
VISIBLE struct clut *_BDFClut (int linear_scale);
void BDFClut (BDFFont *bdf, int linear_scale);
VISIBLE int BDFDepth (BDFFont *bdf);
VISIBLE BDFChar *BDFPieceMeal (BDFFont *bdf, int index);
VISIBLE BDFChar *BDFPieceMealCheck (BDFFont *bdf, int index);

enum piecemeal_flags
{
  pf_antialias = 1,
  pf_bbsized = 2,
  pf_ft_nohints = 4,
  pf_ft_recontext = 8
};

VISIBLE BDFFont *SplineFontPieceMeal (SplineFont *sf, int layer,
                                      int ptsize, int dpi, int flags,
                                      void *freetype_context);
VISIBLE void BDFCharFindBounds (BDFChar *bc, IBounds * bb);
VISIBLE int BDFCharQuickBounds (BDFChar *bc, IBounds * bb, int8_t xoff,
                                int8_t yoff, int use_backup, int first);
void BCPrepareForOutput (BDFChar *bc, int mergeall);
void BCRestoreAfterOutput (BDFChar *bc);
void BCMakeDependent (BDFChar *dependent, BDFChar *base);
VISIBLE void BCRemoveDependent (BDFChar *dependent, BDFRefChar * rf);
void BCExpandBitmapToEmBox (BDFChar *bc, int xmin, int ymin, int xmax,
                            int ymax);
BDFFont *BitmapFontScaleTo (BDFFont *old, int to);
VISIBLE void BDFCharFree (BDFChar *bdfc);
VISIBLE void BDFPropsFree (BDFFont *bdf);
VISIBLE void BDFFontFree (BDFFont *bdf);
void SFDefaultAscent (SplineFont *sf);
int PSBitmapDump (char *filename, BDFFont *font, EncMap *map);
int BDFFontDump (char *filename, BDFFont *font, EncMap *map, int res);
int FNTFontDump (char *filename, BDFFont *font, EncMap *map, int res);
int FONFontDump (char *filename, SplineFont *sf, int32_t *sizes,
                 int res, EncMap *map);
VISIBLE void SFReplaceEncodingBDFProps (SplineFont *sf, EncMap *map);
VISIBLE void SFReplaceFontnameBDFProps (SplineFont *sf);
int IsUnsignedBDFKey (char *key);
VISIBLE int BdfPropHasInt (BDFFont *font, const char *key, int def);
VISIBLE char *BdfPropHasString (BDFFont *font, const char *key, char *def);
void def_Charset_Enc (EncMap *map, char *reg, char *enc);
VISIBLE void Default_XLFD (BDFFont *bdf, EncMap *map, int res);
VISIBLE void Default_Properties (BDFFont *bdf, EncMap *map, char *onlyme);
VISIBLE void BDFDefaultProps (BDFFont *bdf, EncMap *map, int res);
VISIBLE BDFProperties *BdfPropsCopy (BDFProperties * props, int cnt);

struct xlfd_components
{
  char foundry[80];
  char family[100];
  char weight[80];
  char slant[40];
  char setwidth[50];
  char add_style[50];
  int pixel_size;
  int point_size;
  int res_x;
  int res_y;
  char spacing[40];
  int avg_width;
  char cs_reg[80];              /* encoding */
  char cs_enc[80];              /* encoding version? */
  int char_cnt;
};

struct std_bdf_props
{
  char *name;
  int type;
  int defaultable;
};

#define STD_BDF_PROPS_EMPTY { NULL, 0, 0 }

VISIBLE void XLFD_GetComponents (char *xlfd, struct xlfd_components *comp);
VISIBLE void XLFD_CreateComponents (BDFFont *bdf, EncMap *map, int res,
                                    struct xlfd_components *comp);
/* Two lines intersect in at most 1 point */
/* Two quadratics intersect in at most 4 points */
/* Two cubics intersect in at most 9 points */
/* Plus an extra space for a trailing -1 */
VISIBLE int SplinesIntersect (const Spline *s1, const Spline *s2,
                              BasePoint pts[9], my_extended t1s[10],
                              my_extended t2s[10]);
VISIBLE SplineSet *LayerAllSplines (Layer *layer);
VISIBLE SplineSet *LayerUnAllSplines (Layer *layer);
VISIBLE int SplineSetIntersect (SplineSet *spl, Spline **_spline,
                                Spline **_spline2);
int LineTangentToSplineThroughPt (Spline *s, BasePoint *pt,
                                  my_extended ts[4], my_extended tmin,
                                  my_extended tmax);
VISIBLE int _CubicSolve (const Spline1D *sp, bigreal sought, my_extended ts[3]);
VISIBLE int CubicSolve (const Spline1D *sp, bigreal sought, my_extended ts[3]);
/* Uses an algebraic solution */
my_extended SplineSolve (const Spline1D *sp, real tmin, real tmax,
                         my_extended sought_y);
/* Tries to fixup rounding errors that crept in to the solution */
my_extended SplineSolveFixup (const Spline1D *sp, real tmin, real tmax,
                              my_extended sought_y);
/* Uses an iterative approximation */
my_extended IterateSplineSolve (const Spline1D *sp, my_extended tmin,
                                my_extended tmax, my_extended sought_y);
/* Uses an iterative approximation and then tries to fix things up */
my_extended IterateSplineSolveFixup (const Spline1D *sp, my_extended tmin,
                                     my_extended tmax, my_extended sought_y);
void SplineFindExtrema (const Spline1D *sp, my_extended *_t1, my_extended *_t2);
int SSBoundsWithin (SplineSet *ss, bigreal z1, bigreal z2, bigreal *wmin,
                    bigreal *wmax, int major);
bigreal SplineMinDistanceToPoint (Spline *s, BasePoint *p);

SplineSet *SplineSetsInterpolate (SplineSet *base, SplineSet *other,
                                  real amount, SplineChar *sc);
SplineChar *SplineCharInterpolate (SplineChar *base, SplineChar *other,
                                   real amount, SplineFont *newfont);
VISIBLE SplineFont *InterpolateFont (SplineFont *base,
                                     SplineFont *other, real amount,
                                     Encoding *enc);

VISIBLE double SFSerifHeight (SplineFont *sf);

VISIBLE void DumpPfaEditEncodings (void);
VISIBLE char *ParseEncodingFile (char *filename, char *encodingname);
VISIBLE void LoadPfaEditEncodings (void);

int GenerateScript (SplineFont *sf, char *filename, char *bitmaptype,
                    int fmflags, int res, char *subfontdirectory,
                    struct sflist *sfs, EncMap *map,
                    NameList * rename_to, int layer);

void _SCAutoTrace (SplineChar *sc, int layer, char **args);
char **AutoTraceArgs (int ask);

#define CURVATURE_ERROR	-1e9
VISIBLE bigreal SplineCurvature (Spline *s, bigreal t);

double CheckExtremaForSingleBitErrors (const Spline1D *sp, double t,
                                       double othert);
VISIBLE int Spline2DFindExtrema (const Spline *sp, my_extended extrema[4]);
VISIBLE int Spline2DFindPointsOfInflection (const Spline *sp,
                                            my_extended poi[2]);
int SplineAtInflection (Spline1D *sp, bigreal t);
int SplineAtMinMax (Spline1D *sp, bigreal t);
void SplineRemoveExtremaTooClose (Spline1D *sp, my_extended *_t1,
                                  my_extended *_t2);
VISIBLE int NearSpline (struct findsel *fs, Spline *spline);
real SplineNearPoint (Spline *spline, BasePoint *bp, real fudge);
VISIBLE int SplineT2SpiroIndex (Spline *spline, bigreal t, SplineSet *spl);
VISIBLE void SCMakeDependent (SplineChar *dependent, SplineChar *base);
VISIBLE SplinePoint *SplineBisect (Spline *spline, my_extended t);
VISIBLE Spline *SplineSplit (Spline *spline, my_extended ts[3]);
VISIBLE Spline *ApproximateSplineFromPoints (SplinePoint *from,
                                             SplinePoint *to,
                                             TPoint * mid, int cnt, int order2);
VISIBLE Spline *ApproximateSplineFromPointsSlopes (SplinePoint *from,
                                                   SplinePoint *to,
                                                   TPoint * mid,
                                                   int cnt, int order2);
VISIBLE bigreal SplineLength (Spline *spline);
VISIBLE bigreal SplineLengthRange (Spline *spline, real from_t, real to_t);
VISIBLE bigreal PathLength (SplineSet *ss);
Spline *PathFindDistance (SplineSet *path, bigreal d, bigreal *_t);
VISIBLE SplineSet *SplineSetBindToPath (SplineSet *ss, int doscale,
                                        int glyph_as_unit, int align,
                                        real offset, SplineSet *path);
int SplineIsLinear (Spline *spline);
int SplineIsLinearMake (Spline *spline);
int SplineInSplineSet (Spline *spline, SplineSet *spl);
int SSPointWithin (SplineSet *spl, BasePoint *pt);
SplineSet *SSRemoveZeroLengthSplines (SplineSet *base);
void SSRemoveStupidControlPoints (SplineSet *base);
void SSOverlapClusterCpAngles (SplineSet *base, bigreal within);
void SplinesRemoveBetween (SplineChar *sc, SplinePoint *from,
                           SplinePoint *to, int type);
VISIBLE void SplineCharMerge (SplineChar *sc, SplineSet **head, int type);
void SPLNearlyHvCps (SplineChar *sc, SplineSet *ss, bigreal err);
void SPLNearlyHvLines (SplineChar *sc, SplineSet *ss, bigreal err);
int SPLNearlyLines (SplineChar *sc, SplineSet *ss, bigreal err);
VISIBLE int SPInterpolate (SplinePoint *sp);
void SplinePointListSimplify (SplineChar *sc, SplinePointList *spl,
                              struct simplifyinfo *smpl);
VISIBLE SplineSet *SplineCharSimplify (SplineChar *sc, SplineSet *head,
                                       struct simplifyinfo *smpl);
VISIBLE void SPLStartToLeftmost (SplineChar *sc, SplinePointList *spl,
                                 int *changed);
void SPLsStartToLeftmost (SplineChar *sc, int layer);
VISIBLE void CanonicalContours (SplineChar *sc, int layer);
VISIBLE void SplineSetJoinCpFixup (SplinePoint *sp);
VISIBLE SplineSet *SplineSetJoin (SplineSet *start, int doall,
                                  real fudge, int *changed);

enum ae_type
{
  ae_all,
  ae_between_selected,
  ae_only_good,
  ae_only_good_rm_later
};

VISIBLE int SpIsExtremum (SplinePoint *sp);
int Spline1DCantExtremeX (const Spline *s);
int Spline1DCantExtremeY (const Spline *s);
Spline *SplineAddExtrema (Spline *s, int always, real lenbound,
                          real offsetbound, DBounds *b);
VISIBLE void SplineSetAddExtrema (SplineChar *sc, SplineSet *ss,
                                  enum ae_type between_selected, int emsize);
void SplineSetAddSpiroExtrema (SplineChar *sc, SplineSet *ss,
                               enum ae_type between_selected, int emsize);
VISIBLE void SplineCharAddExtrema (SplineChar *sc, SplineSet *head,
                                   enum ae_type between_selected, int emsize);
SplineSet *SplineCharRemoveTiny (SplineChar *sc, SplineSet *head);
VISIBLE SplineFont *SplineFontNew (void);
VISIBLE SplineFont *SplineFontNew_long_form (Encoding *enc, int
                                             foreground_degree,
                                             int background_degree,
                                             int grid_degree);
VISIBLE char *GetNextUntitledName (void);
SplineFont *SplineFontEmpty (void);
VISIBLE SplineFont *SplineFontBlank (int charcnt);
void SFIncrementXUID (SplineFont *sf);
VISIBLE void SFRandomChangeXUID (SplineFont *sf);
VISIBLE SplineSet *SplineSetReverse (SplineSet *spl);
SplineSet *SplineSetsExtractOpen (SplineSet **tbase);
void SplineSetsInsertOpen (SplineSet **tbase, SplineSet *open);
VISIBLE SplineSet *SplineSetsCorrect (SplineSet *base, int *changed);
SplineSet *SplineSetsAntiCorrect (SplineSet *base);
VISIBLE SplineSet *SplineSetsDetectDir (SplineSet **_base, int *lastscan);
void SPAverageCps (SplinePoint *sp);
void SPLAverageCps (SplinePointList *spl);
void SPWeightedAverageCps (SplinePoint *sp);
VISIBLE void BP_HVForce (BasePoint *vector);
VISIBLE void SplineCharDefaultPrevCP (SplinePoint *base);
VISIBLE void SplineCharDefaultNextCP (SplinePoint *base);
VISIBLE void SplineCharTangentNextCP (SplinePoint *sp);
VISIBLE void SplineCharTangentPrevCP (SplinePoint *sp);
VISIBLE void SPAdjustControl (SplinePoint *sp, BasePoint *cp,
                              BasePoint *to, int order2);
VISIBLE void SPHVCurveForce (SplinePoint *sp);
void SPSmoothJoint (SplinePoint *sp);
VISIBLE int PointListIsSelected (SplinePointList *spl);
VISIBLE void SCSplinePointsUntick (SplineChar *sc, int layer);
void SplineSetsUntick (SplineSet *spl);
void SFOrderBitmapList (SplineFont *sf);
int KernThreshold (SplineFont *sf, int cnt);
VISIBLE real SFGuessItalicAngle (SplineFont *sf);

SplinePoint *SplineTtfApprox (Spline *ps);
VISIBLE SplineSet *SSttfApprox (SplineSet *ss);
VISIBLE SplineSet *SplineSetsTTFApprox (SplineSet *ss);
SplineSet *SSPSApprox (SplineSet *ss);
SplineSet *SplineSetsPSApprox (SplineSet *ss);
SplineSet *SplineSetsConvertOrder (SplineSet *ss, int to_order2);
VISIBLE void SplineRefigure2 (Spline *spline);
VISIBLE void SplineRefigureFixup (Spline *spline);
VISIBLE Spline *SplineMake2 (SplinePoint *from, SplinePoint *to);
VISIBLE Spline *SplineMake (SplinePoint *from, SplinePoint *to, int order2);
VISIBLE Spline *SFSplineMake (SplineFont *sf, SplinePoint *from,
                              SplinePoint *to);
void SCConvertToOrder2 (SplineChar *sc);
void SFConvertToOrder2 (SplineFont *sf);
void SCConvertToOrder3 (SplineChar *sc);
void SFConvertToOrder3 (SplineFont *sf);
VISIBLE void SFConvertGridToOrder2 (SplineFont *_sf);
void SCConvertLayerToOrder2 (SplineChar *sc, int layer);
VISIBLE void SFConvertLayerToOrder2 (SplineFont *sf, int layer);
VISIBLE void SFConvertGridToOrder3 (SplineFont *_sf);
void SCConvertLayerToOrder3 (SplineChar *sc, int layer);
VISIBLE void SFConvertLayerToOrder3 (SplineFont *sf, int layer);
void SCConvertOrder (SplineChar *sc, int to_order2);
VISIBLE void SplinePointPrevCPChanged2 (SplinePoint *sp);
VISIBLE void SplinePointNextCPChanged2 (SplinePoint *sp);
int IntersectLinesSlopes (BasePoint *inter, BasePoint *line1,
                          BasePoint *slope1, BasePoint *line2,
                          BasePoint *slope2);
int IntersectLines (BasePoint *inter, BasePoint *line1_1,
                    BasePoint *line1_2, BasePoint *line2_1, BasePoint *line2_2);
int IntersectLinesClip (BasePoint *inter, BasePoint *line1_1,
                        BasePoint *line1_2, BasePoint *line2_1,
                        BasePoint *line2_2);

#if 0
void SSBisectTurners (SplineSet *spl);
#endif
void SSRemoveBacktracks (SplineSet *ss);
VISIBLE enum PolyType PolygonIsConvex (BasePoint *poly, int n,
                                       int *badpointindex);
VISIBLE SplineSet *UnitShape (int isrect);
VISIBLE SplineSet *SplineSetStroke (SplineSet *spl, StrokeInfo *si, int order2);
VISIBLE SplineSet *SplineSetRemoveOverlap (SplineChar *sc,
                                           SplineSet *base, enum overlap_type);
VISIBLE SplineSet *SSShadow (SplineSet *spl, real angle,
                             real outline_width, real shadow_length,
                             SplineChar *sc, int wireframe);

double BlueScaleFigureForced (struct psdict *private_,
                              real bluevalues[], real otherblues[]);
double BlueScaleFigure (struct psdict *private_, real bluevalues[],
                        real otherblues[]);
void FindBlues (SplineFont *sf, int layer, real blues[14], real otherblues[10]);
VISIBLE void QuickBlues (SplineFont *sf, int layer, BlueData * bd);
void FindHStems (SplineFont *sf, real snaps[12], real cnt[12]);
void FindVStems (SplineFont *sf, real snaps[12], real cnt[12]);
double SFStdVW (SplineFont *sf);
VISIBLE int SplineCharIsFlexible (SplineChar *sc, int layer);
void SCGuessHintInstancesList (SplineChar *sc, int layer,
                               StemInfo * hstem, StemInfo * vstem,
                               DStemInfo * dstem, int hvforce, int dforce);
VISIBLE void SCGuessDHintInstances (SplineChar *sc, int layer, DStemInfo * ds);
VISIBLE void SCGuessHHintInstancesAndAdd (SplineChar *sc, int layer,
                                          StemInfo * stem, real guess1,
                                          real guess2);
VISIBLE void SCGuessVHintInstancesAndAdd (SplineChar *sc, int layer,
                                          StemInfo * stem, real guess1,
                                          real guess2);
void SCGuessHHintInstancesList (SplineChar *sc, int layer);
void SCGuessVHintInstancesList (SplineChar *sc, int layer);
real HIlen (StemInfo * stems);
real HIoverlap (HintInstance * mhi, HintInstance * thi);
int StemInfoAnyOverlaps (StemInfo * stems);
VISIBLE int StemListAnyConflicts (StemInfo * stems);
HintInstance *HICopyTrans (HintInstance * hi, real mul, real offset);
void MDAdd (SplineChar *sc, int x, SplinePoint *sp1, SplinePoint *sp2);
int SFNeedsAutoHint (SplineFont *_sf, int layer);

typedef struct bluezone
{
  real base;
  int cvtindex;
  real family_base;             /* NaN if none */
  int family_cvtindex;
  real overshoot;               /* relative to baseline, NOT to base */
  int highest;                  /* used in autoinstructing for HStem positioning */
  int lowest;                   /* as above */
} BlueZone;

typedef struct stdstem
{
  real width;                   /* -1 if none */
  int cvtindex;
  struct stdstem *snapto;       /* NULL means stem isn't snapped to any other */
  int stopat;                   /* at which ppem stop snapping to snapto */
} StdStem;

typedef struct globalinstrct
{
  SplineFont *sf;
  int layer;
  BlueData *bd;
  double fudge;

  /* Did we initialize the tables needed? 'maxp' is skipped because */
  /* its initialization always succeeds. */
  int cvt_done;
  int fpgm_done;
  int prep_done;

  /* PS private data with truetype-specific information added */
  BlueZone blues[12];           /* like in BlueData */
  int bluecnt;
  StdStem stdhw;
  StdStem *stemsnaph;           /* StdHW excluded */
  int stemsnaphcnt;
  StdStem stdvw;
  StdStem *stemsnapv;           /* StdVW excluded */
  int stemsnapvcnt;
} GlobalInstrCt;

VISIBLE void InitGlobalInstrCt (GlobalInstrCt * gic, SplineFont *sf,
                                int layer, BlueData * bd);
VISIBLE void FreeGlobalInstrCt (GlobalInstrCt * gic);
VISIBLE void NowakowskiSCAutoInstr (GlobalInstrCt * gic, SplineChar *sc);
void CVT_ImportPrivate (SplineFont *sf);

VISIBLE void SCModifyHintMasksAdd (SplineChar *sc, int layer, StemInfo * new_);
void SCClearHints (SplineChar *sc);
VISIBLE void SCClearHintMasks (SplineChar *sc, int layer, int counterstoo);
void SCFigureVerticalCounterMasks (SplineChar *sc);
VISIBLE void SCFigureCounterMasks (SplineChar *sc);
VISIBLE void SCFigureHintMasks (SplineChar *sc, int layer);
VISIBLE void _SplineCharAutoHint (SplineChar *sc, int layer,
                                  BlueData * bd, struct glyphdata *gd2,
                                  int gen_undoes);
VISIBLE void SplineCharAutoHint (SplineChar *sc, int layer, BlueData * bd);
void SFSCAutoHint (SplineChar *sc, int layer, BlueData * bd);
void SplineFontAutoHint (SplineFont *sf, int layer);
void SplineFontAutoHintRefs (SplineFont *sf, int layer);
StemInfo *HintCleanup (StemInfo * stem, int dosort, int instance_count);
int SplineFontIsFlexible (SplineFont *sf, int layer, int flags);
VISIBLE int SCDrawsSomething (SplineChar *sc);
VISIBLE int SCWorthOutputting (SplineChar *sc);
VISIBLE int SFFindNotdef (SplineFont *sf, int fixed);
int doesGlyphExpandHorizontally (SplineChar *sc);
int IsntBDFChar (BDFChar *bdfc);
int CIDWorthOutputting (SplineFont *cidmaster, int enc);        /* Returns -1 on failure, font number on success */
int AmfmSplineFont (FILE *afm, MMSet *mm, int formattype, EncMap *map,
                    int layer);
int AfmSplineFont (FILE *afm, SplineFont *sf, int formattype,
                   EncMap *map, int docc, SplineFont *fullsf, int layer);
int PfmSplineFont (FILE *pfm, SplineFont *sf, int type0, EncMap *map,
                   int layer);
int TfmSplineFont (FILE *afm, SplineFont *sf, int formattype,
                   EncMap *map, int layer);
int OfmSplineFont (FILE *afm, SplineFont *sf, int formattype,
                   EncMap *map, int layer);
char *EncodingName (Encoding *map);
VISIBLE char *SFEncodingName (SplineFont *sf, EncMap *map);
void SFLigaturePrepare (SplineFont *sf);
void SFLigatureCleanup (SplineFont *sf);
VISIBLE void SFKernClassTempDecompose (SplineFont *sf, int isv);
VISIBLE void SFKernCleanup (SplineFont *sf, int isv);
int SCSetMetaData (SplineChar *sc, char *name, int unienc, const char *comment);

enum uni_interp interp_from_encoding (Encoding *enc, enum uni_interp interp);
const char *EncName (Encoding *encname);
const char *FindUnicharName (void);
VISIBLE Encoding *_FindOrMakeEncoding (const char *name, int make_it);
VISIBLE Encoding *FindOrMakeEncoding (const char *name);
VISIBLE int SFDWrite (char *filename, SplineFont *sf, EncMap *map,
                      EncMap *normal, int todir);
VISIBLE int SFDWriteBak (SplineFont *sf, EncMap *map, EncMap *normal);
SplineFont *SFDRead (char *filename);
SplineFont *_SFDRead (char *filename, FILE *sfd);
SplineFont *SFDirRead (char *filename);
VISIBLE SplineChar *SFDReadOneChar (SplineFont *sf, const char *name);
char *TTFGetFontName (FILE *ttf, int32_t offset, int32_t off2);
void TTFLoadBitmaps (FILE *ttf, struct ttfinfo *info, int onlyone);

enum ttfflags
{
  ttf_onlystrikes = 1,
  ttf_onlyonestrike = 2,
  ttf_onlykerns = 4,
  ttf_onlynames = 8
};

SplineFont *_SFReadWOFF (FILE *woff, int flags,
                         enum openflags openflags, char *filename,
                         struct fontdict *fd);
SplineFont *_SFReadTTF (FILE *ttf, int flags, enum openflags openflags,
                        char *filename, struct fontdict *fd);
SplineFont *SFReadTTF (char *filename, int flags, enum openflags openflags);
SplineFont *SFReadSVG (char *filename, int flags);
SplineFont *SFReadSVGMem (char *data, int flags);
SplineFont *SFReadUFO (char *filename, int flags);
SplineFont *_CFFParse (FILE *temp, int len, char *fontsetname);
SplineFont *CFFParse (char *filename);
SplineFont *SFReadMacBinary (char *filename, int flags,
                             enum openflags openflags);
SplineFont *SFReadWinFON (char *filename, int toback);
SplineFont *SFReadPalmPdb (char *filename, int toback);
VISIBLE SplineFont *LoadSplineFont (const char *filename, enum openflags);
VISIBLE SplineFont *_ReadSplineFont (FILE *file, const char *filename,
                                     enum openflags openflags);
SplineFont *ReadSplineFont (const char *filename, enum openflags);      /* Don't use this; use LoadSplineFont instead. */
VISIBLE FILE *URLToTempFile (char *url, void *lock);
int URLFromFile (char *url, FILE *from);
void ArchiveCleanup (char *archivedir);
char *Unarchive (char *name, char **_archivedir);
char *Decompress (char *name, int compression);
SplineFont *SFFromBDF (char *filename, int ispk, int toback);
SplineFont *SFFromMF (char *filename);
void SFCheckPSBitmap (SplineFont *sf);
VISIBLE uint16_t _MacStyleCode (char *styles, SplineFont *sf,
                                uint16_t *psstyle);
VISIBLE uint16_t MacStyleCode (SplineFont *sf, uint16_t *psstyle);
SplineFont *SFReadIkarus (char *fontname);
SplineFont *_SFReadPdfFont (FILE *ttf, char *filename,
                            enum openflags openflags);
SplineFont *SFReadPdfFont (char *filename, enum openflags openflags);
VISIBLE char **GetFontNames (char *filename);
char **NamesReadPDF (char *filename);
char **NamesReadSFD (char *filename);
char **NamesReadTTF (char *filename);
char **NamesReadCFF (char *filename);
char **NamesReadPostScript (char *filename);
char **_NamesReadPostScript (FILE *ps);
char **NamesReadSVG (char *filename);
char **NamesReadUFO (char *filename);
char **NamesReadMacBinary (char *filename);

void SFSetOrder (SplineFont *sf, int order2);
int SFFindOrder (SplineFont *sf);

VISIBLE const char *UnicodeRange (int unienc);
VISIBLE SplineChar *SCBuildDummy (SplineChar *dummy, SplineFont *sf,
                                  EncMap *map, int i);
VISIBLE SplineChar *SFMakeChar (SplineFont *sf, EncMap *map, int i);
char *AdobeLigatureFormat (char *name);
uint32_t LigTagFromUnicode (int uni);
void SCLigCaretheck (SplineChar *sc, int clean);
VISIBLE BDFChar *BDFMakeGID (BDFFont *bdf, int gid);
VISIBLE BDFChar *BDFMakeChar (BDFFont *bdf, EncMap *map, int enc);

VISIBLE RefChar *RefCharsCopyState (SplineChar *sc, int layer);
int SCWasEmpty (SplineChar *sc, int skip_this_layer);
VISIBLE void SCUndoSetLBearingChange (SplineChar *sc, int lb);
VISIBLE Undoes *SCPreserveHints (SplineChar *sc, int layer);
VISIBLE Undoes *SCPreserveLayer (SplineChar *sc, int layer, int dohints);
Undoes *_SCPreserveLayer (SplineChar *sc, int layer, int dohints);
VISIBLE Undoes *SCPreserveState (SplineChar *sc, int dohints);
VISIBLE Undoes *SCPreserveBackground (SplineChar *sc);
Undoes *SFPreserveGuide (SplineFont *sf);
Undoes *_SFPreserveGuide (SplineFont *sf);
VISIBLE Undoes *SCPreserveWidth (SplineChar *sc);
VISIBLE Undoes *SCPreserveVWidth (SplineChar *sc);
VISIBLE Undoes *BCPreserveState (BDFChar *bc);
VISIBLE void BCDoRedo (BDFChar *bc);
VISIBLE void BCDoUndo (BDFChar *bc);

int isaccent (int uni);
VISIBLE int SFIsCompositBuildable (SplineFont *sf, int unicodeenc,
                                   SplineChar *sc, int layer);
VISIBLE int SFIsSomethingBuildable (SplineFont *sf, SplineChar *sc,
                                    int layer, int onlyaccents);
VISIBLE int SFIsRotatable (SplineFont *sf, SplineChar *sc, int layer);
/*int SCMakeDotless(SplineFont *sf, SplineChar *dotless, int layer, int copybmp, int doit);*/
VISIBLE void SCBuildComposit (SplineFont *sf, SplineChar *sc,
                              int layer, BDFFont *bmp, int disp_only);
int SCAppendAccent (SplineChar *sc, int layer, char *glyph_name,
                    int uni, uint32_t pos);
VISIBLE const uint32_t *SFGetAlternate (SplineFont *sf, int base,
                                        SplineChar *sc, int nocheck);

int getAdobeEnc (char *name);

VISIBLE void SFSplinesFromLayers (SplineFont *sf, int tostroke);
VISIBLE void SFSetLayerWidthsStroked (SplineFont *sf, real strokewidth);
SplineSet *SplinePointListInterpretSVG (char *filename, char *memory,
                                        int memlen, int em_size,
                                        int ascent, int stroked);
SplineSet *SplinePointListInterpretGlif (char *filename, char *memory,
                                         int memlen, int em_size,
                                         int ascent, int stroked);
#define UNDEFINED_WIDTH	-999999
SplinePointList *SplinePointListInterpretPS (FILE *ps, int flags,
                                             int stroked, int *width);
void PSFontInterpretPS (FILE *ps, struct charprocs *cp, char **encoding);
struct enc *PSSlurpEncodings (FILE *file);
VISIBLE int EvaluatePS (char *str, real *stack, int size);

struct pscontext
{
  int is_type2;
  int painttype;
  int instance_count;
  real blend_values[17];
  int blend_warn;
};

int UnblendedCompare (real u1[MmMax], real u2[MmMax], int cnt);
SplineChar *PSCharStringToSplines (uint8_t *type1, int len,
                                   struct pscontext *context,
                                   struct pschars *subrs,
                                   struct pschars *gsubrs, const char *name);
void MatMultiply (real m1[6], real m2[6], real to[6]);
int MatIsIdentity (real transform[6]);

VISIBLE int NameToEncoding (SplineFont *sf, EncMap *map, const char *uname);
VISIBLE void GlyphHashFree (SplineFont *sf);
void SFHashGlyph (SplineFont *sf, SplineChar *sc);
VISIBLE SplineChar *SFHashName (SplineFont *sf, const char *name);
VISIBLE int SFFindGID (SplineFont *sf, int unienc, const char *name);
VISIBLE int SFFindSlot (SplineFont *sf, EncMap *map, int unienc,
                        const char *name);
int SFCIDFindCID (SplineFont *sf, int unienc, const char *name);
VISIBLE SplineChar *SFGetChar (SplineFont *sf, int unienc, const char *name);
int SFHasChar (SplineFont *sf, int unienc, const char *name);
SplineChar *SFGetOrMakeChar (SplineFont *sf, int unienc, const char *name);
VISIBLE int SFFindExistingSlot (SplineFont *sf, int unienc, const char *name);
int SFCIDFindExistingChar (SplineFont *sf, int unienc, const char *name);
int SFHasCID (SplineFont *sf, int cid);

VISIBLE char *getUserCacheDir (void);
VISIBLE char *getUserConfigDir (void);
VISIBLE char *getUserDataDir (void);

VISIBLE void _DoAutoSaves (struct fontviewbase *);
VISIBLE void CleanAutoRecovery (void);
VISIBLE int DoAutoRecovery (int);
SplineFont *SFRecoverFile (char *autosavename, int inquire, int *state);
void SFAutoSave (SplineFont *sf, EncMap *map);
VISIBLE void SFClearAutoSave (SplineFont *sf);

void PSCharsFree (struct pschars *chrs);
void PSDictFree (struct psdict *chrs);
struct psdict *PSDictCopy (struct psdict *dict);
int PSDictFindEntry (struct psdict *dict, const char *key);
const char *PSDictHasEntry (struct psdict *dict, const char *key);
bool PSDictSame (struct psdict *dict1, struct psdict *dict2);
int PSDictRemoveEntry (struct psdict *dict, const char *key);
int PSDictChangeEntry (struct psdict *dict, const char *key,
                       const char *newval);
VISIBLE int SFPrivateGuess (SplineFont *sf, int layer,
                            struct psdict *private_, char *name, int onlyone);

VISIBLE void SFRemoveLayer (SplineFont *sf, int l);
VISIBLE void SFAddLayer (SplineFont *sf, char *name, int order2,
                         int background);
VISIBLE void SFLayerSetBackground (SplineFont *sf, int layer, int is_back);

VISIBLE void SplineSetsRound2Int (SplineSet *spl, real factor,
                                  int inspiro, int onlysel);
VISIBLE void SCRound2Int (SplineChar *sc, int layer, real factor);
VISIBLE int SCRoundToCluster (SplineChar *sc, int layer, int sel,
                              bigreal within, bigreal max);
int SplineSetsRemoveAnnoyingExtrema (SplineSet *ss, bigreal err);
VISIBLE int hascomposing (SplineFont *sf, int u, SplineChar *sc);
#if 0
void SFFigureGrid (SplineFont *sf);
#endif

struct cidmap;                  /* private structure to encoding.c */
VISIBLE int CIDFromName (char *name, SplineFont *cidmaster);
VISIBLE int CID2Uni (struct cidmap *map, int cid);
int CID2NameUni (struct cidmap *map, int cid, char *buffer, int len);
int NameUni2CID (struct cidmap *map, int uni, const char *name);
struct altuni *CIDSetAltUnis (struct cidmap *map, int cid);
VISIBLE int MaxCID (struct cidmap *map);
VISIBLE struct cidmap *LoadMapFromFile (char *file, char *registry,
                                        char *ordering, int supplement);
VISIBLE struct cidmap *FindCidMap (char *registry, char *ordering,
                                   int supplement, SplineFont *sf);
VISIBLE void SFEncodeToMap (SplineFont *sf, struct cidmap *map);
SplineFont *CIDFlatten (SplineFont *cidmaster, SplineChar **chars, int charcnt);
VISIBLE void SFFlatten (SplineFont *cidmaster);
VISIBLE int SFFlattenByCMap (SplineFont *sf, char *cmapname);
VISIBLE SplineFont *MakeCIDMaster (SplineFont *sf, EncMap *oldmap,
                                   int bycmap, char *cmapfilename,
                                   struct cidmap *cidmap);

int getushort (FILE *ttf);
int32_t getlong (FILE *ttf);
int get3byte (FILE *ttf);
real getfixed (FILE *ttf);
real get2dot14 (FILE *ttf);
void putshort (FILE *file, int sval);
void putlong (FILE *file, int val);
void putfixed (FILE *file, real dval);
int ttfcopyfile (FILE *ttf, FILE *other, int pos, char *table_name);

VISIBLE void SCCopyLayerToLayer (SplineChar *sc, int from, int to, int doclear);

VISIBLE bool hasFreeType (void);
VISIBLE bool hasFreeTypeDebugger (void);
VISIBLE bool hasFreeTypeByteCode (void);
VISIBLE char *FreeTypeStringVersion (void);
void doneFreeType (void);
VISIBLE void *_FreeTypeFontContext (SplineFont *sf, SplineChar *sc,
                                    struct fontviewbase *fv, int layer,
                                    enum fontformat ff, int flags,
                                    void *shared_ftc);
VISIBLE void *FreeTypeFontContext (SplineFont *sf, SplineChar *sc,
                                   struct fontviewbase *fv, int layer);
VISIBLE BDFFont *SplineFontFreeTypeRasterize (void *freetypecontext,
                                              int pixelsize, int depth);
VISIBLE BDFChar *SplineCharFreeTypeRasterize (void *freetypecontext,
                                              int gid, int ptsize,
                                              int dpi, int depth);
VISIBLE void FreeTypeFreeContext (void *freetypecontext);
VISIBLE SplineSet *FreeType_GridFitChar (void *single_glyph_context,
                                         int enc, real ptsizey,
                                         real ptsizex, int dpi,
                                         uint16_t *width,
                                         SplineChar *sc, int depth, int scaled);
VISIBLE struct freetype_raster *FreeType_GetRaster (void
                                                    *single_glyph_context,
                                                    int enc,
                                                    real ptsizey,
                                                    real ptsizex,
                                                    int dpi, int depth);
VISIBLE BDFChar *SplineCharFreeTypeRasterizeNoHints (SplineChar *sc,
                                                     int layer,
                                                     int ptsize,
                                                     int dpi, int depth);
VISIBLE BDFFont *SplineFontFreeTypeRasterizeNoHints (SplineFont *sf,
                                                     int layer,
                                                     int pixelsize, int depth);
VISIBLE void FreeType_FreeRaster (struct freetype_raster *raster);

struct TT_ExecContextRec_;

struct freetype_raster *DebuggerCurrentRaster (struct
                                               TT_ExecContextRec_ *exc,
                                               int depth);

VISIBLE int UniFromName (const char *name, enum uni_interp interp,
                         Encoding *encname);
VISIBLE const char *StdGlyphName (char *buffer, int uni,
                                  enum uni_interp interp,
                                  NameList * for_this_font);
VISIBLE char **AllGlyphNames (int uni, NameList * for_this_font,
                              SplineChar *sc /* May be NULL */ );
VISIBLE char **AllNamelistNames (void);
VISIBLE NameList *DefaultNameListForNewFonts (void);
VISIBLE NameList *NameListByName (char *name);
VISIBLE NameList *LoadNamelist (char *filename);
VISIBLE void LoadNamelistDir (char *dir);
VISIBLE void SFRenameGlyphsToNamelist (SplineFont *sf, NameList * new_);
VISIBLE char **SFTemporaryRenameGlyphsToNamelist (SplineFont *sf,
                                                  NameList * new_);
VISIBLE void SFTemporaryRestoreGlyphNames (SplineFont *sf, char **former);

AnchorPos *AnchorPositioning (SplineChar *sc, uint32_t *ustr,
                              SplineChar **sstr);
void AnchorPosFree (AnchorPos * apos);

int SF_CloseAllInstrs (SplineFont *sf);
int SSTtfNumberPoints (SplineSet *ss);
VISIBLE int SCNumberPoints (SplineChar *sc, int layer);
VISIBLE int SCPointsNumberedProperly (SplineChar *sc, int layer);
VISIBLE int ttfFindPointInSC (SplineChar *sc, int layer, int pnum,
                              BasePoint *pos, RefChar *bound);

int SFFigureDefWidth (SplineFont *sf, int *_nomwid);

int SFRenameTheseFeatureTags (SplineFont *sf, uint32_t tag, int sli,
                              int flags, uint32_t totag, int tosli,
                              int toflags, int ismac);
int SFRemoveUnusedNestedFeatures (SplineFont *sf);

char *utf8_verify_copy (const char *str);

VISIBLE char *MacStrToUtf8 (const char *str, int macenc, int maclang);
VISIBLE char *Utf8ToMacStr (const char *ustr, int macenc, int maclang);
VISIBLE uint8_t MacEncFromMacLang (int maclang);
uint16_t WinLangFromMac (int maclang);
uint16_t WinLangToMac (int winlang);
int CanEncodingWinLangAsMac (int winlang);
const int32_t *MacEncToUnicode (int script, int lang);
int MacLangFromLocale (void);
char *MacLanguageFromCode (int code);

VISIBLE int32_t UniFromEnc (int enc, Encoding *encname);
VISIBLE int32_t EncFromUni (int32_t uni, Encoding *encname);
int32_t EncFromName (const char *name, enum uni_interp interp,
                     Encoding *encname);

void MatInverse (real into[6], real orig[6]);

VISIBLE int BpCollinear (BasePoint *first, BasePoint *mid, BasePoint *last);
int BpWithin (BasePoint *first, BasePoint *mid, BasePoint *last);
    /* Collinear & between */

enum psstrokeflags
{
  /* sf_removeoverlap=2, */
  sf_handle_eraser = 4,
  sf_correctdir = 8,
  sf_clearbeforeinput = 16
};

char *MMAxisAbrev (char *axis_name);
char *MMMakeMasterFontname (MMSet *mm, int ipos, char **fullname);
char *MMGuessWeight (MMSet *mm, int ipos, char *def);
char *MMExtractNth (char *pt, int ipos);
char *MMExtractArrayNth (char *pt, int ipos);
VISIBLE int MMValid (MMSet *mm, int complain);
VISIBLE void MMKern (SplineFont *sf, SplineChar *first,
                     SplineChar *second, int diff,
                     struct lookup_subtable *sub, KernPair *oldkp);
VISIBLE char *MMBlendChar (MMSet *mm, int gid);

char *EnforcePostScriptName (char *old);

enum Compare_Ret
{
  SS_DiffContourCount = 1,
  SS_MismatchOpenClosed = 2,
  SS_DisorderedContours = 4,
  SS_DisorderedStart = 8,
  SS_DisorderedDirection = 16,
  SS_PointsMatch = 32,
  SS_ContourMatch = 64,
  SS_NoMatch = 128,
  SS_RefMismatch = 256,
  SS_WidthMismatch = 512,
  SS_VWidthMismatch = 1024,
  SS_HintMismatch = 2048,
  SS_HintMaskMismatch = 4096,
  SS_LayerCntMismatch = 8192,
  SS_ContourMismatch = 16384,
  SS_UnlinkRefMatch = 32768,

  BC_DepthMismatch = 1 << 16,
  BC_BoundingBoxMismatch = 2 << 16,
  BC_BitmapMismatch = 4 << 16,
  BC_NoMatch = 8 << 16,
  BC_Match = 16 << 16,

  SS_RefPtMismatch = 32 << 16
};

enum Compare_Ret BitmapCompare (BDFChar *bc1, BDFChar *bc2, int err,
                                int bb_err);
enum Compare_Ret SSsCompare (const SplineSet *ss1,
                             const SplineSet *ss2, real pt_err,
                             real spline_err, SplinePoint **hmfail);

enum font_compare_flags
{
  fcf_outlines = 1,
  fcf_exact = 2,
  fcf_warn_not_exact = 4,
  fcf_hinting = 8,
  fcf_hintmasks = 0x10,
  fcf_hmonlywithconflicts = 0x20,
  fcf_warn_not_ref_exact = 0x40,
  fcf_bitmaps = 0x80,
  fcf_names = 0x100,
  fcf_gpos = 0x200,
  fcf_gsub = 0x400,
  fcf_adddiff2sf1 = 0x800,
  fcf_addmissing = 0x1000
};

VISIBLE int CompareFonts (SplineFont *sf1, EncMap *map1,
                          SplineFont *sf2, FILE *diffs, int flags);
VISIBLE int LayersSimilar (Layer *ly1, Layer *ly2, double spline_err);

void DefaultOtherSubrs (void);
VISIBLE int ReadOtherSubrsFile (char *filename);

char *utf8toutf7_copy (const char *_str);
char *utf7toutf8_copy (const char *_str);

VISIBLE void SFSetModTime (SplineFont *sf);
void SFTimesFromFile (SplineFont *sf, FILE *);

VISIBLE int SFHasInstructions (SplineFont *sf);
int RefDepth (RefChar *ref, int layer);

SplineChar *SCHasSubs (SplineChar *sc, uint32_t tag);

VISIBLE char *TagFullName (SplineFont *sf, uint32_t tag, int onlyifknown);

VISIBLE uint32_t *SFScriptsInLookups (SplineFont *sf, int gpos);
VISIBLE uint32_t *SFLangsInScript (SplineFont *sf, int gpos, uint32_t script);
VISIBLE uint32_t *SFFeaturesInScriptLang (SplineFont *sf, int gpos,
                                          uint32_t script, uint32_t lang);
OTLookup **SFLookupsInScriptLangFeature (SplineFont *sf, int gpos,
                                         uint32_t script,
                                         uint32_t lang, uint32_t feature);
SplineChar **SFGlyphsWithPSTinSubtable (SplineFont *sf,
                                        struct lookup_subtable *subtable);
SplineChar **SFGlyphsWithLigatureinLookup (SplineFont *sf,
                                           struct lookup_subtable *subtable);
VISIBLE void SFFindUnusedLookups (SplineFont *sf);
VISIBLE void SFFindClearUnusedLookupBits (SplineFont *sf);
VISIBLE int LookupUsedNested (SplineFont *sf, OTLookup *checkme);
void SFRemoveUnusedLookupSubTables (SplineFont *sf,
                                    int
                                    remove_incomplete_anchorclasses,
                                    int remove_unused_lookups);
VISIBLE void SFRemoveLookupSubTable (SplineFont *sf,
                                     struct lookup_subtable *sub);
VISIBLE void SFRemoveLookup (SplineFont *sf, OTLookup *otl);
VISIBLE struct lookup_subtable *SFFindLookupSubtable (SplineFont *sf,
                                                      char *name);
struct lookup_subtable *SFFindLookupSubtableAndFreeName (SplineFont
                                                         *sf, char *name);
VISIBLE OTLookup *SFFindLookup (SplineFont *sf, char *name);
VISIBLE void NameOTLookup (OTLookup *otl, SplineFont *sf);
VISIBLE int GlyphNameCnt (const char *pt);
char *reverseGlyphNames (char *str);
VISIBLE char *FPSTRule_From_Str (SplineFont *sf, FPST *fpst,
                                 struct fpst_rule *rule, char *line,
                                 int *return_is_warning);
VISIBLE char *FPSTRule_To_Str (SplineFont *sf, FPST *fpst,
                               struct fpst_rule *rule);
void FListAppendScriptLang (FeatureScriptLangList *fl,
                            uint32_t script_tag, uint32_t lang_tag);
void FListsAppendScriptLang (FeatureScriptLangList *fl,
                             uint32_t script_tag, uint32_t lang_tag);
struct scriptlanglist *SLCopy (struct scriptlanglist *sl);
struct scriptlanglist *SListCopy (struct scriptlanglist *sl);
FeatureScriptLangList *FeatureListCopy (FeatureScriptLangList *fl);
void SLMerge (FeatureScriptLangList *into, struct scriptlanglist *fsl);
VISIBLE void FLMerge (OTLookup *into, OTLookup *from);
VISIBLE FeatureScriptLangList *FLOrder (FeatureScriptLangList *fl);
VISIBLE int FeatureScriptTagInFeatureScriptList (uint32_t tag,
                                                 uint32_t script,
                                                 FeatureScriptLangList *fl);
VISIBLE FeatureScriptLangList
  *FindFeatureTagInFeatureScriptList (uint32_t tag, FeatureScriptLangList *fl);
VISIBLE int FeatureTagInFeatureScriptList (uint32_t tag,
                                           FeatureScriptLangList *fl);
int DefaultLangTagInOneScriptList (struct scriptlanglist *sl);
struct scriptlanglist *DefaultLangTagInScriptList (struct
                                                   scriptlanglist *sl,
                                                   int DFLT_ok);
VISIBLE int ScriptInFeatureScriptList (uint32_t script,
                                       FeatureScriptLangList *fl);
VISIBLE int _FeatureOrderId (int isgpos, uint32_t tag);
VISIBLE int FeatureOrderId (int isgpos, FeatureScriptLangList *fl);
VISIBLE void SFSubTablesMerge (SplineFont *_sf,
                               struct lookup_subtable *subfirst,
                               struct lookup_subtable *subsecond);
struct lookup_subtable *SFSubTableFindOrMake (SplineFont *sf,
                                              uint32_t tag,
                                              uint32_t script, int lookup_type);
struct lookup_subtable *SFSubTableMake (SplineFont *sf, uint32_t tag,
                                        uint32_t script, int lookup_type);
OTLookup *OTLookupCopyInto (SplineFont *into_sf, SplineFont *from_sf,
                            OTLookup *from_otl);
VISIBLE void OTLookupsCopyInto (SplineFont *into_sf,
                                SplineFont *from_sf,
                                OTLookup **from_list, OTLookup *before);
VISIBLE struct opentype_str *ApplyTickedFeatures (SplineFont *sf,
                                                  uint32_t *flist,
                                                  uint32_t script,
                                                  uint32_t lang,
                                                  int pixelsize,
                                                  SplineChar **glyphs);
VISIBLE int VerticalKernFeature (SplineFont *sf, OTLookup *otl, int ask);
VISIBLE void SFGlyphRenameFixup (SplineFont *sf,
                                 char *old_name,
                                 char *new_name, bool rename_related_glyphs);

struct sllk
{
  uint32_t script;
  int cnt, max;
  OTLookup **lookups;
  int lcnt, lmax;
  uint32_t *langs;
};

VISIBLE void SllkFree (struct sllk *sllk, int sllk_cnt);
VISIBLE struct sllk *AddOTLToSllks (OTLookup *otl, struct sllk *sllk,
                                    int *_sllk_cnt, int *_sllk_max);
VISIBLE OTLookup *NewAALTLookup (SplineFont *sf, struct sllk *sllk,
                                 int sllk_cnt, int i);
void AddNewAALTFeatures (SplineFont *sf);

VISIBLE void SplinePointRound (SplinePoint *, real);

VISIBLE int KCFindName (char *name, char **classnames, int cnt,
                        int allow_class0);
KernClass *SFFindKernClass (SplineFont *sf, SplineChar *first,
                            SplineChar *last, int *index, int allow_zero);
KernClass *SFFindVKernClass (SplineFont *sf, SplineChar *first,
                             SplineChar *last, int *index, int allow_zero);

void SCClearRounds (SplineChar *sc, int layer);
VISIBLE void SCSynchronizeWidth (SplineChar *sc, real newwidth,
                                 real oldwidth, struct fontviewbase *fv);
VISIBLE RefChar *HasUseMyMetrics (SplineChar *sc, int layer);
VISIBLE void SCSynchronizeLBearing (SplineChar *sc, real off, int layer);
VISIBLE void RevertedGlyphReferenceFixup (SplineChar *sc, SplineFont *sf);

VISIBLE void SFUntickAll (SplineFont *sf);

VISIBLE void BDFOrigFixup (BDFFont *bdf, int orig_cnt, SplineFont *sf);

VISIBLE void SCImportSVG (SplineChar *sc, int layer, char *path,
                          char *memory, int memlen, int doclear);
VISIBLE void SCImportGlif (SplineChar *sc, int layer, char *path,
                           char *memory, int memlen, int doclear);
void SCImportPS (SplineChar *sc, int layer, char *path, int doclear, int flags);
VISIBLE void SCImportPSFile (SplineChar *sc, int layer, FILE *ps,
                             int doclear, int flags);
void SCImportPDF (SplineChar *sc, int layer, char *path, int doclear,
                  int flags);
VISIBLE void SCImportPDFFile (SplineChar *sc, int layer, FILE *ps,
                              int doclear, int flags);
VISIBLE void SCImportPlateFile (SplineChar *sc, int layer, FILE *plate,
                                int doclear, int flags);
VISIBLE void SCAddScaleImage (SplineChar *sc, struct gimage *image,
                              int doclear, int layer);
void SCInsertImage (SplineChar *sc, struct gimage *image, real scale,
                    real yoff, real xoff, int layer);
VISIBLE void SCImportFig (SplineChar *sc, int layer, char *path, int doclear);

VISIBLE int _ExportPlate (FILE *pdf, SplineChar *sc, int layer);
VISIBLE int _ExportPDF (FILE *pdf, SplineChar *sc, int layer);
VISIBLE int _ExportEPS (FILE *eps, SplineChar *sc, int layer, int gen_preview);
VISIBLE int _ExportSVG (FILE *svg, SplineChar *sc, int layer);
VISIBLE int _ExportGlif (FILE *glif, SplineChar *sc, int layer);
VISIBLE int ExportEPS (char *filename, SplineChar *sc, int layer);
VISIBLE int ExportPDF (char *filename, SplineChar *sc, int layer);
VISIBLE int ExportPlate (char *filename, SplineChar *sc, int layer);
VISIBLE int ExportSVG (char *filename, SplineChar *sc, int layer);
VISIBLE int ExportGlif (char *filename, SplineChar *sc, int layer);
VISIBLE int ExportFig (char *filename, SplineChar *sc, int layer);
VISIBLE int BCExportXBM (char *filename, BDFChar *bdfc, int format);
VISIBLE int ExportImage (char *filename, SplineChar *sc, int layer,
                         int format, int pixelsize, int bitsperpixel);
void ScriptExport (SplineFont *sf, BDFFont *bdf, int format, int gid,
                   char *format_spec, EncMap *map);

VISIBLE EncMap *EncMapFromEncoding (SplineFont *sf, Encoding *enc);
void SFRemoveGlyph (SplineFont *sf, SplineChar *sc, int *flags);
void SFAddEncodingSlot (SplineFont *sf, int gid);
VISIBLE void SFAddGlyphAndEncode (SplineFont *sf, SplineChar *sc,
                                  EncMap *basemap, int baseenc);
VISIBLE void SCDoRedo (SplineChar *sc, int layer);
VISIBLE void SCDoUndo (SplineChar *sc, int layer);
VISIBLE void SCCopyWidth (SplineChar *sc, enum undotype);
void SCAppendPosSub (SplineChar *sc, enum possub_type type, char **d,
                     SplineFont *copied_from);
VISIBLE void SCClearBackground (SplineChar *sc);
VISIBLE void BackgroundImageTransform (SplineChar *sc, ImageList *img,
                                       real transform[6]);
VISIBLE int SFIsDuplicatable (SplineFont *sf, SplineChar *sc);

VISIBLE void DoAutoSaves (void);

void SCClearLayer (SplineChar *sc, int layer);
VISIBLE void SCClearContents (SplineChar *sc, int layer);
VISIBLE void SCClearAll (SplineChar *sc, int layer);
VISIBLE void BCClearAll (BDFChar *bc);

#if !defined(_NO_PYTHON)
void FontForge_PythonInit (void);
void PyFF_ErrorString (const char *msg, const char *str);
void PyFF_ErrorF3 (const char *frmt, const char *str, int size, int depth);
VISIBLE void PyFF_Stdin (void);
void PyFF_Main (int argc, char **argv, int start);
void PyFF_ScriptFile (struct fontviewbase *fv, SplineChar *sc, char *filename);
VISIBLE void PyFF_ScriptString (struct fontviewbase *fv,
                                SplineChar *sc, int layer, char *str);
VISIBLE void PyFF_FreeFV (struct fontviewbase *fv);
void PyFF_FreeSC (SplineChar *sc);
void PyFF_FreeSF (SplineFont *sf);
VISIBLE void PyFF_ProcessInitFiles (void);
char *PyFF_PickleMeToString (void *pydata);
void *PyFF_UnPickleMeToObjects (char *str);
struct _object;                 /* Python Object */
void PyFF_CallDictFunc (struct _object *dict, char *key, char *argtypes, ...);
VISIBLE void ff_init (void);
struct _object *ff_init_py3 (int);
#endif
void doinitFontForgeMain (void);

VISIBLE void InitSimpleStuff (void);

VISIBLE int SSExistsInLayer (SplineSet *ss, SplineSet *lots);
VISIBLE int SplineExistsInSS (Spline *s, SplineSet *ss);
int SpExistsInSS (SplinePoint *sp, SplineSet *ss);

VISIBLE int MSLanguageFromLocale (void);

VISIBLE extern struct math_constants_descriptor
{
  char *ui_name;
  char *script_name;
  int offset;
  int devtab_offset;
  char *message;
  int new_page;
} math_constants_descriptor[];

#define MATH_CONSTANTS_DESCRIPTOR_EMPTY { NULL, NULL, 0, 0, NULL, 0 }

VISIBLE extern const char *knownweights[], *realweights[], **noticeweights[];

VISIBLE int BPTooFar (BasePoint *bp1, BasePoint *bp2);
VISIBLE StemInfo *SCHintOverlapInMask (SplineChar *sc, HintMask * hm);
char *VSErrorsFromMask (int mask, int private_mask);
VISIBLE int SCValidate (SplineChar *sc, int layer, int force);
VISIBLE AnchorClass *SCValidateAnchors (SplineChar *sc);
VISIBLE void SCTickValidationState (SplineChar *sc, int layer);
VISIBLE int ValidatePrivate (SplineFont *sf);
VISIBLE int SFValidate (SplineFont *sf, int layer, int force);
VISIBLE int VSMaskFromFormat (SplineFont *sf, int layer,
                              enum fontformat format);

SplineSet *SpiroCP2SplineSet (spiro_cp *spiros);
VISIBLE spiro_cp *SplineSet2SpiroCP (SplineSet *ss, uint16_t *_cnt);
spiro_cp *SpiroCPCopy (spiro_cp *spiros, uint16_t *_cnt);
VISIBLE void SSRegenerateFromSpiros (SplineSet *spl);

struct lang_frequencies;

VISIBLE uint32_t *PrtBuildDef (SplineFont *sf, void *tf,
                               void (*langsyscallback) (void *tf,
                                                        int end,
                                                        uint32_t
                                                        script, uint32_t lang));
VISIBLE char *RandomParaFromScriptLang (uint32_t script, uint32_t lang,
                                        SplineFont *sf,
                                        struct lang_frequencies *freq);
char *RandomParaFromScript (uint32_t script, uint32_t *lang, SplineFont *sf);
int SF2Scripts (SplineFont *sf, uint32_t scripts[100]);
VISIBLE char **SFScriptLangs (SplineFont *sf, struct lang_frequencies ***freq);

int SSHasClip (SplineSet *ss);
int SSHasDrawn (SplineSet *ss);
VISIBLE struct gradient *GradientCopy (struct gradient *old, real transform[6]);
VISIBLE void GradientFree (struct gradient *grad);
VISIBLE struct pattern *PatternCopy (struct pattern *old, real transform[6]);
VISIBLE void PatternFree (struct pattern *pat);
void BrushCopy (struct brush *into, struct brush *from, real transform[6]);
void PenCopy (struct pen *into, struct pen *from, real transform[6]);
VISIBLE void PatternSCBounds (SplineChar *sc, DBounds *b);

VISIBLE char *SFDefaultImage (SplineFont *sf, char *filename);
void SCClearInstrsOrMark (SplineChar *sc, int layer, int complain);
VISIBLE void instrcheck (SplineChar *sc, int layer);
VISIBLE void TTFPointMatches (SplineChar *sc, int layer, int top);

bigreal SFCapHeight (SplineFont *sf, int layer, int return_error);
bigreal SFXHeight (SplineFont *sf, int layer, int return_error);
bigreal SFAscender (SplineFont *sf, int layer, int return_error);
bigreal SFDescender (SplineFont *sf, int layer, int return_error);

SplineChar ***GlyphClassesFromNames (SplineFont *sf, char **classnames,
                                     int class_cnt);
#endif
