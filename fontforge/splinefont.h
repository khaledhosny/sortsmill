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

#include "basics.h"
#include <iconv.h>

#ifdef FONTFORGE_CONFIG_USE_DOUBLE
typedef double real;
typedef double bigreal;
#else
typedef float real;
typedef double bigreal;
#endif

typedef double extended;

/* Solaris wants to define extended to be unsigned [3] unless we do this*/
#define _EXTENDED

#define CHR(ch1,ch2,ch3,ch4) (((ch1)<<24)|((ch2)<<16)|((ch3)<<8)|(ch4))

#define MmMax		16      /* PS says at most this many instances for type1/2 mm fonts */
#define AppleMmMax	26      /* Apple sort of has a limit of 4095, but we only support this many */

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

enum otlookup_type
{
  ot_undef = 0,                 /* Not a lookup type */
  gsub_start = 0x000,           /* Not a lookup type */
  gsub_single = 0x001,
  gsub_multiple = 0x002,
  gsub_alternate = 0x003,
  gsub_ligature = 0x004,
  gsub_context = 0x005,
  gsub_contextchain = 0x006,
  /* GSUB extension 7 */
  gsub_reversecchain = 0x008,
  /* mac state machines */
  morx_indic = 0x0fd,
  morx_context = 0x0fe,
  morx_insert = 0x0ff,
  /* ********************* */
  gpos_start = 0x100,           /* Not a lookup type */

  gpos_single = 0x101,
  gpos_pair = 0x102,
  gpos_cursive = 0x103,
  gpos_mark2base = 0x104,
  gpos_mark2ligature = 0x105,
  gpos_mark2mark = 0x106,
  gpos_context = 0x107,
  gpos_contextchain = 0x108,
  /* GPOS extension 9 */
  kern_statemachine = 0x1ff
    /* otlookup&0xff == lookup type for the appropriate table */
    /* otlookup>>8:     0=>GSUB, 1=>GPOS */
};

enum otlookup_typemasks
{
  gsub_single_mask = 0x00001,
  gsub_multiple_mask = 0x00002,
  gsub_alternate_mask = 0x00004,
  gsub_ligature_mask = 0x00008,
  gsub_context_mask = 0x00010,
  gsub_contextchain_mask = 0x00020,
  gsub_reversecchain_mask = 0x00040,
  morx_indic_mask = 0x00080,
  morx_context_mask = 0x00100,
  morx_insert_mask = 0x00200,
  /* ********************* */
  gpos_single_mask = 0x00400,
  gpos_pair_mask = 0x00800,
  gpos_cursive_mask = 0x01000,
  gpos_mark2base_mask = 0x02000,
  gpos_mark2ligature_mask = 0x04000,
  gpos_mark2mark_mask = 0x08000,
  gpos_context_mask = 0x10000,
  gpos_contextchain_mask = 0x20000,
  kern_statemachine_mask = 0x40000
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
  bool ismac;                   /* treat the featuretag as a mac feature/setting */
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

struct lookup_subtable
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
};

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
  uint16_t first_pixel_size, last_pixel_size;   /* A range of point sizes to which this table applies */
  int8_t *corrections;          /* a set of pixel corrections, one for each point size */
} DeviceTable;

typedef struct valdev
{                               /* Value records can have four associated device tables */
  DeviceTable xadjust;
  DeviceTable yadjust;
  DeviceTable xadv;
  DeviceTable yadv;
} ValDevTab;

enum anchorclass_type
{
  act_mark,
  act_mkmk,
  act_curs,
  act_mklg
};

typedef struct anchorclass
{
  char *name;                   /* in utf8 */
  struct lookup_subtable *subtable;
  uint8_t type;                 /* anchorclass_type */
  uint8_t has_base;
  uint8_t processed, has_mark, matches, ac_num;
  uint8_t ticked;
  struct anchorclass *next;
} AnchorClass;

enum anchor_type
{
  at_mark,
  at_basechar,
  at_baselig,
  at_basemark,
  at_centry,
  at_cexit,
  at_max,
  at_illegal = 15
};

typedef struct anchorpoint
{
  AnchorClass *anchor;
  BasePoint me;
  DeviceTable xadjust, yadjust;
  unsigned int type:4;
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

struct macname
{
  struct macname *next;
  uint16_t enc;                 /* Platform specific encoding. 0=>mac roman, 1=>sjis, 7=>russian */
  uint16_t lang;                /* Mac languages 0=>english, 1=>french, 2=>german */
  char *name;                   /* Not a unicode string, uninterpreted mac encoded string */
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

struct macsetting
{
  struct macsetting *next;
  uint16_t setting;
  uint16_t strid;
  struct macname *setname;
  bool initially_enabled;
};

typedef struct macfeat
{
  struct macfeat *next;
  uint16_t feature;
  uint8_t ismutex;
  uint8_t default_setting;      /* Apple's docs say both that this is a byte and a short. It's a byte */
  uint16_t strid;               /* Temporary value, used when reading in */
  struct macname *featname;
  struct macsetting *settings;
} MacFeat;

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
{                               /* A per-font map of encoding to glyph id */
  int32_t *map;                 /* Map from encoding to glyphid */
  int32_t *backmap;             /* Map from glyphid to encoding */
  int enccount;                 /* used size of the map array */
  /*  strictly speaking this might include */
  /*  glyphs that are not encoded, but which */
  /*  are displayed after the proper encoding */
  int encmax;                   /* allocated size of the map array */
  int backmax;                  /* allocated size of the backmap array */
  struct remap *remap;
  Encoding *enc;
  bool ticked;
} EncMap;

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
                                   font-wide vertical offset or when
                                   generating morx where it is the
                                   mask of tables in which the glyph
                                   occurs.  Always a temporary
                                   value */
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

typedef struct splinefont
{
  char *fontname, *fullname, *familyname, *weight;
  char *copyright;
  char *filename;               /* sfd name. NULL if we open a font, that's origname */
  char *defbasefilename;
  char *version;
  real italicangle, upos, uwidth;       /* In font info */
  int ascent, descent;
  int uniqueid;                 /* Not copied when reading in!!!! */
  int glyphcnt, glyphmax;       /* allocated size of glyphs array */
  SplineChar **glyphs;
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
  bool multilayer;              /* only applies if TYPE3 is set, means this font can contain strokes & fills */
  /*  I leave it in so as to avoid cluttering up code with #ifdefs */
  bool strokedfont;
//    bool new_;                        /* A new and unsaved font */ <-- Switch to this later.
  bool new;                     /* A new and unsaved font */
  bool compacted;               /* only used when opening a font */
  unsigned int backedup:2;      /* 0=>don't know, 1=>no, 2=>yes */
  bool use_typo_metrics;        /* The standard says to. But MS */
  /* seems to feel that isn't good */
  /* enough and has created a bit */
  /* to mean "really use them" */
  bool weight_width_slope_only; /* This bit seems stupid to me */
  bool save_to_dir;             /* Loaded from an sfdir collection rather than a simple sfd file */
  bool head_optimized_for_cleartype;    /* Bit in the 'head' flags field, if unset "East Asian fonts in the Windows Presentation Framework (Avalon) will not be hinted" */
  bool ticked;
  bool internal_temp;           /* Internal temporary font to be passed to freetype for rasterizing. Don't complain about oddities. Don't generate GPOS/GSUB tables, etc. */
  bool complained_about_spiros;
  bool use_xuid;                /* Adobe has deprecated these two */
  bool use_uniqueid;            /* fields. Mostly we don't want to use them */
  /* 2 bits left */
  struct fontviewbase *fv;
  struct metricsview *metrics;
  enum uni_interp uni_interp;
  NameList *for_new_glyphs;
  EncMap *map;                  /* only used when opening a font to provide original default encoding */
  Layer grid;
  BDFFont *bitmaps;
  char *origname;               /* filename of font file (ie. if not an sfd) */
  char *autosavename;
  int display_size;             /* a val <0 => Generate our own images from splines, a value >0 => find a bdf font of that size */
//    struct psdict *private_;  /* read in from type1 file or provided by user */ <-- switch to this later.
  struct psdict *private;       /* read in from type1 file or provided by user */
  char *xuid;
  struct pfminfo pfminfo;
  struct ttflangname *names;
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
  /* Apple morx subtables become gsub, and kern subtables become gpos */
  AnchorClass *anchor;
  KernClass *kerns, *vkerns;
  FPST *possub;
  MacFeat *features;
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
  int extrema_bound;            /* Splines do not count for extrema complaints when the distance between the endpoints is less than or equal to this */
  int width_separation;
  int sfntRevision;
#define sfntRevisionUnset	0x44445555
  int woffMajor;
#define woffUnset		0x4455
  int woffMinor;
  char *woffMetadata;
  real ufo_ascent, ufo_descent; /* I don't know what these mean, they don't seem to correspond to any other ascent/descent pair, but retain them so round-trip ufo input/output leaves them unchanged */
  /* ufo_descent is negative */
} SplineFont;

struct axismap
{
  int points;                   /* size of the next two arrays */
  real *blends;                 /* between [0,1] ordered so that blend[0]<blend[1]<... */
  real *designs;                /* between the design ranges for this axis, typically [1,999] or [6,72] */
  real min, def, max;           /* For mac */
  struct macname *axisnames;    /* For mac */
};

struct named_instance
{                               /* For mac */
  real *coords;                 /* array[axis], these are in user units */
  struct macname *names;
};

/* I am going to simplify my life and not encourage intermediate designs */
/*  this means I can easily calculate ConvertDesignVector, and don't have */
/*  to bother the user with specifying it. */
/* (NormalizeDesignVector is fairly basic and shouldn't need user help ever) */
/*  (As long as they want piecewise linear) */
/* I'm not going to support intermediate designs at all for apple var tables */
typedef struct mmset
{
  int axis_count;
  char *axes[4];
  int instance_count;
  SplineFont **instances;
  SplineFont *normal;
  real *positions;              /* array[instance][axis] saying where each instance lies on each axis */
  real *defweights;             /* array[instance] saying how much of each instance makes the normal font */
  /* for adobe */
  struct axismap *axismaps;     /* array[axis] */
  char *cdv, *ndv;              /* for adobe */
  int named_instance_count;
  struct named_instance *named_instances;
  bool changed;
  bool apple;
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

VISIBLE extern char *strconcat (const char *str, const char *str2);
VISIBLE extern char *strconcat3 (const char *str, const char *str2,
                                 const char *str3);

extern char *XUIDFromFD (int xuid[20]);
extern SplineFont *SplineFontFromPSFont (struct fontdict *fd);
extern int CheckAfmOfPostScript (SplineFont *sf, char *psname, EncMap *map);
extern int LoadKerningDataFromAmfm (SplineFont *sf, char *filename,
                                    EncMap *map);
extern int LoadKerningDataFromAfm (SplineFont *sf, char *filename,
                                   EncMap *map);
extern int LoadKerningDataFromTfm (SplineFont *sf, char *filename,
                                   EncMap *map);
extern int LoadKerningDataFromOfm (SplineFont *sf, char *filename,
                                   EncMap *map);
extern int LoadKerningDataFromPfm (SplineFont *sf, char *filename,
                                   EncMap *map);
extern int LoadKerningDataFromMacFOND (SplineFont *sf, char *filename,
                                       EncMap *map);
VISIBLE extern int LoadKerningDataFromMetricsFile (SplineFont *sf,
                                                   char *filename,
                                                   EncMap *map);
VISIBLE extern void FeatDumpFontLookups (FILE *out, SplineFont *sf);
VISIBLE extern void FeatDumpOneLookup (FILE *out, SplineFont *sf,
                                       OTLookup *otl);
extern void SFApplyFeatureFile (SplineFont *sf, FILE *file, char *filename);
extern void SFApplyFeatureFilename (SplineFont *sf, char *filename);
extern void SubsNew (SplineChar *to, enum possub_type type, int tag,
                     char *components, SplineChar *default_script);
extern void PosNew (SplineChar *to, int tag, int dx, int dy, int dh, int dv);
extern int SFOneWidth (SplineFont *sf);
extern int CIDOneWidth (SplineFont *sf);
extern int SFOneHeight (SplineFont *sf);
extern int SFIsCJK (SplineFont *sf, EncMap *map);
VISIBLE extern void CIDMasterAsDes (SplineFont *sf);
enum fontformat
{ ff_pfa, ff_pfb, ff_pfbmacbin, ff_multiple, ff_mma, ff_mmb,
  ff_ptype3, ff_ptype0, ff_cid, ff_cff, ff_cffcid,
  ff_type42, ff_type42cid,
  ff_ttf, ff_ttfsym, ff_ttfmacbin, ff_ttc, ff_ttfdfont, ff_otf, ff_otfdfont,
  ff_otfcid, ff_otfciddfont, ff_svg, ff_ufo, ff_woff, ff_none
};
VISIBLE extern int CanWoff (void);
extern struct pschars *SplineFont2ChrsSubrs (SplineFont *sf, int iscjk,
                                             struct pschars *subrs, int flags,
                                             enum fontformat format,
                                             int layer);
extern int CanonicalCombiner (int uni);
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

extern void GrowBuffer (GrowBuf * gb);
extern void GrowBufferAdd (GrowBuf * gb, int ch);
VISIBLE extern void GrowBufferAddStr (GrowBuf * gb, char *str);

struct glyphdata;
extern int UnitsParallel (BasePoint *u1, BasePoint *u2, int strict);
extern int CvtPsStem3 (struct growbuf *gb, SplineChar *scs[MmMax],
                       int instance_count, int ishstem, int round);
extern struct pschars *CID2ChrsSubrs (SplineFont *cidmaster,
                                      struct cidbytes *cidbytes, int flags,
                                      int layer);
extern struct pschars *SplineFont2ChrsSubrs2 (SplineFont *sf, int nomwid,
                                              int defwid, const int *bygid,
                                              int cnt, int flags,
                                              struct pschars **_subrs,
                                              int layer);
extern struct pschars *CID2ChrsSubrs2 (SplineFont *cidmaster,
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

extern int32_t filechecksum (FILE *file);
VISIBLE extern const char *GetAuthor (void);
extern SplineChar *SFFindExistingCharMac (SplineFont *, EncMap *map,
                                          int unienc);
extern void SC_PSDump (void (*dumpchar) (int ch, void *data), void *data,
                       SplineChar *sc, int refs_to_splines, int pdfopers,
                       int layer);
extern int _WritePSFont (FILE *out, SplineFont *sf, enum fontformat format,
                         int flags, EncMap *enc, SplineFont *fullsf,
                         int layer);
extern int WritePSFont (char *fontname, SplineFont *sf,
                        enum fontformat format, int flags, EncMap *enc,
                        SplineFont *fullsf, int layer);
extern int WriteMacPSFont (char *fontname, SplineFont *sf,
                           enum fontformat format, int flags, EncMap *enc,
                           int layer);
extern int _WriteWOFFFont (FILE *ttf, SplineFont *sf, enum fontformat format,
                           int32_t *bsizes, enum bitmapformat bf, int flags,
                           EncMap *enc, int layer);
extern int WriteWOFFFont (char *fontname, SplineFont *sf,
                          enum fontformat format, int32_t *bsizes,
                          enum bitmapformat bf, int flags, EncMap *enc,
                          int layer);
extern int _WriteTTFFont (FILE *ttf, SplineFont *sf, enum fontformat format,
                          int32_t *bsizes, enum bitmapformat bf, int flags,
                          EncMap *enc, int layer);
extern int WriteTTFFont (char *fontname, SplineFont *sf,
                         enum fontformat format, int32_t *bsizes,
                         enum bitmapformat bf, int flags, EncMap *enc,
                         int layer);
extern int _WriteType42SFNTS (FILE *type42, SplineFont *sf,
                              enum fontformat format, int flags, EncMap *enc,
                              int layer);
extern int WriteMacTTFFont (char *fontname, SplineFont *sf,
                            enum fontformat format, int32_t *bsizes,
                            enum bitmapformat bf, int flags, EncMap *enc,
                            int layer);
extern int WriteMacBitmaps (char *filename, SplineFont *sf, int32_t *sizes,
                            int is_dfont, EncMap *enc);
extern int WritePalmBitmaps (char *filename, SplineFont *sf, int32_t *sizes,
                             EncMap *enc);
VISIBLE extern int WriteMacFamily (char *filename, struct sflist *sfs,
                                   enum fontformat format,
                                   enum bitmapformat bf, int flags,
                                   int layer);
VISIBLE extern int WriteTTC (char *filename, struct sflist *sfs,
                             enum fontformat format, enum bitmapformat bf,
                             int flags, int layer, enum ttc_flags ttcflags);
extern long mactime (void);
extern int WriteSVGFont (char *fontname, SplineFont *sf,
                         enum fontformat format, int flags, EncMap *enc,
                         int layer);
extern int _WriteSVGFont (FILE *file, SplineFont *sf, enum fontformat format,
                          int flags, EncMap *enc, int layer);
extern int WriteUFOFont (char *fontname, SplineFont *sf,
                         enum fontformat format, int flags, EncMap *enc,
                         int layer);
VISIBLE extern void SfListFree (struct sflist *sfs);
VISIBLE extern void TTF_PSDupsDefault (SplineFont *sf);
extern void DefaultTTFEnglishNames (struct ttflangname *dummy,
                                    SplineFont *sf);
VISIBLE extern void TeXDefaultParams (SplineFont *sf);
VISIBLE extern int AlreadyMSSymbolArea (SplineFont *sf, EncMap *map);
VISIBLE extern void OS2FigureCodePages (SplineFont *sf, uint32_t CodePage[2]);
VISIBLE extern void OS2FigureUnicodeRanges (SplineFont *sf,
                                            uint32_t Ranges[4]);
VISIBLE extern void SFDefaultOS2Info (struct pfminfo *pfminfo, SplineFont *sf,
                                      char *fontname);
extern void SFDefaultOS2Simple (struct pfminfo *pfminfo, SplineFont *sf);
VISIBLE extern void SFDefaultOS2SubSuper (struct pfminfo *pfminfo, int emsize,
                                          double italicangle);
extern void VerifyLanguages (SplineFont *sf);
VISIBLE extern int ScriptIsRightToLeft (uint32_t script);
extern void ScriptMainRange (uint32_t script, int *start, int *end);
VISIBLE extern uint32_t ScriptFromUnicode (int u, SplineFont *sf);
VISIBLE extern uint32_t SCScriptFromUnicode (SplineChar *sc);
VISIBLE extern int SCRightToLeft (SplineChar *sc);
extern int SLIContainsR2L (SplineFont *sf, int sli);
extern void SFFindNearTop (SplineFont *);
extern void SFRestoreNearTop (SplineFont *);
VISIBLE extern int SFForceEncoding (SplineFont *sf, EncMap *old,
                                    Encoding *new_map);
extern int CountOfEncoding (Encoding *encoding_name);
extern void SFMatchGlyphs (SplineFont *sf, SplineFont *target,
                           int addempties);
VISIBLE extern void MMMatchGlyphs (MMSet *mm);
extern char *_GetModifiers (char *fontname, char *familyname, char *weight);
extern char *SFGetModifiers (SplineFont *sf);
VISIBLE extern const uint32_t *_uGetModifiers (const uint32_t *fontname,
                                               const uint32_t *familyname,
                                               const uint32_t *weight);
extern void SFSetFontName (SplineFont *sf, char *family, char *mods,
                           char *full);
extern void ttfdumpbitmap (SplineFont *sf, struct alltabs *at,
                           int32_t *sizes);
extern void ttfdumpbitmapscaling (SplineFont *sf, struct alltabs *at,
                                  int32_t *sizes);
VISIBLE extern void SplineFontSetUnChanged (SplineFont *sf);

extern int Within4RoundingErrors (bigreal v1, bigreal v2);
extern int Within16RoundingErrors (bigreal v1, bigreal v2);
extern int Within64RoundingErrors (bigreal v1, bigreal v2);
VISIBLE extern int RealNear (real a, real b);
VISIBLE extern int RealNearish (real a, real b);
VISIBLE extern int RealApprox (real a, real b);
VISIBLE extern int RealWithin (real a, real b, real fudge);
extern int RealRatio (real a, real b, real fudge);

VISIBLE extern int PointsDiagonalable (SplineFont *sf, BasePoint **bp,
                                       BasePoint *unit);
VISIBLE extern int MergeDStemInfo (SplineFont *sf, DStemInfo ** ds,
                                   DStemInfo * test);

extern void LineListFree (LineList * ll);
extern void LinearApproxFree (LinearApprox * la);
VISIBLE extern void SplineFree (Spline * spline);
VISIBLE extern SplinePoint *SplinePointCreate (real x, real y);
VISIBLE extern void SplinePointFree (SplinePoint *sp);
extern void SplinePointsFree (SplinePointList *spl);
VISIBLE extern void SplinePointListFree (SplinePointList *spl);
VISIBLE extern void SplinePointListsFree (SplinePointList *head);
VISIBLE extern void SplineSetSpirosClear (SplineSet *spl);
extern void SplineSetBeziersClear (SplineSet *spl);
VISIBLE extern void RefCharFree (RefChar *ref);
VISIBLE extern void RefCharsFree (RefChar *ref);
VISIBLE extern void RefCharsFreeRef (RefChar *ref);
extern void CopyBufferFree (void);
extern void CopyBufferClearCopiedFrom (SplineFont *dying);
VISIBLE extern void UndoesFree (Undoes *undo);
VISIBLE extern void StemInfosFree (StemInfo * h);
VISIBLE extern void StemInfoFree (StemInfo * h);
VISIBLE extern void DStemInfosFree (DStemInfo * h);
VISIBLE extern void DStemInfoFree (DStemInfo * h);
VISIBLE extern void KernPairsFree (KernPair *kp);
VISIBLE extern void SCOrderAP (SplineChar *sc);
VISIBLE extern void AnchorPointsFree (AnchorPoint *ap);
VISIBLE extern AnchorPoint *AnchorPointsCopy (AnchorPoint *alist);
VISIBLE extern void SFRemoveAnchorClass (SplineFont *sf, AnchorClass * an);
extern int AnchorClassesNextMerge (AnchorClass * ac);
VISIBLE extern int IsAnchorClassUsed (SplineChar *sc, AnchorClass * an);
extern AnchorPoint *APAnchorClassMerge (AnchorPoint *anchors,
                                        AnchorClass * into,
                                        AnchorClass * from);
extern void AnchorClassMerge (SplineFont *sf, AnchorClass * into,
                              AnchorClass * from);
extern void AnchorClassesFree (AnchorClass * kp);
VISIBLE extern void TtfTablesFree (struct ttf_table *tab);
extern void SFRemoveSavedTable (SplineFont *sf, uint32_t tag);
VISIBLE extern AnchorClass *AnchorClassMatch (SplineChar *sc1,
                                              SplineChar *sc2,
                                              AnchorClass * restrict_,
                                              AnchorPoint **_ap1,
                                              AnchorPoint **_ap2);
extern AnchorClass *AnchorClassMkMkMatch (SplineChar *sc1, SplineChar *sc2,
                                          AnchorPoint **_ap1,
                                          AnchorPoint **_ap2);
extern AnchorClass *AnchorClassCursMatch (SplineChar *sc1, SplineChar *sc2,
                                          AnchorPoint **_ap1,
                                          AnchorPoint **_ap2);
extern void SCInsertPST (SplineChar *sc, PST *new_);
VISIBLE extern void ValDevFree (ValDevTab * adjust);
VISIBLE extern ValDevTab *ValDevTabCopy (ValDevTab * orig);
VISIBLE extern void DeviceTableFree (DeviceTable *adjust);
VISIBLE extern DeviceTable *DeviceTableCopy (DeviceTable *orig);
VISIBLE extern void DeviceTableSet (DeviceTable *adjust, int size,
                                    int correction);
VISIBLE extern void PSTFree (PST *lig);
extern uint16_t PSTDefaultFlags (enum possub_type type, SplineChar *sc);
VISIBLE extern int PSTContains (const char *components, const char *name);
VISIBLE extern StemInfo *StemInfoCopy (StemInfo * h);
VISIBLE extern DStemInfo *DStemInfoCopy (DStemInfo * h);
VISIBLE extern void SPChangePointType (SplinePoint *sp, int pointtype);

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

extern PST *PSTCopy (PST *base, SplineChar *sc, struct sfmergecontext *mc);
extern struct lookup_subtable *MCConvertSubtable (struct sfmergecontext *mc,
                                                  struct lookup_subtable
                                                  *sub);
extern AnchorClass *MCConvertAnchorClass (struct sfmergecontext *mc,
                                          AnchorClass * ac);
extern void SFFinishMergeContext (struct sfmergecontext *mc);
VISIBLE extern SplineChar *SplineCharCopy (SplineChar *sc, SplineFont *into,
                                           struct sfmergecontext *);
extern BDFChar *BDFCharCopy (BDFChar *bc);
VISIBLE extern void BCFlattenFloat (BDFChar *bc);
extern void BitmapsCopy (SplineFont *to, SplineFont *from, int to_index,
                         int from_index);
extern struct gimage *ImageAlterClut (struct gimage *image);
VISIBLE extern void ImageListsFree (ImageList *imgs);
VISIBLE extern void TTFLangNamesFree (struct ttflangname *l);
VISIBLE extern void AltUniFree (struct altuni *altuni);
extern void AltUniFigure (SplineFont *sf, EncMap *map, int check_dups);
extern void AltUniRemove (SplineChar *sc, int uni);
extern void AltUniAdd (SplineChar *sc, int uni);
extern void AltUniAdd_DontCheckDups (SplineChar *sc, int uni);
VISIBLE extern void LayerDefault (Layer *);
extern SplineChar *SplineCharCreate (int layer_cnt);
VISIBLE extern SplineChar *SFSplineCharCreate (SplineFont *sf);
VISIBLE extern RefChar *RefCharCreate (void);
VISIBLE extern RefChar *RefCharsCopy (RefChar *ref);    /* Still needs to be instanciated and have the dependency list adjusted */
VISIBLE extern struct altuni *AltUniCopy (struct altuni *altuni,
                                          SplineFont *noconflicts);
extern void SCAddRef (SplineChar *sc, SplineChar *rsc, int layer, real xoff,
                      real yoff);
extern void _SCAddRef (SplineChar *sc, SplineChar *rsc, int layer,
                       real transform[6]);
extern KernClass *KernClassCopy (KernClass *kc);
extern void KernClassFreeContents (KernClass *kc);
VISIBLE extern void KernClassListFree (KernClass *kc);
extern int KernClassContains (KernClass *kc, char *name1, char *name2,
                              int ordered);
extern void OTLookupFree (OTLookup *lookup);
extern void OTLookupListFree (OTLookup *lookup);
extern FPST *FPSTCopy (FPST *fpst);
VISIBLE extern void FPSTRuleContentsFree (struct fpst_rule *r,
                                          enum fpossub_format format);
VISIBLE extern void FPSTClassesFree (FPST *fpst);
VISIBLE extern void FPSTRulesFree (struct fpst_rule *r,
                                   enum fpossub_format format, int rcnt);
VISIBLE extern void FPSTFree (FPST *fpst);
VISIBLE extern struct macname *MacNameCopy (struct macname *mn);
VISIBLE extern void MacNameListFree (struct macname *mn);
VISIBLE extern void MacSettingListFree (struct macsetting *ms);
VISIBLE extern void MacFeatListFree (MacFeat *mf);
VISIBLE extern void GlyphVariantsFree (struct glyphvariants *gv);
VISIBLE extern struct glyphvariants *GlyphVariantsCopy (struct glyphvariants
                                                        *gv);
extern void MathKernVContentsFree (struct mathkernvertex *mk);
VISIBLE extern void MathKernFree (struct mathkern *mk);
VISIBLE extern struct mathkern *MathKernCopy (struct mathkern *mk);
extern void SplineCharListsFree (struct splinecharlist *dlist);
extern void LayerFreeContents (SplineChar *sc, int layer);
VISIBLE extern void SplineCharFreeContents (SplineChar *sc);
VISIBLE extern void SplineCharFree (SplineChar *sc);
VISIBLE extern void EncMapFree (EncMap *map);
VISIBLE extern EncMap *EncMapFromEncoding (SplineFont *sf, Encoding *enc);
VISIBLE extern EncMap *CompactEncMap (EncMap *map, SplineFont *sf);
VISIBLE extern EncMap *EncMapNew (int encmax, int backmax, Encoding *enc);
VISIBLE extern EncMap *EncMap1to1 (int enccount);
VISIBLE extern EncMap *EncMapCopy (EncMap *map);
extern void SFExpandGlyphCount (SplineFont *sf, int newcnt);
extern void ScriptLangListFree (struct scriptlanglist *sl);
VISIBLE extern void FeatureScriptLangListFree (FeatureScriptLangList *fl);
VISIBLE extern void SFBaseSort (SplineFont *sf);
VISIBLE extern struct baselangextent *BaseLangCopy (struct baselangextent
                                                    *extent);
VISIBLE extern void BaseLangFree (struct baselangextent *extent);
extern void BaseScriptFree (struct basescript *bs);
VISIBLE extern void BaseFree (struct Base *base);
VISIBLE extern void SplineFontFree (SplineFont *sf);
VISIBLE extern struct jstf_lang *JstfLangsCopy (struct jstf_lang *jl);
VISIBLE extern void JstfLangFree (struct jstf_lang *jl);
VISIBLE extern void JustifyFree (Justify * just);
VISIBLE extern void MATHFree (struct MATH *math);
VISIBLE extern struct MATH *MathTableNew (SplineFont *sf);
VISIBLE extern void OtfNameListFree (struct otfname *on);
VISIBLE extern void OtfFeatNameListFree (struct otffeatname *fn);
extern struct otffeatname *findotffeatname (uint32_t tag, SplineFont *sf);
VISIBLE extern void MarkSetFree (int cnt, char **classes, char **names);
VISIBLE extern void MarkClassFree (int cnt, char **classes, char **names);
VISIBLE extern void MMSetFreeContents (MMSet *mm);
extern void MMSetFree (MMSet *mm);
VISIBLE extern void SFRemoveUndoes (SplineFont *sf, uint8_t *selected,
                                    EncMap *map);
VISIBLE extern void SplineRefigure3 (Spline * spline);
VISIBLE extern void SplineRefigure (Spline * spline);
VISIBLE extern Spline *SplineMake3 (SplinePoint *from, SplinePoint *to);
extern LinearApprox *SplineApproximate (Spline * spline, real scale);
VISIBLE extern int SplinePointListIsClockwise (const SplineSet *spl);
VISIBLE extern void SplineSetFindBounds (const SplinePointList *spl,
                                         DBounds *bounds);
VISIBLE extern void SplineCharLayerFindBounds (SplineChar *sc, int layer,
                                               DBounds *bounds);
VISIBLE extern void SplineCharFindBounds (SplineChar *sc, DBounds *bounds);
extern void SplineFontLayerFindBounds (SplineFont *sf, int layer,
                                       DBounds *bounds);
extern void SplineFontFindBounds (SplineFont *sf, DBounds *bounds);
VISIBLE extern void CIDLayerFindBounds (SplineFont *sf, int layer,
                                        DBounds *bounds);
VISIBLE extern void SplineSetQuickBounds (SplineSet *ss, DBounds *b);
VISIBLE extern void SplineCharLayerQuickBounds (SplineChar *sc, int layer,
                                                DBounds *bounds);
VISIBLE extern void SplineCharQuickBounds (SplineChar *sc, DBounds *b);
extern void SplineSetQuickConservativeBounds (SplineSet *ss, DBounds *b);
extern void SplineCharQuickConservativeBounds (SplineChar *sc, DBounds *b);
extern void SplineFontQuickConservativeBounds (SplineFont *sf, DBounds *b);
VISIBLE extern void SplinePointCategorize (SplinePoint *sp);
extern int SplinePointIsACorner (SplinePoint *sp);
VISIBLE extern void SPLCategorizePoints (SplinePointList *spl);
extern void SCCategorizePoints (SplineChar *sc);
VISIBLE extern SplinePointList *SplinePointListCopy1 (const SplinePointList
                                                      *spl);
VISIBLE extern SplinePointList *SplinePointListCopy (const SplinePointList
                                                     *base);
VISIBLE extern SplinePointList *SplinePointListCopySelected (SplinePointList
                                                             *base);
VISIBLE extern SplinePointList
  *SplinePointListCopySpiroSelected (SplinePointList *base);
extern ImageList *ImageListCopy (ImageList *cimg);
VISIBLE extern ImageList *ImageListTransform (ImageList *cimg,
                                              real transform[6],
                                              int everything);
extern void BpTransform (BasePoint *to, BasePoint *from, real transform[6]);
VISIBLE extern void ApTransform (AnchorPoint *ap, real transform[6]);

/* The order of the enum elements below doesn't make much sense, but
   it's done this way to preserve binary compatibility */
enum transformPointType
{
  tpt_OnlySelected,
  tpt_AllPoints,
  tpt_OnlySelectedInterpCPs
};

VISIBLE extern SplinePointList *SplinePointListTransform (SplinePointList
                                                          *base,
                                                          real transform[6],
                                                          enum
                                                          transformPointType
                                                          allpoints);
VISIBLE extern SplinePointList *SplinePointListSpiroTransform (SplinePointList
                                                               *base,
                                                               real
                                                               transform[6],
                                                               int allpoints);
extern SplinePointList *SplinePointListShift (SplinePointList *base,
                                              real xoff,
                                              enum transformPointType
                                              allpoints);
extern HintMask *HintMaskFromTransformedRef (RefChar *ref, BasePoint *trans,
                                             SplineChar *basesc,
                                             HintMask * hm);
extern SplinePointList *SPLCopyTranslatedHintMasks (SplinePointList *base,
                                                    SplineChar *basesc,
                                                    SplineChar *subsc,
                                                    BasePoint *trans);
extern SplinePointList *SPLCopyTransformedHintMasks (RefChar *r,
                                                     SplineChar *basesc,
                                                     BasePoint *trans,
                                                     int layer);
VISIBLE extern SplinePointList *SplinePointListRemoveSelected (SplineChar *sc,
                                                               SplinePointList
                                                               *base);
extern void SplinePointListSet (SplinePointList *tobase,
                                SplinePointList *frombase);
extern void SplinePointListSelect (SplinePointList *spl, int sel);
VISIBLE extern void SCRefToSplines (SplineChar *sc, RefChar *rf, int layer);
VISIBLE extern void RefCharFindBounds (RefChar *rf);
VISIBLE extern void SCReinstanciateRefChar (SplineChar *sc, RefChar *rf,
                                            int layer);
VISIBLE extern void SCReinstanciateRef (SplineChar *sc, SplineChar *rsc,
                                        int layer);
VISIBLE extern void SFReinstanciateRefs (SplineFont *sf);
extern void SFInstanciateRefs (SplineFont *sf);
extern SplineChar *MakeDupRef (SplineChar *base, int local_enc, int uni_enc);
VISIBLE extern void SCRemoveDependent (SplineChar *dependent, RefChar *rf,
                                       int layer);
extern void SCRemoveLayerDependents (SplineChar *dependent, int layer);
extern void SCRemoveDependents (SplineChar *dependent);
VISIBLE extern int SCDependsOnSC (SplineChar *parent, SplineChar *child);
VISIBLE extern void BCCompressBitmap (BDFChar *bdfc);
extern void BCRegularizeBitmap (BDFChar *bdfc);
extern void BCRegularizeGreymap (BDFChar *bdfc);
VISIBLE extern void BCPasteInto (BDFChar *bc, BDFChar *rbc, int ixoff,
                                 int iyoff, int invert, int cleartoo);
extern void BCRotateCharForVert (BDFChar *bc, BDFChar *from,
                                 BDFFont *frombdf);
extern int GradientHere (bigreal scale, DBounds *bbox, int iy, int ix,
                         struct gradient *grad, struct pattern *pat,
                         int defgrey);
extern void PatternPrep (SplineChar *sc, struct brush *brush, bigreal scale);
VISIBLE extern BDFChar *SplineCharRasterize (SplineChar *sc, int layer,
                                             bigreal pixelsize);
extern BDFFont *SplineFontToBDFHeader (SplineFont *_sf, int pixelsize,
                                       int indicate);
extern BDFFont *SplineFontRasterize (SplineFont *sf, int layer, int pixelsize,
                                     int indicate);
extern void BDFCAntiAlias (BDFChar *bc, int linear_scale);
VISIBLE extern BDFChar *SplineCharAntiAlias (SplineChar *sc, int layer,
                                             int pixelsize, int linear_scale);
extern BDFFont *SplineFontAntiAlias (SplineFont *sf, int layer, int pixelsize,
                                     int linear_scale);
VISIBLE extern struct clut *_BDFClut (int linear_scale);
extern void BDFClut (BDFFont *bdf, int linear_scale);
VISIBLE extern int BDFDepth (BDFFont *bdf);
VISIBLE extern BDFChar *BDFPieceMeal (BDFFont *bdf, int index);
VISIBLE extern BDFChar *BDFPieceMealCheck (BDFFont *bdf, int index);

enum piecemeal_flags
{
  pf_antialias = 1,
  pf_bbsized = 2,
  pf_ft_nohints = 4,
  pf_ft_recontext = 8
};

VISIBLE extern BDFFont *SplineFontPieceMeal (SplineFont *sf, int layer,
                                             int ptsize, int dpi, int flags,
                                             void *freetype_context);
VISIBLE extern void BDFCharFindBounds (BDFChar *bc, IBounds * bb);
VISIBLE extern int BDFCharQuickBounds (BDFChar *bc, IBounds * bb, int8_t xoff,
                                       int8_t yoff, int use_backup,
                                       int first);
extern void BCPrepareForOutput (BDFChar *bc, int mergeall);
extern void BCRestoreAfterOutput (BDFChar *bc);
extern void BCMakeDependent (BDFChar *dependent, BDFChar *base);
VISIBLE extern void BCRemoveDependent (BDFChar *dependent, BDFRefChar * rf);
extern void BCExpandBitmapToEmBox (BDFChar *bc, int xmin, int ymin, int xmax,
                                   int ymax);
extern BDFFont *BitmapFontScaleTo (BDFFont *old, int to);
VISIBLE extern void BDFCharFree (BDFChar *bdfc);
VISIBLE extern void BDFPropsFree (BDFFont *bdf);
VISIBLE extern void BDFFontFree (BDFFont *bdf);
extern void SFDefaultAscent (SplineFont *sf);
extern int PSBitmapDump (char *filename, BDFFont *font, EncMap *map);
extern int BDFFontDump (char *filename, BDFFont *font, EncMap *map, int res);
extern int FNTFontDump (char *filename, BDFFont *font, EncMap *map, int res);
extern int FONFontDump (char *filename, SplineFont *sf, int32_t *sizes,
                        int res, EncMap *map);
VISIBLE extern void SFReplaceEncodingBDFProps (SplineFont *sf, EncMap *map);
VISIBLE extern void SFReplaceFontnameBDFProps (SplineFont *sf);
extern int IsUnsignedBDFKey (char *key);
VISIBLE extern int BdfPropHasInt (BDFFont *font, const char *key, int def);
VISIBLE extern char *BdfPropHasString (BDFFont *font, const char *key,
                                       char *def);
extern void def_Charset_Enc (EncMap *map, char *reg, char *enc);
VISIBLE extern void Default_XLFD (BDFFont *bdf, EncMap *map, int res);
VISIBLE extern void Default_Properties (BDFFont *bdf, EncMap *map,
                                        char *onlyme);
VISIBLE extern void BDFDefaultProps (BDFFont *bdf, EncMap *map, int res);
VISIBLE extern BDFProperties *BdfPropsCopy (BDFProperties * props, int cnt);

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

VISIBLE extern void XLFD_GetComponents (char *xlfd,
                                        struct xlfd_components *comp);
VISIBLE extern void XLFD_CreateComponents (BDFFont *bdf, EncMap *map, int res,
                                           struct xlfd_components *comp);
/* Two lines intersect in at most 1 point */
/* Two quadratics intersect in at most 4 points */
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           /* Two cubics intersect in at most 9 points *//* Plus an extra space for a trailing -1 */
VISIBLE extern int SplinesIntersect (const Spline * s1, const Spline * s2,
                                     BasePoint pts[9], extended t1s[10],
                                     extended t2s[10]);
VISIBLE extern SplineSet *LayerAllSplines (Layer *layer);
VISIBLE extern SplineSet *LayerUnAllSplines (Layer *layer);
VISIBLE extern int SplineSetIntersect (SplineSet *spl, Spline ** _spline,
                                       Spline ** _spline2);
extern int LineTangentToSplineThroughPt (Spline * s, BasePoint *pt,
                                         extended ts[4], extended tmin,
                                         extended tmax);
VISIBLE extern int _CubicSolve (const Spline1D *sp, bigreal sought,
                                extended ts[3]);
VISIBLE extern int CubicSolve (const Spline1D *sp, bigreal sought,
                               extended ts[3]);
/* Uses an algebraic solution */
extern extended SplineSolve (const Spline1D *sp, real tmin, real tmax,
                             extended sought_y);
/* Tries to fixup rounding errors that crept in to the solution */
extern extended SplineSolveFixup (const Spline1D *sp, real tmin, real tmax,
                                  extended sought_y);
/* Uses an iterative approximation */
extern extended IterateSplineSolve (const Spline1D *sp, extended tmin,
                                    extended tmax, extended sought_y);
/* Uses an iterative approximation and then tries to fix things up */
extern extended IterateSplineSolveFixup (const Spline1D *sp, extended tmin,
                                         extended tmax, extended sought_y);
extern void SplineFindExtrema (const Spline1D *sp, extended *_t1,
                               extended *_t2);
extern int SSBoundsWithin (SplineSet *ss, bigreal z1, bigreal z2,
                           bigreal *wmin, bigreal *wmax, int major);
extern bigreal SplineMinDistanceToPoint (Spline * s, BasePoint *p);

SplineSet *SplineSetsInterpolate (SplineSet *base, SplineSet *other,
                                  real amount, SplineChar *sc);
SplineChar *SplineCharInterpolate (SplineChar *base, SplineChar *other,
                                   real amount, SplineFont *newfont);
VISIBLE extern SplineFont *InterpolateFont (SplineFont *base,
                                            SplineFont *other, real amount,
                                            Encoding *enc);

VISIBLE double SFSerifHeight (SplineFont *sf);

VISIBLE extern void DumpPfaEditEncodings (void);
VISIBLE extern char *ParseEncodingFile (char *filename, char *encodingname);
VISIBLE extern void LoadPfaEditEncodings (void);

extern int GenerateScript (SplineFont *sf, char *filename, char *bitmaptype,
                           int fmflags, int res, char *subfontdirectory,
                           struct sflist *sfs, EncMap *map,
                           NameList * rename_to, int layer);

extern void _SCAutoTrace (SplineChar *sc, int layer, char **args);
extern char **AutoTraceArgs (int ask);

#define CURVATURE_ERROR	-1e9
VISIBLE extern bigreal SplineCurvature (Spline * s, bigreal t);

extern double CheckExtremaForSingleBitErrors (const Spline1D *sp, double t,
                                              double othert);
VISIBLE extern int Spline2DFindExtrema (const Spline * sp,
                                        extended extrema[4]);
VISIBLE extern int Spline2DFindPointsOfInflection (const Spline * sp,
                                                   extended poi[2]);
extern int SplineAtInflection (Spline1D *sp, bigreal t);
extern int SplineAtMinMax (Spline1D *sp, bigreal t);
extern void SplineRemoveExtremaTooClose (Spline1D *sp, extended *_t1,
                                         extended *_t2);
VISIBLE extern int NearSpline (struct findsel *fs, Spline * spline);
extern real SplineNearPoint (Spline * spline, BasePoint *bp, real fudge);
VISIBLE extern int SplineT2SpiroIndex (Spline * spline, bigreal t,
                                       SplineSet *spl);
VISIBLE extern void SCMakeDependent (SplineChar *dependent, SplineChar *base);
VISIBLE extern SplinePoint *SplineBisect (Spline * spline, extended t);
VISIBLE extern Spline *SplineSplit (Spline * spline, extended ts[3]);
VISIBLE extern Spline *ApproximateSplineFromPoints (SplinePoint *from,
                                                    SplinePoint *to,
                                                    TPoint * mid, int cnt,
                                                    int order2);
VISIBLE extern Spline *ApproximateSplineFromPointsSlopes (SplinePoint *from,
                                                          SplinePoint *to,
                                                          TPoint * mid,
                                                          int cnt,
                                                          int order2);
VISIBLE extern bigreal SplineLength (Spline * spline);
VISIBLE extern bigreal SplineLengthRange (Spline * spline, real from_t,
                                          real to_t);
VISIBLE extern bigreal PathLength (SplineSet *ss);
extern Spline *PathFindDistance (SplineSet *path, bigreal d, bigreal *_t);
VISIBLE extern SplineSet *SplineSetBindToPath (SplineSet *ss, int doscale,
                                               int glyph_as_unit, int align,
                                               real offset, SplineSet *path);
extern int SplineIsLinear (Spline * spline);
extern int SplineIsLinearMake (Spline * spline);
extern int SplineInSplineSet (Spline * spline, SplineSet *spl);
extern int SSPointWithin (SplineSet *spl, BasePoint *pt);
extern SplineSet *SSRemoveZeroLengthSplines (SplineSet *base);
extern void SSRemoveStupidControlPoints (SplineSet *base);
extern void SSOverlapClusterCpAngles (SplineSet *base, bigreal within);
extern void SplinesRemoveBetween (SplineChar *sc, SplinePoint *from,
                                  SplinePoint *to, int type);
VISIBLE extern void SplineCharMerge (SplineChar *sc, SplineSet **head,
                                     int type);
extern void SPLNearlyHvCps (SplineChar *sc, SplineSet *ss, bigreal err);
extern void SPLNearlyHvLines (SplineChar *sc, SplineSet *ss, bigreal err);
extern int SPLNearlyLines (SplineChar *sc, SplineSet *ss, bigreal err);
VISIBLE extern int SPInterpolate (SplinePoint *sp);
extern void SplinePointListSimplify (SplineChar *sc, SplinePointList *spl,
                                     struct simplifyinfo *smpl);
VISIBLE extern SplineSet *SplineCharSimplify (SplineChar *sc, SplineSet *head,
                                              struct simplifyinfo *smpl);
VISIBLE extern void SPLStartToLeftmost (SplineChar *sc, SplinePointList *spl,
                                        int *changed);
extern void SPLsStartToLeftmost (SplineChar *sc, int layer);
VISIBLE extern void CanonicalContours (SplineChar *sc, int layer);
VISIBLE extern void SplineSetJoinCpFixup (SplinePoint *sp);
VISIBLE extern SplineSet *SplineSetJoin (SplineSet *start, int doall,
                                         real fudge, int *changed);

enum ae_type
{
  ae_all,
  ae_between_selected,
  ae_only_good,
  ae_only_good_rm_later
};

VISIBLE extern int SpIsExtremum (SplinePoint *sp);
extern int Spline1DCantExtremeX (const Spline * s);
extern int Spline1DCantExtremeY (const Spline * s);
extern Spline *SplineAddExtrema (Spline * s, int always, real lenbound,
                                 real offsetbound, DBounds *b);
VISIBLE extern void SplineSetAddExtrema (SplineChar *sc, SplineSet *ss,
                                         enum ae_type between_selected,
                                         int emsize);
extern void SplineSetAddSpiroExtrema (SplineChar *sc, SplineSet *ss,
                                      enum ae_type between_selected,
                                      int emsize);
VISIBLE extern void SplineCharAddExtrema (SplineChar *sc, SplineSet *head,
                                          enum ae_type between_selected,
                                          int emsize);
extern SplineSet *SplineCharRemoveTiny (SplineChar *sc, SplineSet *head);
VISIBLE extern SplineFont *SplineFontNew (void);
VISIBLE extern char *GetNextUntitledName (void);
extern SplineFont *SplineFontEmpty (void);
VISIBLE extern SplineFont *SplineFontBlank (int charcnt);
extern void SFIncrementXUID (SplineFont *sf);
VISIBLE extern void SFRandomChangeXUID (SplineFont *sf);
VISIBLE extern SplineSet *SplineSetReverse (SplineSet *spl);
extern SplineSet *SplineSetsExtractOpen (SplineSet **tbase);
extern void SplineSetsInsertOpen (SplineSet **tbase, SplineSet *open);
VISIBLE extern SplineSet *SplineSetsCorrect (SplineSet *base, int *changed);
extern SplineSet *SplineSetsAntiCorrect (SplineSet *base);
VISIBLE extern SplineSet *SplineSetsDetectDir (SplineSet **_base,
                                               int *lastscan);
extern void SPAverageCps (SplinePoint *sp);
extern void SPLAverageCps (SplinePointList *spl);
extern void SPWeightedAverageCps (SplinePoint *sp);
VISIBLE extern void BP_HVForce (BasePoint *vector);
VISIBLE extern void SplineCharDefaultPrevCP (SplinePoint *base);
VISIBLE extern void SplineCharDefaultNextCP (SplinePoint *base);
VISIBLE extern void SplineCharTangentNextCP (SplinePoint *sp);
VISIBLE extern void SplineCharTangentPrevCP (SplinePoint *sp);
VISIBLE extern void SPAdjustControl (SplinePoint *sp, BasePoint *cp,
                                     BasePoint *to, int order2);
VISIBLE extern void SPHVCurveForce (SplinePoint *sp);
extern void SPSmoothJoint (SplinePoint *sp);
VISIBLE extern int PointListIsSelected (SplinePointList *spl);
VISIBLE extern void SCSplinePointsUntick (SplineChar *sc, int layer);
extern void SplineSetsUntick (SplineSet *spl);
extern void SFOrderBitmapList (SplineFont *sf);
extern int KernThreshold (SplineFont *sf, int cnt);
VISIBLE extern real SFGuessItalicAngle (SplineFont *sf);

extern SplinePoint *SplineTtfApprox (Spline * ps);
VISIBLE extern SplineSet *SSttfApprox (SplineSet *ss);
VISIBLE extern SplineSet *SplineSetsTTFApprox (SplineSet *ss);
extern SplineSet *SSPSApprox (SplineSet *ss);
extern SplineSet *SplineSetsPSApprox (SplineSet *ss);
extern SplineSet *SplineSetsConvertOrder (SplineSet *ss, int to_order2);
VISIBLE extern void SplineRefigure2 (Spline * spline);
VISIBLE extern void SplineRefigureFixup (Spline * spline);
VISIBLE extern Spline *SplineMake2 (SplinePoint *from, SplinePoint *to);
VISIBLE extern Spline *SplineMake (SplinePoint *from, SplinePoint *to,
                                   int order2);
VISIBLE extern Spline *SFSplineMake (SplineFont *sf, SplinePoint *from,
                                     SplinePoint *to);
extern void SCConvertToOrder2 (SplineChar *sc);
extern void SFConvertToOrder2 (SplineFont *sf);
extern void SCConvertToOrder3 (SplineChar *sc);
extern void SFConvertToOrder3 (SplineFont *sf);
VISIBLE extern void SFConvertGridToOrder2 (SplineFont *_sf);
extern void SCConvertLayerToOrder2 (SplineChar *sc, int layer);
VISIBLE extern void SFConvertLayerToOrder2 (SplineFont *sf, int layer);
VISIBLE extern void SFConvertGridToOrder3 (SplineFont *_sf);
extern void SCConvertLayerToOrder3 (SplineChar *sc, int layer);
VISIBLE extern void SFConvertLayerToOrder3 (SplineFont *sf, int layer);
extern void SCConvertOrder (SplineChar *sc, int to_order2);
VISIBLE extern void SplinePointPrevCPChanged2 (SplinePoint *sp);
VISIBLE extern void SplinePointNextCPChanged2 (SplinePoint *sp);
extern int IntersectLinesSlopes (BasePoint *inter, BasePoint *line1,
                                 BasePoint *slope1, BasePoint *line2,
                                 BasePoint *slope2);
extern int IntersectLines (BasePoint *inter, BasePoint *line1_1,
                           BasePoint *line1_2, BasePoint *line2_1,
                           BasePoint *line2_2);
extern int IntersectLinesClip (BasePoint *inter, BasePoint *line1_1,
                               BasePoint *line1_2, BasePoint *line2_1,
                               BasePoint *line2_2);

#if 0
extern void SSBisectTurners (SplineSet *spl);
#endif
extern void SSRemoveBacktracks (SplineSet *ss);
VISIBLE extern enum PolyType PolygonIsConvex (BasePoint *poly, int n,
                                              int *badpointindex);
VISIBLE extern SplineSet *UnitShape (int isrect);
VISIBLE extern SplineSet *SplineSetStroke (SplineSet *spl, StrokeInfo *si,
                                           int order2);
VISIBLE extern SplineSet *SplineSetRemoveOverlap (SplineChar *sc,
                                                  SplineSet *base,
                                                  enum overlap_type);
VISIBLE extern SplineSet *SSShadow (SplineSet *spl, real angle,
                                    real outline_width, real shadow_length,
                                    SplineChar *sc, int wireframe);

extern double BlueScaleFigureForced (struct psdict *private_,
                                     real bluevalues[], real otherblues[]);
extern double BlueScaleFigure (struct psdict *private_, real bluevalues[],
                               real otherblues[]);
extern void FindBlues (SplineFont *sf, int layer, real blues[14],
                       real otherblues[10]);
VISIBLE extern void QuickBlues (SplineFont *sf, int layer, BlueData * bd);
extern void FindHStems (SplineFont *sf, real snaps[12], real cnt[12]);
extern void FindVStems (SplineFont *sf, real snaps[12], real cnt[12]);
extern double SFStdVW (SplineFont *sf);
VISIBLE extern int SplineCharIsFlexible (SplineChar *sc, int layer);
extern void SCGuessHintInstancesList (SplineChar *sc, int layer,
                                      StemInfo * hstem, StemInfo * vstem,
                                      DStemInfo * dstem, int hvforce,
                                      int dforce);
VISIBLE extern void SCGuessDHintInstances (SplineChar *sc, int layer,
                                           DStemInfo * ds);
VISIBLE extern void SCGuessHHintInstancesAndAdd (SplineChar *sc, int layer,
                                                 StemInfo * stem, real guess1,
                                                 real guess2);
VISIBLE extern void SCGuessVHintInstancesAndAdd (SplineChar *sc, int layer,
                                                 StemInfo * stem, real guess1,
                                                 real guess2);
extern void SCGuessHHintInstancesList (SplineChar *sc, int layer);
extern void SCGuessVHintInstancesList (SplineChar *sc, int layer);
extern real HIlen (StemInfo * stems);
extern real HIoverlap (HintInstance * mhi, HintInstance * thi);
extern int StemInfoAnyOverlaps (StemInfo * stems);
VISIBLE extern int StemListAnyConflicts (StemInfo * stems);
extern HintInstance *HICopyTrans (HintInstance * hi, real mul, real offset);
extern void MDAdd (SplineChar *sc, int x, SplinePoint *sp1, SplinePoint *sp2);
extern int SFNeedsAutoHint (SplineFont *_sf, int layer);

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

VISIBLE extern void InitGlobalInstrCt (GlobalInstrCt * gic, SplineFont *sf,
                                       int layer, BlueData * bd);
VISIBLE extern void FreeGlobalInstrCt (GlobalInstrCt * gic);
VISIBLE extern void NowakowskiSCAutoInstr (GlobalInstrCt * gic,
                                           SplineChar *sc);
extern void CVT_ImportPrivate (SplineFont *sf);

VISIBLE extern void SCModifyHintMasksAdd (SplineChar *sc, int layer,
                                          StemInfo * new_);
extern void SCClearHints (SplineChar *sc);
VISIBLE extern void SCClearHintMasks (SplineChar *sc, int layer,
                                      int counterstoo);
extern void SCFigureVerticalCounterMasks (SplineChar *sc);
VISIBLE extern void SCFigureCounterMasks (SplineChar *sc);
VISIBLE extern void SCFigureHintMasks (SplineChar *sc, int layer);
VISIBLE extern void _SplineCharAutoHint (SplineChar *sc, int layer,
                                         BlueData * bd, struct glyphdata *gd2,
                                         int gen_undoes);
VISIBLE extern void SplineCharAutoHint (SplineChar *sc, int layer,
                                        BlueData * bd);
extern void SFSCAutoHint (SplineChar *sc, int layer, BlueData * bd);
extern void SplineFontAutoHint (SplineFont *sf, int layer);
extern void SplineFontAutoHintRefs (SplineFont *sf, int layer);
extern StemInfo *HintCleanup (StemInfo * stem, int dosort,
                              int instance_count);
extern int SplineFontIsFlexible (SplineFont *sf, int layer, int flags);
VISIBLE extern int SCDrawsSomething (SplineChar *sc);
VISIBLE extern int SCWorthOutputting (SplineChar *sc);
VISIBLE extern int SFFindNotdef (SplineFont *sf, int fixed);
extern int doesGlyphExpandHorizontally (SplineChar *sc);
extern int IsntBDFChar (BDFChar *bdfc);
extern int CIDWorthOutputting (SplineFont *cidmaster, int enc); /* Returns -1 on failure, font number on success */
extern int AmfmSplineFont (FILE *afm, MMSet *mm, int formattype, EncMap *map,
                           int layer);
extern int AfmSplineFont (FILE *afm, SplineFont *sf, int formattype,
                          EncMap *map, int docc, SplineFont *fullsf,
                          int layer);
extern int PfmSplineFont (FILE *pfm, SplineFont *sf, int type0, EncMap *map,
                          int layer);
extern int TfmSplineFont (FILE *afm, SplineFont *sf, int formattype,
                          EncMap *map, int layer);
extern int OfmSplineFont (FILE *afm, SplineFont *sf, int formattype,
                          EncMap *map, int layer);
extern char *EncodingName (Encoding *map);
VISIBLE extern char *SFEncodingName (SplineFont *sf, EncMap *map);
extern void SFLigaturePrepare (SplineFont *sf);
extern void SFLigatureCleanup (SplineFont *sf);
VISIBLE extern void SFKernClassTempDecompose (SplineFont *sf, int isv);
VISIBLE extern void SFKernCleanup (SplineFont *sf, int isv);
extern int SCSetMetaData (SplineChar *sc, char *name, int unienc,
                          const char *comment);

extern enum uni_interp interp_from_encoding (Encoding *enc,
                                             enum uni_interp interp);
extern const char *EncName (Encoding *encname);
extern const char *FindUnicharName (void);
VISIBLE extern Encoding *_FindOrMakeEncoding (const char *name, int make_it);
VISIBLE extern Encoding *FindOrMakeEncoding (const char *name);
VISIBLE extern int SFDWrite (char *filename, SplineFont *sf, EncMap *map,
                             EncMap *normal, int todir);
VISIBLE extern int SFDWriteBak (SplineFont *sf, EncMap *map, EncMap *normal);
extern SplineFont *SFDRead (char *filename);
extern SplineFont *_SFDRead (char *filename, FILE *sfd);
extern SplineFont *SFDirRead (char *filename);
VISIBLE extern SplineChar *SFDReadOneChar (SplineFont *sf, const char *name);
extern char *TTFGetFontName (FILE *ttf, int32_t offset, int32_t off2);
extern void TTFLoadBitmaps (FILE *ttf, struct ttfinfo *info, int onlyone);

enum ttfflags
{
  ttf_onlystrikes = 1,
  ttf_onlyonestrike = 2,
  ttf_onlykerns = 4,
  ttf_onlynames = 8
};

extern SplineFont *_SFReadWOFF (FILE *woff, int flags,
                                enum openflags openflags, char *filename,
                                struct fontdict *fd);
extern SplineFont *_SFReadTTF (FILE *ttf, int flags, enum openflags openflags,
                               char *filename, struct fontdict *fd);
extern SplineFont *SFReadTTF (char *filename, int flags,
                              enum openflags openflags);
extern SplineFont *SFReadSVG (char *filename, int flags);
extern SplineFont *SFReadSVGMem (char *data, int flags);
extern SplineFont *SFReadUFO (char *filename, int flags);
extern SplineFont *_CFFParse (FILE *temp, int len, char *fontsetname);
extern SplineFont *CFFParse (char *filename);
extern SplineFont *SFReadMacBinary (char *filename, int flags,
                                    enum openflags openflags);
extern SplineFont *SFReadWinFON (char *filename, int toback);
extern SplineFont *SFReadPalmPdb (char *filename, int toback);
VISIBLE extern SplineFont *LoadSplineFont (char *filename, enum openflags);
VISIBLE extern SplineFont *_ReadSplineFont (FILE *file, char *filename,
                                            enum openflags openflags);
extern SplineFont *ReadSplineFont (char *filename, enum openflags);     /* Don't use this, use LoadSF instead */
VISIBLE extern FILE *URLToTempFile (char *url, void *lock);
extern int URLFromFile (char *url, FILE *from);
extern void ArchiveCleanup (char *archivedir);
extern char *Unarchive (char *name, char **_archivedir);
extern char *Decompress (char *name, int compression);
extern SplineFont *SFFromBDF (char *filename, int ispk, int toback);
extern SplineFont *SFFromMF (char *filename);
extern void SFCheckPSBitmap (SplineFont *sf);
VISIBLE extern uint16_t _MacStyleCode (char *styles, SplineFont *sf,
                                       uint16_t *psstyle);
VISIBLE extern uint16_t MacStyleCode (SplineFont *sf, uint16_t *psstyle);
extern SplineFont *SFReadIkarus (char *fontname);
extern SplineFont *_SFReadPdfFont (FILE *ttf, char *filename,
                                   enum openflags openflags);
extern SplineFont *SFReadPdfFont (char *filename, enum openflags openflags);
VISIBLE extern char **GetFontNames (char *filename);
extern char **NamesReadPDF (char *filename);
extern char **NamesReadSFD (char *filename);
extern char **NamesReadTTF (char *filename);
extern char **NamesReadCFF (char *filename);
extern char **NamesReadPostScript (char *filename);
extern char **_NamesReadPostScript (FILE *ps);
extern char **NamesReadSVG (char *filename);
extern char **NamesReadUFO (char *filename);
extern char **NamesReadMacBinary (char *filename);

extern void SFSetOrder (SplineFont *sf, int order2);
extern int SFFindOrder (SplineFont *sf);

VISIBLE extern const char *UnicodeRange (int unienc);
VISIBLE extern SplineChar *SCBuildDummy (SplineChar *dummy, SplineFont *sf,
                                         EncMap *map, int i);
VISIBLE extern SplineChar *SFMakeChar (SplineFont *sf, EncMap *map, int i);
extern char *AdobeLigatureFormat (char *name);
extern uint32_t LigTagFromUnicode (int uni);
extern void SCLigCaretheck (SplineChar *sc, int clean);
VISIBLE extern BDFChar *BDFMakeGID (BDFFont *bdf, int gid);
VISIBLE extern BDFChar *BDFMakeChar (BDFFont *bdf, EncMap *map, int enc);

VISIBLE extern RefChar *RefCharsCopyState (SplineChar *sc, int layer);
extern int SCWasEmpty (SplineChar *sc, int skip_this_layer);
VISIBLE extern void SCUndoSetLBearingChange (SplineChar *sc, int lb);
VISIBLE extern Undoes *SCPreserveHints (SplineChar *sc, int layer);
VISIBLE extern Undoes *SCPreserveLayer (SplineChar *sc, int layer,
                                        int dohints);
extern Undoes *_SCPreserveLayer (SplineChar *sc, int layer, int dohints);
VISIBLE extern Undoes *SCPreserveState (SplineChar *sc, int dohints);
VISIBLE extern Undoes *SCPreserveBackground (SplineChar *sc);
extern Undoes *SFPreserveGuide (SplineFont *sf);
extern Undoes *_SFPreserveGuide (SplineFont *sf);
VISIBLE extern Undoes *SCPreserveWidth (SplineChar *sc);
VISIBLE extern Undoes *SCPreserveVWidth (SplineChar *sc);
VISIBLE extern Undoes *BCPreserveState (BDFChar *bc);
VISIBLE extern void BCDoRedo (BDFChar *bc);
VISIBLE extern void BCDoUndo (BDFChar *bc);

extern int isaccent (int uni);
VISIBLE extern int SFIsCompositBuildable (SplineFont *sf, int unicodeenc,
                                          SplineChar *sc, int layer);
VISIBLE extern int SFIsSomethingBuildable (SplineFont *sf, SplineChar *sc,
                                           int layer, int onlyaccents);
VISIBLE extern int SFIsRotatable (SplineFont *sf, SplineChar *sc, int layer);
/*extern int SCMakeDotless(SplineFont *sf, SplineChar *dotless, int layer, int copybmp, int doit);*/
VISIBLE extern void SCBuildComposit (SplineFont *sf, SplineChar *sc,
                                     int layer, BDFFont *bmp, int disp_only);
extern int SCAppendAccent (SplineChar *sc, int layer, char *glyph_name,
                           int uni, uint32_t pos);
VISIBLE extern const uint32_t *SFGetAlternate (SplineFont *sf, int base,
                                               SplineChar *sc, int nocheck);

extern int getAdobeEnc (char *name);

VISIBLE extern void SFSplinesFromLayers (SplineFont *sf, int tostroke);
VISIBLE extern void SFSetLayerWidthsStroked (SplineFont *sf,
                                             real strokewidth);
extern SplineSet *SplinePointListInterpretSVG (char *filename, char *memory,
                                               int memlen, int em_size,
                                               int ascent, int stroked);
extern SplineSet *SplinePointListInterpretGlif (char *filename, char *memory,
                                                int memlen, int em_size,
                                                int ascent, int stroked);
#define UNDEFINED_WIDTH	-999999
extern SplinePointList *SplinePointListInterpretPS (FILE *ps, int flags,
                                                    int stroked, int *width);
extern void PSFontInterpretPS (FILE *ps, struct charprocs *cp,
                               char **encoding);
extern struct enc *PSSlurpEncodings (FILE *file);
VISIBLE extern int EvaluatePS (char *str, real *stack, int size);

struct pscontext
{
  int is_type2;
  int painttype;
  int instance_count;
  real blend_values[17];
  int blend_warn;
};

extern int UnblendedCompare (real u1[MmMax], real u2[MmMax], int cnt);
extern SplineChar *PSCharStringToSplines (uint8_t *type1, int len,
                                          struct pscontext *context,
                                          struct pschars *subrs,
                                          struct pschars *gsubrs,
                                          const char *name);
extern void MatMultiply (real m1[6], real m2[6], real to[6]);
extern int MatIsIdentity (real transform[6]);

VISIBLE extern int NameToEncoding (SplineFont *sf, EncMap *map,
                                   const char *uname);
VISIBLE extern void GlyphHashFree (SplineFont *sf);
extern void SFHashGlyph (SplineFont *sf, SplineChar *sc);
VISIBLE extern SplineChar *SFHashName (SplineFont *sf, const char *name);
VISIBLE extern int SFFindGID (SplineFont *sf, int unienc, const char *name);
VISIBLE extern int SFFindSlot (SplineFont *sf, EncMap *map, int unienc,
                               const char *name);
extern int SFCIDFindCID (SplineFont *sf, int unienc, const char *name);
VISIBLE extern SplineChar *SFGetChar (SplineFont *sf, int unienc,
                                      const char *name);
extern int SFHasChar (SplineFont *sf, int unienc, const char *name);
extern SplineChar *SFGetOrMakeChar (SplineFont *sf, int unienc,
                                    const char *name);
VISIBLE extern int SFFindExistingSlot (SplineFont *sf, int unienc,
                                       const char *name);
extern int SFCIDFindExistingChar (SplineFont *sf, int unienc,
                                  const char *name);
extern int SFHasCID (SplineFont *sf, int cid);

VISIBLE extern char *getUserCacheDir (void);
VISIBLE extern char *getUserConfigDir (void);
VISIBLE extern char *getUserDataDir (void);

VISIBLE extern void _DoAutoSaves (struct fontviewbase *);
VISIBLE extern void CleanAutoRecovery (void);
VISIBLE extern int DoAutoRecovery (int);
extern SplineFont *SFRecoverFile (char *autosavename, int inquire,
                                  int *state);
extern void SFAutoSave (SplineFont *sf, EncMap *map);
VISIBLE extern void SFClearAutoSave (SplineFont *sf);

extern void PSCharsFree (struct pschars *chrs);
VISIBLE extern void PSDictFree (struct psdict *chrs);
extern struct psdict *PSDictCopy (struct psdict *dict);
VISIBLE extern int PSDictFindEntry (struct psdict *dict, char *key);
VISIBLE extern char *PSDictHasEntry (struct psdict *dict, char *key);
extern int PSDictSame (struct psdict *dict1, struct psdict *dict2);
extern int PSDictRemoveEntry (struct psdict *dict, char *key);
VISIBLE extern int PSDictChangeEntry (struct psdict *dict, char *key,
                                      char *newval);
VISIBLE extern int SFPrivateGuess (SplineFont *sf, int layer,
                                   struct psdict *private_, char *name,
                                   int onlyone);

VISIBLE extern void SFRemoveLayer (SplineFont *sf, int l);
VISIBLE extern void SFAddLayer (SplineFont *sf, char *name, int order2,
                                int background);
VISIBLE extern void SFLayerSetBackground (SplineFont *sf, int layer,
                                          int is_back);

VISIBLE extern void SplineSetsRound2Int (SplineSet *spl, real factor,
                                         int inspiro, int onlysel);
VISIBLE extern void SCRound2Int (SplineChar *sc, int layer, real factor);
VISIBLE extern int SCRoundToCluster (SplineChar *sc, int layer, int sel,
                                     bigreal within, bigreal max);
extern int SplineSetsRemoveAnnoyingExtrema (SplineSet *ss, bigreal err);
VISIBLE extern int hascomposing (SplineFont *sf, int u, SplineChar *sc);
#if 0
extern void SFFigureGrid (SplineFont *sf);
#endif

struct cidmap;                  /* private structure to encoding.c */
VISIBLE extern int CIDFromName (char *name, SplineFont *cidmaster);
VISIBLE extern int CID2Uni (struct cidmap *map, int cid);
extern int CID2NameUni (struct cidmap *map, int cid, char *buffer, int len);
extern int NameUni2CID (struct cidmap *map, int uni, const char *name);
extern struct altuni *CIDSetAltUnis (struct cidmap *map, int cid);
VISIBLE extern int MaxCID (struct cidmap *map);
VISIBLE extern struct cidmap *LoadMapFromFile (char *file, char *registry,
                                               char *ordering,
                                               int supplement);
VISIBLE extern struct cidmap *FindCidMap (char *registry, char *ordering,
                                          int supplement, SplineFont *sf);
VISIBLE extern void SFEncodeToMap (SplineFont *sf, struct cidmap *map);
extern SplineFont *CIDFlatten (SplineFont *cidmaster, SplineChar **chars,
                               int charcnt);
VISIBLE extern void SFFlatten (SplineFont *cidmaster);
VISIBLE extern int SFFlattenByCMap (SplineFont *sf, char *cmapname);
VISIBLE extern SplineFont *MakeCIDMaster (SplineFont *sf, EncMap *oldmap,
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

VISIBLE extern void SCCopyLayerToLayer (SplineChar *sc, int from, int to,
                                        int doclear);

VISIBLE extern bool hasFreeType (void);
VISIBLE extern bool hasFreeTypeDebugger (void);
VISIBLE extern bool hasFreeTypeByteCode (void);
VISIBLE extern char *FreeTypeStringVersion (void);
extern void doneFreeType (void);
VISIBLE extern void *_FreeTypeFontContext (SplineFont *sf, SplineChar *sc,
                                           struct fontviewbase *fv, int layer,
                                           enum fontformat ff, int flags,
                                           void *shared_ftc);
VISIBLE extern void *FreeTypeFontContext (SplineFont *sf, SplineChar *sc,
                                          struct fontviewbase *fv, int layer);
VISIBLE extern BDFFont *SplineFontFreeTypeRasterize (void *freetypecontext,
                                                     int pixelsize,
                                                     int depth);
VISIBLE extern BDFChar *SplineCharFreeTypeRasterize (void *freetypecontext,
                                                     int gid, int ptsize,
                                                     int dpi, int depth);
VISIBLE extern void FreeTypeFreeContext (void *freetypecontext);
VISIBLE extern SplineSet *FreeType_GridFitChar (void *single_glyph_context,
                                                int enc, real ptsizey,
                                                real ptsizex, int dpi,
                                                uint16_t *width,
                                                SplineChar *sc, int depth,
                                                int scaled);
VISIBLE extern struct freetype_raster *FreeType_GetRaster (void
                                                           *single_glyph_context,
                                                           int enc,
                                                           real ptsizey,
                                                           real ptsizex,
                                                           int dpi,
                                                           int depth);
VISIBLE extern BDFChar *SplineCharFreeTypeRasterizeNoHints (SplineChar *sc,
                                                            int layer,
                                                            int ptsize,
                                                            int dpi,
                                                            int depth);
VISIBLE extern BDFFont *SplineFontFreeTypeRasterizeNoHints (SplineFont *sf,
                                                            int layer,
                                                            int pixelsize,
                                                            int depth);
VISIBLE extern void FreeType_FreeRaster (struct freetype_raster *raster);

struct TT_ExecContextRec_;

extern struct freetype_raster *DebuggerCurrentRaster (struct
                                                      TT_ExecContextRec_ *exc,
                                                      int depth);

VISIBLE extern int UniFromName (const char *name, enum uni_interp interp,
                                Encoding *encname);
VISIBLE extern const char *StdGlyphName (char *buffer, int uni,
                                         enum uni_interp interp,
                                         NameList * for_this_font);
VISIBLE extern char **AllGlyphNames (int uni, NameList * for_this_font,
                                     SplineChar *sc /* May be NULL */ );
VISIBLE extern char **AllNamelistNames (void);
VISIBLE extern NameList *DefaultNameListForNewFonts (void);
VISIBLE extern NameList *NameListByName (char *name);
VISIBLE extern NameList *LoadNamelist (char *filename);
VISIBLE extern void LoadNamelistDir (char *dir);
extern const char *RenameGlyphToNamelist (char *buffer, SplineChar *sc,
                                          NameList * old, NameList * new_,
                                          char **sofar);
VISIBLE extern void SFRenameGlyphsToNamelist (SplineFont *sf,
                                              NameList * new_);
VISIBLE extern char **SFTemporaryRenameGlyphsToNamelist (SplineFont *sf,
                                                         NameList * new_);
VISIBLE extern void SFTemporaryRestoreGlyphNames (SplineFont *sf,
                                                  char **former);

extern AnchorPos *AnchorPositioning (SplineChar *sc, uint32_t *ustr,
                                     SplineChar **sstr);
extern void AnchorPosFree (AnchorPos * apos);

extern int SF_CloseAllInstrs (SplineFont *sf);
extern int SSTtfNumberPoints (SplineSet *ss);
VISIBLE extern int SCNumberPoints (SplineChar *sc, int layer);
VISIBLE extern int SCPointsNumberedProperly (SplineChar *sc, int layer);
VISIBLE extern int ttfFindPointInSC (SplineChar *sc, int layer, int pnum,
                                     BasePoint *pos, RefChar *bound);

int SFFigureDefWidth (SplineFont *sf, int *_nomwid);

extern int SFRenameTheseFeatureTags (SplineFont *sf, uint32_t tag, int sli,
                                     int flags, uint32_t totag, int tosli,
                                     int toflags, int ismac);
extern int SFRemoveUnusedNestedFeatures (SplineFont *sf);

extern char *utf8_verify_copy (const char *str);

VISIBLE extern char *MacStrToUtf8 (const char *str, int macenc, int maclang);
VISIBLE extern char *Utf8ToMacStr (const char *ustr, int macenc, int maclang);
VISIBLE extern uint8_t MacEncFromMacLang (int maclang);
extern uint16_t WinLangFromMac (int maclang);
extern uint16_t WinLangToMac (int winlang);
extern int CanEncodingWinLangAsMac (int winlang);
extern const int32_t *MacEncToUnicode (int script, int lang);
extern int MacLangFromLocale (void);
extern char *MacLanguageFromCode (int code);
extern char *FindEnglishNameInMacName (struct macname *mn);
VISIBLE extern char *PickNameFromMacName (struct macname *mn);
extern MacFeat *FindMacFeature (SplineFont *sf, int feat,
                                MacFeat **secondary);
extern struct macsetting *FindMacSetting (SplineFont *sf, int feat, int set,
                                          struct macsetting **secondary);
extern struct macname *FindMacSettingName (SplineFont *sf, int feat, int set);

VISIBLE extern int32_t UniFromEnc (int enc, Encoding *encname);
VISIBLE extern int32_t EncFromUni (int32_t uni, Encoding *encname);
extern int32_t EncFromName (const char *name, enum uni_interp interp,
                            Encoding *encname);

extern void MatInverse (real into[6], real orig[6]);

VISIBLE extern int BpCollinear (BasePoint *first, BasePoint *mid,
                                BasePoint *last);
extern int BpWithin (BasePoint *first, BasePoint *mid, BasePoint *last);
    /* Collinear & between */

enum psstrokeflags
{
  /* sf_removeoverlap=2, */
  sf_handle_eraser = 4,
  sf_correctdir = 8,
  sf_clearbeforeinput = 16
};

extern char *MMAxisAbrev (char *axis_name);
extern char *MMMakeMasterFontname (MMSet *mm, int ipos, char **fullname);
extern char *MMGuessWeight (MMSet *mm, int ipos, char *def);
extern char *MMExtractNth (char *pt, int ipos);
extern char *MMExtractArrayNth (char *pt, int ipos);
VISIBLE extern int MMValid (MMSet *mm, int complain);
VISIBLE extern void MMKern (SplineFont *sf, SplineChar *first,
                            SplineChar *second, int diff,
                            struct lookup_subtable *sub, KernPair *oldkp);
VISIBLE extern char *MMBlendChar (MMSet *mm, int gid);

extern char *EnforcePostScriptName (char *old);

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

extern enum Compare_Ret BitmapCompare (BDFChar *bc1, BDFChar *bc2, int err,
                                       int bb_err);
extern enum Compare_Ret SSsCompare (const SplineSet *ss1,
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

VISIBLE extern int CompareFonts (SplineFont *sf1, EncMap *map1,
                                 SplineFont *sf2, FILE *diffs, int flags);
VISIBLE extern int LayersSimilar (Layer *ly1, Layer *ly2, double spline_err);

extern void DefaultOtherSubrs (void);
VISIBLE extern int ReadOtherSubrsFile (char *filename);

extern char *utf8toutf7_copy (const char *_str);
extern char *utf7toutf8_copy (const char *_str);

VISIBLE extern void SFSetModTime (SplineFont *sf);
extern void SFTimesFromFile (SplineFont *sf, FILE *);

VISIBLE extern int SFHasInstructions (SplineFont *sf);
extern int RefDepth (RefChar *ref, int layer);

extern SplineChar *SCHasSubs (SplineChar *sc, uint32_t tag);

VISIBLE extern char *TagFullName (SplineFont *sf, uint32_t tag, int ismac,
                                  int onlyifknown);

VISIBLE extern uint32_t *SFScriptsInLookups (SplineFont *sf, int gpos);
VISIBLE extern uint32_t *SFLangsInScript (SplineFont *sf, int gpos,
                                          uint32_t script);
VISIBLE extern uint32_t *SFFeaturesInScriptLang (SplineFont *sf, int gpos,
                                                 uint32_t script,
                                                 uint32_t lang);
extern OTLookup **SFLookupsInScriptLangFeature (SplineFont *sf, int gpos,
                                                uint32_t script,
                                                uint32_t lang,
                                                uint32_t feature);
extern SplineChar **SFGlyphsWithPSTinSubtable (SplineFont *sf,
                                               struct lookup_subtable
                                               *subtable);
extern SplineChar **SFGlyphsWithLigatureinLookup (SplineFont *sf,
                                                  struct lookup_subtable
                                                  *subtable);
VISIBLE extern void SFFindUnusedLookups (SplineFont *sf);
VISIBLE extern void SFFindClearUnusedLookupBits (SplineFont *sf);
VISIBLE extern int LookupUsedNested (SplineFont *sf, OTLookup *checkme);
extern void SFRemoveUnusedLookupSubTables (SplineFont *sf,
                                           int
                                           remove_incomplete_anchorclasses,
                                           int remove_unused_lookups);
VISIBLE extern void SFRemoveLookupSubTable (SplineFont *sf,
                                            struct lookup_subtable *sub);
VISIBLE extern void SFRemoveLookup (SplineFont *sf, OTLookup *otl);
VISIBLE extern struct lookup_subtable *SFFindLookupSubtable (SplineFont *sf,
                                                             char *name);
extern struct lookup_subtable *SFFindLookupSubtableAndFreeName (SplineFont
                                                                *sf,
                                                                char *name);
VISIBLE extern OTLookup *SFFindLookup (SplineFont *sf, char *name);
VISIBLE extern void NameOTLookup (OTLookup *otl, SplineFont *sf);
VISIBLE extern int GlyphNameCnt (const char *pt);
extern char *reverseGlyphNames (char *str);
VISIBLE extern char *FPSTRule_From_Str (SplineFont *sf, FPST *fpst,
                                        struct fpst_rule *rule, char *line,
                                        int *return_is_warning);
VISIBLE extern char *FPSTRule_To_Str (SplineFont *sf, FPST *fpst,
                                      struct fpst_rule *rule);
extern void FListAppendScriptLang (FeatureScriptLangList *fl,
                                   uint32_t script_tag, uint32_t lang_tag);
extern void FListsAppendScriptLang (FeatureScriptLangList *fl,
                                    uint32_t script_tag, uint32_t lang_tag);
struct scriptlanglist *SLCopy (struct scriptlanglist *sl);
struct scriptlanglist *SListCopy (struct scriptlanglist *sl);
extern FeatureScriptLangList *FeatureListCopy (FeatureScriptLangList *fl);
extern void SLMerge (FeatureScriptLangList *into, struct scriptlanglist *fsl);
VISIBLE extern void FLMerge (OTLookup *into, OTLookup *from);
VISIBLE extern FeatureScriptLangList *FLOrder (FeatureScriptLangList *fl);
VISIBLE extern int FeatureScriptTagInFeatureScriptList (uint32_t tag,
                                                        uint32_t script,
                                                        FeatureScriptLangList
                                                        *fl);
VISIBLE extern FeatureScriptLangList
  *FindFeatureTagInFeatureScriptList (uint32_t tag,
                                      FeatureScriptLangList *fl);
VISIBLE extern int FeatureTagInFeatureScriptList (uint32_t tag,
                                                  FeatureScriptLangList *fl);
extern int DefaultLangTagInOneScriptList (struct scriptlanglist *sl);
extern struct scriptlanglist *DefaultLangTagInScriptList (struct
                                                          scriptlanglist *sl,
                                                          int DFLT_ok);
VISIBLE extern int ScriptInFeatureScriptList (uint32_t script,
                                              FeatureScriptLangList *fl);
VISIBLE extern int _FeatureOrderId (int isgpos, uint32_t tag);
VISIBLE extern int FeatureOrderId (int isgpos, FeatureScriptLangList *fl);
VISIBLE extern void SFSubTablesMerge (SplineFont *_sf,
                                      struct lookup_subtable *subfirst,
                                      struct lookup_subtable *subsecond);
extern struct lookup_subtable *SFSubTableFindOrMake (SplineFont *sf,
                                                     uint32_t tag,
                                                     uint32_t script,
                                                     int lookup_type);
extern struct lookup_subtable *SFSubTableMake (SplineFont *sf, uint32_t tag,
                                               uint32_t script,
                                               int lookup_type);
extern OTLookup *OTLookupCopyInto (SplineFont *into_sf, SplineFont *from_sf,
                                   OTLookup *from_otl);
VISIBLE extern void OTLookupsCopyInto (SplineFont *into_sf,
                                       SplineFont *from_sf,
                                       OTLookup **from_list,
                                       OTLookup *before);
VISIBLE extern struct opentype_str *ApplyTickedFeatures (SplineFont *sf,
                                                         uint32_t *flist,
                                                         uint32_t script,
                                                         uint32_t lang,
                                                         int pixelsize,
                                                         SplineChar **glyphs);
VISIBLE extern int VerticalKernFeature (SplineFont *sf, OTLookup *otl,
                                        int ask);
VISIBLE extern void SFGlyphRenameFixup (SplineFont *sf, char *old,
                                        char *new_);

struct sllk
{
  uint32_t script;
  int cnt, max;
  OTLookup **lookups;
  int lcnt, lmax;
  uint32_t *langs;
};

VISIBLE extern void SllkFree (struct sllk *sllk, int sllk_cnt);
VISIBLE extern struct sllk *AddOTLToSllks (OTLookup *otl, struct sllk *sllk,
                                           int *_sllk_cnt, int *_sllk_max);
VISIBLE extern OTLookup *NewAALTLookup (SplineFont *sf, struct sllk *sllk,
                                        int sllk_cnt, int i);
extern void AddNewAALTFeatures (SplineFont *sf);

VISIBLE extern void SplinePointRound (SplinePoint *, real);

VISIBLE extern int KCFindName (char *name, char **classnames, int cnt,
                               int allow_class0);
extern KernClass *SFFindKernClass (SplineFont *sf, SplineChar *first,
                                   SplineChar *last, int *index,
                                   int allow_zero);
extern KernClass *SFFindVKernClass (SplineFont *sf, SplineChar *first,
                                    SplineChar *last, int *index,
                                    int allow_zero);

extern void SCClearRounds (SplineChar *sc, int layer);
VISIBLE extern void SCSynchronizeWidth (SplineChar *sc, real newwidth,
                                        real oldwidth,
                                        struct fontviewbase *fv);
VISIBLE extern RefChar *HasUseMyMetrics (SplineChar *sc, int layer);
VISIBLE extern void SCSynchronizeLBearing (SplineChar *sc, real off,
                                           int layer);
VISIBLE extern void RevertedGlyphReferenceFixup (SplineChar *sc,
                                                 SplineFont *sf);

VISIBLE extern void SFUntickAll (SplineFont *sf);

VISIBLE extern void BDFOrigFixup (BDFFont *bdf, int orig_cnt, SplineFont *sf);

VISIBLE extern void SCImportSVG (SplineChar *sc, int layer, char *path,
                                 char *memory, int memlen, int doclear);
VISIBLE extern void SCImportGlif (SplineChar *sc, int layer, char *path,
                                  char *memory, int memlen, int doclear);
extern void SCImportPS (SplineChar *sc, int layer, char *path, int doclear,
                        int flags);
VISIBLE extern void SCImportPSFile (SplineChar *sc, int layer, FILE *ps,
                                    int doclear, int flags);
extern void SCImportPDF (SplineChar *sc, int layer, char *path, int doclear,
                         int flags);
VISIBLE extern void SCImportPDFFile (SplineChar *sc, int layer, FILE *ps,
                                     int doclear, int flags);
VISIBLE extern void SCImportPlateFile (SplineChar *sc, int layer, FILE *plate,
                                       int doclear, int flags);
VISIBLE extern void SCAddScaleImage (SplineChar *sc, struct gimage *image,
                                     int doclear, int layer);
extern void SCInsertImage (SplineChar *sc, struct gimage *image, real scale,
                           real yoff, real xoff, int layer);
VISIBLE extern void SCImportFig (SplineChar *sc, int layer, char *path,
                                 int doclear);

VISIBLE extern int _ExportPlate (FILE *pdf, SplineChar *sc, int layer);
VISIBLE extern int _ExportPDF (FILE *pdf, SplineChar *sc, int layer);
VISIBLE extern int _ExportEPS (FILE *eps, SplineChar *sc, int layer,
                               int gen_preview);
VISIBLE extern int _ExportSVG (FILE *svg, SplineChar *sc, int layer);
VISIBLE extern int _ExportGlif (FILE *glif, SplineChar *sc, int layer);
VISIBLE extern int ExportEPS (char *filename, SplineChar *sc, int layer);
VISIBLE extern int ExportPDF (char *filename, SplineChar *sc, int layer);
VISIBLE extern int ExportPlate (char *filename, SplineChar *sc, int layer);
VISIBLE extern int ExportSVG (char *filename, SplineChar *sc, int layer);
VISIBLE extern int ExportGlif (char *filename, SplineChar *sc, int layer);
VISIBLE extern int ExportFig (char *filename, SplineChar *sc, int layer);
VISIBLE extern int BCExportXBM (char *filename, BDFChar *bdfc, int format);
VISIBLE extern int ExportImage (char *filename, SplineChar *sc, int layer,
                                int format, int pixelsize, int bitsperpixel);
extern void ScriptExport (SplineFont *sf, BDFFont *bdf, int format, int gid,
                          char *format_spec, EncMap *map);

VISIBLE extern EncMap *EncMapFromEncoding (SplineFont *sf, Encoding *enc);
extern void SFRemoveGlyph (SplineFont *sf, SplineChar *sc, int *flags);
extern void SFAddEncodingSlot (SplineFont *sf, int gid);
VISIBLE extern void SFAddGlyphAndEncode (SplineFont *sf, SplineChar *sc,
                                         EncMap *basemap, int baseenc);
VISIBLE extern void SCDoRedo (SplineChar *sc, int layer);
VISIBLE extern void SCDoUndo (SplineChar *sc, int layer);
VISIBLE extern void SCCopyWidth (SplineChar *sc, enum undotype);
extern void SCAppendPosSub (SplineChar *sc, enum possub_type type, char **d,
                            SplineFont *copied_from);
VISIBLE extern void SCClearBackground (SplineChar *sc);
VISIBLE extern void BackgroundImageTransform (SplineChar *sc, ImageList *img,
                                              real transform[6]);
VISIBLE extern int SFIsDuplicatable (SplineFont *sf, SplineChar *sc);

VISIBLE extern void DoAutoSaves (void);

extern void SCClearLayer (SplineChar *sc, int layer);
VISIBLE extern void SCClearContents (SplineChar *sc, int layer);
VISIBLE extern void SCClearAll (SplineChar *sc, int layer);
VISIBLE extern void BCClearAll (BDFChar *bc);

#if !defined(_NO_PYTHON)
extern void FontForge_PythonInit (void);
extern void PyFF_ErrorString (const char *msg, const char *str);
extern void PyFF_ErrorF3 (const char *frmt, const char *str, int size,
                          int depth);
VISIBLE extern void PyFF_Stdin (void);
extern void PyFF_Main (int argc, char **argv, int start);
extern void PyFF_ScriptFile (struct fontviewbase *fv, SplineChar *sc,
                             char *filename);
VISIBLE extern void PyFF_ScriptString (struct fontviewbase *fv,
                                       SplineChar *sc, int layer, char *str);
VISIBLE extern void PyFF_FreeFV (struct fontviewbase *fv);
extern void PyFF_FreeSC (SplineChar *sc);
extern void PyFF_FreeSF (SplineFont *sf);
VISIBLE extern void PyFF_ProcessInitFiles (void);
extern char *PyFF_PickleMeToString (void *pydata);
extern void *PyFF_UnPickleMeToObjects (char *str);
struct _object;                 /* Python Object */
extern void PyFF_CallDictFunc (struct _object *dict, char *key,
                               char *argtypes, ...);
VISIBLE extern void ff_init (void);
extern struct _object *ff_init_py3 (int);
#endif
extern void doinitFontForgeMain (void);

VISIBLE extern void InitSimpleStuff (void);

VISIBLE extern int SSExistsInLayer (SplineSet *ss, SplineSet *lots);
VISIBLE extern int SplineExistsInSS (Spline * s, SplineSet *ss);
extern int SpExistsInSS (SplinePoint *sp, SplineSet *ss);

VISIBLE extern int MSLanguageFromLocale (void);

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

VISIBLE extern int BPTooFar (BasePoint *bp1, BasePoint *bp2);
VISIBLE extern StemInfo *SCHintOverlapInMask (SplineChar *sc, HintMask * hm);
extern char *VSErrorsFromMask (int mask, int private_mask);
VISIBLE extern int SCValidate (SplineChar *sc, int layer, int force);
VISIBLE extern AnchorClass *SCValidateAnchors (SplineChar *sc);
VISIBLE extern void SCTickValidationState (SplineChar *sc, int layer);
VISIBLE extern int ValidatePrivate (SplineFont *sf);
VISIBLE extern int SFValidate (SplineFont *sf, int layer, int force);
VISIBLE extern int VSMaskFromFormat (SplineFont *sf, int layer,
                                     enum fontformat format);

extern SplineSet *SpiroCP2SplineSet (spiro_cp *spiros);
VISIBLE extern spiro_cp *SplineSet2SpiroCP (SplineSet *ss, uint16_t *_cnt);
extern spiro_cp *SpiroCPCopy (spiro_cp *spiros, uint16_t *_cnt);
VISIBLE extern void SSRegenerateFromSpiros (SplineSet *spl);

struct lang_frequencies;

VISIBLE extern uint32_t *PrtBuildDef (SplineFont *sf, void *tf,
                                      void (*langsyscallback) (void *tf,
                                                               int end,
                                                               uint32_t
                                                               script,
                                                               uint32_t
                                                               lang));
VISIBLE extern char *RandomParaFromScriptLang (uint32_t script, uint32_t lang,
                                               SplineFont *sf,
                                               struct lang_frequencies *freq);
extern char *RandomParaFromScript (uint32_t script, uint32_t *lang,
                                   SplineFont *sf);
extern int SF2Scripts (SplineFont *sf, uint32_t scripts[100]);
VISIBLE extern char **SFScriptLangs (SplineFont *sf,
                                     struct lang_frequencies ***freq);

extern int SSHasClip (SplineSet *ss);
extern int SSHasDrawn (SplineSet *ss);
VISIBLE extern struct gradient *GradientCopy (struct gradient *old,
                                              real transform[6]);
VISIBLE extern void GradientFree (struct gradient *grad);
VISIBLE extern struct pattern *PatternCopy (struct pattern *old,
                                            real transform[6]);
VISIBLE extern void PatternFree (struct pattern *pat);
extern void BrushCopy (struct brush *into, struct brush *from,
                       real transform[6]);
extern void PenCopy (struct pen *into, struct pen *from, real transform[6]);
VISIBLE extern void PatternSCBounds (SplineChar *sc, DBounds *b);

VISIBLE extern char *SFDefaultImage (SplineFont *sf, char *filename);
extern void SCClearInstrsOrMark (SplineChar *sc, int layer, int complain);
VISIBLE extern void instrcheck (SplineChar *sc, int layer);
VISIBLE extern void TTFPointMatches (SplineChar *sc, int layer, int top);

extern bigreal SFCapHeight (SplineFont *sf, int layer, int return_error);
extern bigreal SFXHeight (SplineFont *sf, int layer, int return_error);
extern bigreal SFAscender (SplineFont *sf, int layer, int return_error);
extern bigreal SFDescender (SplineFont *sf, int layer, int return_error);

extern SplineChar ***GlyphClassesFromNames (SplineFont *sf, char **classnames,
                                            int class_cnt);
#endif
