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

/* Copyright (C) 2001-2012 by George Williams */
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

#include "psfont.h"             /* for struct fddata */

#define MAC_DELETED_GLYPH_NAME	"<Delete>"

/* Some glyphs have multiple encodings ("A" might be used for Alpha and Cyrillic A) */
struct dup
{
  SplineChar *sc;
  int enc;
  int uni;
  struct dup *prev;
};

enum gsub_inusetype
{ git_normal, git_justinuse, git_findnames };

struct savetab
{
  uint32_t tag;
  uint32_t offset;
  int len;
};

struct ttfinfo
{
  int emsize;                   /* ascent + descent? from the head table */
  int ascent, descent;          /* from the hhea table */
  /* not the usWinAscent from the OS/2 table */
  int vertical_origin;          /* if vmetrics are present */
  int width_cnt;                /* from the hhea table, in the hmtx table */
  int glyph_cnt;                /* from maxp table (or cff table) */
  bool index_to_loc_is_long;    /* in head table */
  bool is_ttc;                  /* Is it a font collection? */
  bool is_onebyte;              /* Is it a one byte encoding? */
  bool twobytesymbol;           /* it had a symbol encoding which we converted to unicode */
  bool complainedbeyondglyfend; /* Don't complain about this more than once */
  bool extensionrequested;      /* Only ask once for a copy of a font containing extension subtables */
  bool to_order2;               /* We are to leave the font as truetype (order2) splines, else convert to ps */
  bool complainedmultname;      /* Don't complain about this more than once */
  bool strokedfont;             /* painttype==2 for otf */
  bool use_typo_metrics;
  bool weight_width_slope_only;
  bool optimized_for_cleartype;
  bool apply_lsb;
  int sfntRevision;
  enum openflags openflags;
  /* Mac fonts platform=0/1, platform specific enc id, roman=0, english is lang code 0 */
  /* iso platform=2, platform specific enc id, latin1=0/2, no language */
  /* microsoft platform=3, platform specific enc id, 1, english is lang code 0x??09 */
  char *copyright;              /* from the name table, nameid=0 */
  char *familyname;             /* nameid=1 */
  char *fullname;               /* nameid=4 */
  char *weight;
  char *version;                /* nameid=5 */
  char *fontname;               /* postscript font name, nameid=6 */
  char *xuid;                   /* Only for open type cff fonts */
  int uniqueid;
  real italicAngle;             /* from post table */
  int upos, uwidth;             /* underline pos, width from post table */
  real strokewidth;
  int fstype;
  struct psdict *private;       /* Only for open type cff fonts */
  EncMap *map;
  enum uni_interp uni_interp;
  struct pfminfo pfminfo;
  short os2_version;
  short gasp_version;
  int dupnamestate;
  struct ttflangname *names;
  char *fontcomments, *fontlog;
  char **cvt_names;
  SplineChar **chars;           /* from all over, glyf table for contours */
  /*                cmap table for encodings */
  /*                hmtx table for widths */
  /*                post table for names */
  /* Or from        CFF  table for everything in opentype */
  LayerInfo *layers;
  int layer_cnt;
  BDFFont *bitmaps;
  char *cidregistry, *ordering;
  int supplement;
  real cidfontversion;
  int subfontcnt;
  SplineFont **subfonts;
  char *inuse;                  /* What glyphs are used by this font in the ttc */

  int numtables;
  /* BASE  */
  uint32_t base_start;          /* Offset from sof to start of 'BASE' table */
  /* CFF  */
  uint32_t cff_start;           /* Offset from sof to start of postscript compact font format */
  uint32_t cff_length;
  /* cmap */
  uint32_t encoding_start;      /* Offset from sof to start of encoding table */
  uint32_t vs_start;            /* Offset within 'cmap' to variant selector table */
  /* gasp */
  uint32_t gasp_start;
  /* glyf */
  uint32_t glyph_start;         /* Offset from sof to start of glyph table */
  uint32_t glyph_length;
  /* GDEF */
  uint32_t gdef_start;          /* Offset from sof to start of GDEF table (glyph class defn, ligature carets) */
  uint32_t gdef_length;
  /* GPOS */
  uint32_t gpos_start;          /* Offset from sof to start of GPOS table */
  uint32_t gpos_length;
  /* GSUB */
  uint32_t gsub_start;          /* Offset from sof to start of GSUB table */
  uint32_t gsub_length;
  uint32_t g_bounds;            /* Filled in with g???_start+g???_length */
  /* EBDT, bdat */
  uint32_t bitmapdata_start;    /* Offset to start of bitmap data */
  uint32_t bitmapdata_length;
  /* EBLT, bloc */
  uint32_t bitmaploc_start;     /* Offset to start of bitmap locator data */
  uint32_t bitmaploc_length;
  /* head */
  uint32_t head_start;
  /* hhea */
  uint32_t hhea_start;
  /* hmtx */
  uint32_t hmetrics_start;
  /* JSTF */
  uint32_t jstf_start;
  uint32_t jstf_length;
  /* kern */
  uint32_t kern_start;
  /* loca */
  uint32_t glyphlocations_start;        /* there are glyph_cnt of these, from maxp tab */
  uint32_t loca_length;         /* actually glypn_cnt is wrong. Use the table length (divided by size) instead */
  /* maxp */
  uint32_t maxp_start;          /* maximum number of glyphs */
  uint32_t maxp_len;
  /* name */
  uint32_t copyright_start;     /* copyright and fontname */
  /* post */
  uint32_t postscript_start;    /* names for the glyphs, italic angle, etc. */
  /* OS/2 */
  uint32_t os2_start;
  /* TYP1 */
  uint32_t typ1_start;          /* For Adobe's? Apple's? attempt to stuff a type1 font into an sfnt wrapper */
  uint32_t typ1_length;
  /* vhea */
  uint32_t vhea_start;
  /* vmtx */
  uint32_t vmetrics_start;
  /* VORG */
  uint32_t vorg_start;

  /* PfEd -- FontForge/PfaEdit specific info */
  uint32_t pfed_start;
  /* TeX  -- TeX table, also non-standard */
  uint32_t tex_start;
  /* BDF  -- BDF properties, also non-standard */
  uint32_t bdf_start;
  /* FFTM -- FontForge timestamps */
  uint32_t fftm_start;

  /* MATH Table */
  uint32_t math_start;
  uint32_t math_length;

  /* Info for instructions */
  uint32_t cvt_start, cvt_len;
  uint32_t prep_start, prep_len;
  uint32_t fpgm_start, fpgm_len;

  bool one_of_many;             /* A TTCF file, or a opentype font with multiple fonts */
  bool obscomplain;             /* We've complained about obsolete format 3 in EBDT table */
  bool cmpcomplain;             /* We've complained about compressed format 4 in EBDT */
  bool unkcomplain;             /* We've complained about unknown formats in EBDT */
  bool comcomplain;             /* We've complained about composit formats in EBDT */
  bool onlystrikes;             /* Only read in the bitmaps, not the outlines */
  bool onlyonestrike;           /* Only read in one bitmap (strike) */
  bool barecff;                 /* pay attention to the encoding in the cff file, we won't have a cmap */
  bool wdthcomplain;            /* We've complained about advance widths exceding the max */
  bool bbcomplain;              /* We've complained about glyphs being outside the bounding box */
  bool gbbcomplain;             /* We've complained about points being outside the bounding box */

  int platform, specific;       /* values of the encoding we chose to use */

  int anchor_class_cnt;         /* For GPOS */
  int anchor_merge_cnt;
  AnchorClass *ahead, *alast;

  KernClass *khead, *klast, *vkhead, *vklast;

  OTLookup *gpos_lookups, *gsub_lookups, *cur_lookups;

  struct ttf_table *tabs;
  FPST *possub;
  char *chosenname;
  int macstyle;
  int lookup_cnt;               /* Max lookup in current GPOS/GSUB table */
  int feature_cnt;              /* Max feature in current GPOS/GSUB table */
  struct fontdict *fd;          /* For reading in Type42 fonts. Glyph names in postscript section must be associated with glyphs in TTF section */
  int savecnt;
  struct savetab *savetab;
  int32_t last_size_pos;
  uint16_t design_size;
  uint16_t fontstyle_id;
  struct otfname *fontstyle_name;
  uint16_t design_range_bottom, design_range_top;
  struct texdata texdata;
  int mark_class_cnt;
  char **mark_classes;          /* glyph name list */
  char **mark_class_names;      /* used within ff (utf8) */
  int mark_set_cnt;
  char **mark_sets;             /* glyph name list */
  char **mark_set_names;        /* used within ff (utf8) */
  SplineChar **badgids;         /* which use out of range glyph IDs as temporary flags */
#ifdef HAVE_LONG_LONG_INT
  long long creationtime;       /* seconds since 1970 */
  long long modificationtime;
#else
  long creationtime;
  long modificationtime;
#endif
  int gasp_cnt;
  struct gasp *gasp;
  struct MATH *math;
  /* Set of errors we found when loading the font */
  bool bad_ps_fontname;
  bool bad_glyph_data;
  bool bad_cff;
  bool bad_metrics;
  bool bad_cmap;
  bool bad_embedded_bitmap;
  bool bad_gx;
  bool bad_ot;
  bool bad_os2_version;
  bool bad_sfnt_header;
  Layer guidelines;
  struct Base *horiz_base, *vert_base;
  Justify *justify;

  int advanceWidthMax;
  int fbb[4];                   /* x,yMin x,yMax */
  int isFixedPitch;

  uint32_t jstf_script;
  uint32_t jstf_lang;
  int16_t jstf_isShrink, jstf_prio, jstf_lcnt;
  struct otffeatname *feat_names;
  enum gsub_inusetype justinuse;
};

struct taboff
{
  uint32_t tag;                 /* Table name */
  uint32_t checksum;            /* for table */
  uint32_t offset;              /* to start of table in file */
  uint32_t length;
  FILE *data;
  uint16_t dup_of;
  uint16_t orderingval;
};

#define MAX_TAB	48
struct tabdir
{
  int32_t version;              /* 0x00010000 */
  uint16_t numtab;
  uint16_t searchRange;         /* (Max power of 2 <= numtab) *16 */
  uint16_t entrySel;            /* Log2(Max power of 2 <= numtab ) */
  uint16_t rangeShift;          /* numtab*16 - searchRange */
  struct taboff tabs[MAX_TAB];  /* room for all the tables */
  /* Not in any particular order. */
  struct taboff *ordered[MAX_TAB];      /* Ordered the way the tables should be output in file */
  struct taboff *alpha[MAX_TAB];        /* Ordered alphabetically by tag for the ttf header */
};

struct glyphhead
{
  int16_t numContours;
  int16_t xmin;
  int16_t ymin;
  int16_t xmax;
  int16_t ymax;
};

struct head
{
  int32_t version;              /* 0x00010000 */
  int32_t revision;             /* 0 */
  uint32_t checksumAdj;         /* set to 0, sum entire font, store 0xb1b0afba-sum */
  uint32_t magicNum;            /* 0x5f0f3cf5 */
  uint16_t flags;               /* 1 */
  uint16_t emunits;             /* sf->ascent+sf->descent */
  int32_t createtime[2];        /* number of seconds since 1904 */
  int32_t modtime[2];
  int16_t xmin;                 /* min for entire font */
  int16_t ymin;
  int16_t xmax;
  int16_t ymax;
  uint16_t macstyle;            /* 1=>Bold, 2=>Italic */
  uint16_t lowestreadable;      /* size in pixels. Say about 10? */
  int16_t dirhint;              /* 0=>mixed directional characters, */
  int16_t locais32;             /* is the location table 32bits or 16, 0=>16, 1=>32 */
  int16_t glyphformat;          /* 0 */
  uint16_t mbz;                 /* padding */
};

struct hhead
{
  int32_t version;              /* 0x00010000 */
  int16_t ascender;             /* sf->ascender */
  int16_t descender;            /* -sf->descender */
  int16_t linegap;              /* 0 */
  int16_t maxwidth;             /* of all characters */
  int16_t minlsb;               /* How is this different from xmin above? */
  int16_t minrsb;
  int16_t maxextent;            /* How is this different from xmax above? */
  int16_t caretSlopeRise;       /* Uh... let's say 1? */
  int16_t caretSlopeRun;        /* Uh... let's say 0 */
  /* not exactly specified, but FontValidator wants this to match italicangle */
  int16_t mbz[5];
  int16_t metricformat;         /* 0 */
  uint16_t numMetrics;          /* just set to glyph count */
};

struct hmtx
{
  uint16_t width;               /* NOTE: TTF only allows positive widths!!! */
  int16_t lsb;
};

struct kp
{
  uint16_t left;                /* left glyph num */
  uint16_t right;               /* right glyph num */
  /* table is ordered by these two above treated as uint32_t */
  int16_t offset;               /* kern amount */
};

struct kern
{
  uint16_t version;             /* 0 */
  uint16_t ntab;                /* 1, number of subtables */
  /* first (and only) subtable */
  uint16_t stversion;           /* 0 */
  uint16_t length;              /* length of subtable beginning at &stversion */
  uint16_t coverage;            /* 1, (set of flags&format) */
  uint16_t nPairs;              /* number of kern pairs */
  uint16_t searchRange;         /* (Max power of 2 <= nPairs) *6 */
  uint16_t entrySel;            /* Log2(Max power of 2 <= nPairs ) */
  uint16_t rangeShift;          /* numtab*6 - searchRange */
  struct kp *kerns;             /* Array should be nPairs big */
};

struct maxp
{
  int32_t version;              /* 0x00010000 */
  uint16_t numGlyphs;
  uint16_t maxPoints;           /* max number of points in a simple glyph */
  uint16_t maxContours;         /* max number of paths in a simple glyph */
  uint16_t maxCompositPts;
  uint16_t maxCompositCtrs;
  uint16_t maxZones;            /* 1 */
  uint16_t maxTwilightPts;      /* 0 */
  uint16_t maxStorage;          /* 0 */
  uint16_t maxFDEFs;            /* 0 */
  uint16_t maxIDEFs;            /* 0 */
  uint16_t maxStack;            /* 0 */
  uint16_t maxglyphInstr;       /* 0 */
  uint16_t maxnumcomponents;    /* Maximum number of refs in any composit */
  uint16_t maxcomponentdepth;
  /* Apple docs say: 0 (if no composits), maximum value 1 (one level of composit) */
  /* OpenType docs say: 1 (if no composits), any depth allowed */
};

struct namerec
{
  uint16_t platform;            /* 3 => MS */
  uint16_t specific;            /* 1 */
  uint16_t language;            /* 0x0409 */
  uint16_t nameid;              /* 0=>copyright, 1=>family, 2=>weight, 4=>fullname */
  /*  5=>version, 6=>postscript name */
  uint16_t strlen;
  uint16_t stroff;
};

struct nametab
{
  uint16_t format;              /* 0 */
  uint16_t numrec;              /* 1 */
  uint16_t startOfStrings;      /* offset from start of table to start of strings */
  struct namerec nr[6];
};

struct os2
{
  uint16_t version;             /* 1 */
  int16_t avgCharWid;           /* average all chars (v3) see v2 definition below */
  uint16_t weightClass;         /* 100=>thin, 200=>extra-light, 300=>light, 400=>normal, */
  /* 500=>Medium, 600=>semi-bold, 700=>bold, 800=>extra-bold, */
  /* 900=>black */
  uint16_t widthClass;          /* 75=>condensed, 100, 125=>expanded */
  int16_t fstype;               /* 0x0008 => allow embedded editing */
  int16_t ysubXSize;            /* emsize/5 */
  int16_t ysubYSize;            /* emsize/5 */
  int16_t ysubXOff;             /* 0 */
  int16_t ysubYOff;             /* emsize/5 */
  int16_t ysupXSize;            /* emsize/5 */
  int16_t ysupYSize;            /* emsize/5 */
  int16_t ysupXOff;             /* 0 */
  int16_t ysupYOff;             /* emsize/5 */
  int16_t yStrikeoutSize;       /* 102/2048 *emsize */
  int16_t yStrikeoutPos;        /* 530/2048 *emsize */
  int16_t sFamilyClass;         /* ??? 0 */
  /* high order byte is the "class", low order byte the sub class */
  /* class = 0 => no classification */
  /* class = 1 => old style serifs */
  /*      subclass 0, no class; 1 ibm rounded; 2 garalde; 3 venetian; 4 mod venitian; 5 dutch modern; 6 dutch trad; 7 contemporary; 8 caligraphic; 15 misc */
  /* class = 2 => transitional serifs */
  /*      subclass 0, no class; 1 drect line; 2 script; 15 misc */
  /* class = 3 => modern serifs */
  /*      subclass: 1, italian; 2, script */
  /* class = 4 => clarendon serifs */
  /*      subclass: 1, clarendon; 2, modern; 3 trad; 4 newspaper; 5 stub; 6 monotone; 7 typewriter */
  /* class = 5 => slab serifs */
  /*      subclass: 1, monotone; 2, humanist; 3 geometric; 4 swiss; 5 typewriter */
  /* class = 7 => freeform serifs */
  /*      subclass: 1, modern */
  /* class = 8 => sans serif */
  /*      subclass: 1, ibm neogrotesque; 2 humanist; 3 low-x rounded; 4 high-x rounded; 5 neo-grotesque; 6 mod neo-grot; 9 typewriter; 10 matrix */
  /* class = 9 => ornamentals */
  /*      subclass: 1, engraver; 2 black letter; 3 decorative; 4 3D */
  /* class = 10 => scripts */
  /*      subclass: 1, uncial; 2 brush joined; 3 formal joined; 4 monotone joined; 5 calligraphic; 6 brush unjoined; 7 formal unjoined; 8 monotone unjoined */
  /* class = 12 => symbolic */
  /*      subclass: 3 mixed serif; 6 old style serif; 7 neo-grotesque sans; */
  char panose[10];              /* can be set to zero */
  uint32_t unicoderange[4];
  /* 1<<0=>ascii, 1<<1 => latin1, 2=>100-17f, 3=>180-24f, 4=>250-2af */
  /* 5=> 2b0-2ff, 6=>300-36f, ... */
  char achVendID[4];            /* can be zero */
  uint16_t fsSel;               /* 1=> italic, 32=>bold, 64 => regular */
  /* 2=>underscore, 4=>negative, 8->outlined, 16=>strikeout */
  /* version 4 of OS/2 */
  /* 128->don't use win_ascent/descent for line spacing */
  /* 256=>family varies on weight width slope only */
  /* 512=>oblique (as opposed to italic) */
  uint16_t firstcharindex;      /* minimum unicode encoding */
  uint16_t lastcharindex;       /* maximum unicode encoding */
  uint16_t ascender;            /* font ascender height (not ascent) */
  uint16_t descender;           /* font descender height */
  uint16_t linegap;             /* 0 */
  uint16_t winascent;           /* ymax */
  uint16_t windescent;          /* ymin */
  uint32_t ulCodePage[2];
  /* 1<<0 => latin1, 1<<1=>latin2, cyrillic, greek, turkish, hebrew, arabic */
  /* 1<<30 => mac, 1<<31 => symbol */
  /* OTF stuff (version 2 of OS/2) */
  short xHeight;
  short capHeight;
  short defChar;
  short breakChar;
  short maxContext;
  /* V3 of OS/2 has no additional data */
  /* V4 of OS/2 has no additional data */

  int v1_avgCharWid;            /* 1&2 Weighted average of the lower case letters and space */
  int v3_avgCharWid;            /* 3&4 average over all non-zero width glyphs */
};

struct post
{
  int32_t formattype;           /* 0x00020000 */
  int32_t italicAngle;          /* in fixed format */
  int16_t upos;
  int16_t uwidth;
  uint32_t isfixed;
  uint32_t minmem42;
  uint32_t maxmem42;
  uint32_t minmem1;
  uint32_t maxmem1;
  uint16_t numglyphs;
  uint16_t glyphnameindex[1];
};

struct glyphinfo
{
  struct maxp *maxp;            /* this one is given to dumpglyphs, rest blank */
  uint32_t *loca;
  FILE *glyphs;
  FILE *hmtx;
  int hmtxlen;
  FILE *vmtx;
  int vmtxlen;
  int next_glyph;
  int glyph_len;
  int xmin, ymin, xmax, ymax;
  BlueData bd;
  int strikecnt;                /* number of bitmaps to dump */
  int lasthwidth, lastvwidth;   /* encoding of last glyph for which we generate a full metrics entry */
  int hfullcnt, vfullcnt;
  int flags;
  int fixed_width;
  int32_t *bsizes;
  bool onlybitmaps;
  bool has_instrs;
  bool is_ttf;
  bool ttc_composite_font;
  SplineFont *sf;
  int32_t *pointcounts;
  int *bygid;                   /* glyph list */
  int gcnt;
  int layer;
};

struct vorg
{
  uint16_t majorVersion;        /* 1 */
  uint16_t minorVersion;        /* 0 */
  short defaultVertOriginY;     /* Y coord of default vertical origin in the design coordinate system */
  uint16_t numVertOriginYMetrics;       /* exceptions to the above, elements in following array */
#if 0
  struct
  {
    uint16_t glyphindex;        /* ordered */
    short vertOrigin;
  } origins[];
#endif
};

struct alltabs
{
  struct tabdir tabdir;
  struct head head;
  struct hhead hhead;
  struct hhead vhead;
  struct maxp maxp;
  struct os2 os2;
  struct vorg vorg;
  FILE *loca;
  int localen;
  FILE *name;
  int namelen;
  FILE *post;
  int postlen;
  FILE *gpos;                   /* Used instead of kern for opentype (and other glyph positioning) */
  int gposlen;
  FILE *gsub;                   /* Used for ligatures and other substitutions */
  int gsublen;
  FILE *gdef;                   /* If we use mark to base we need this to tell the text processor what things are marks (the opentype docs say it is optional. They are wrong) */
  int gdeflen;
  FILE *kern;
  int kernlen;
  FILE *cmap;
  int cmaplen;
  FILE *headf;
  int headlen;
  FILE *hheadf;
  int hheadlen;
  FILE *maxpf;
  int maxplen;
  FILE *os2f;
  int os2len;
  FILE *math;
  int mathlen;
  FILE *base;
  int baselen;
  FILE *jstf;
  int jstflen;
  FILE *cvtf;
  int cvtlen;
  FILE *fpgmf;                  /* Copied from an original ttf file and dumped out. Never generated */
  int fpgmlen;
  FILE *prepf;                  /* Copied from an original ttf file and dumped out. Never generated */
  int preplen;
  FILE *vheadf;
  int vheadlen;
  FILE *vorgf;
  int vorglen;
  FILE *gaspf;
  int gasplen;
  FILE *cfff;
  int cfflen;
  FILE *sidf;
  FILE *sidh;
  FILE *charset;
  FILE *encoding;
  FILE *globalsubrs;
  FILE *private;
  FILE *charstrings;
  FILE *fdselect;
  FILE *fdarray;
  FILE *bdat;                   /* might be EBDT */
  int bdatlen;
  FILE *bloc;                   /* might be EBLC */
  int bloclen;
  FILE *ebsc;
  int ebsclen;
  FILE *pfed;
  int pfedlen;
  FILE *tex;
  int texlen;
  FILE *bdf;
  int bdflen;
  FILE *fftmf;
  int fftmlen;
  FILE *dsigf;
  int dsiglen;
  FILE *hdmxf;
  int hdmxlen;
  int defwid, nomwid;
  int sidcnt;
  int lenpos;
  int privatelen;
  bool sidlongoffset;
  bool cfflongoffset;
  bool applemode;               /* Where apple & ms differ do things apple's way (bitmaps, name table PostScript) */
  bool opentypemode;            /* Where apple & ms differ do things opentype's way (bitmaps, name table PostScript) */
  /* If both are set then try to generate both types of tables. Some things can't be fudged though (name table postscript) */
  bool msbitmaps;
  bool applebitmaps;
  bool otbbitmaps;
  bool isotf;
  bool error;
  struct glyphinfo gi;
  int isfixed;
  struct fd2data *fds;
  int next_strid;

  int next_lookup;              /* for doing nested lookups in contextual features */
  short *gn_sid;
  enum fontformat format;
  int fontstyle_name_strid;     /* For GPOS 'size' */
  SplineFont *sf;
  EncMap *map;
  struct ttf_table *oldcvt;
  int oldcvtlen;
};

struct subhead
{
  uint16_t first, cnt, delta, rangeoff;
};                              /* a sub header in 8/16 cmap table */

enum touchflags
{ tf_x = 1, tf_y = 2, tf_d = 4, tf_endcontour = 0x80, tf_startcontour = 0x40 };

struct ct_branch
{
  uint16_t classnum;
  struct contexttree *branch;
};

struct ct_subs
{
  struct fpst_rule *rule;
  struct contexttree *branch;   /* if the rule ends here this will be null */
  uint16_t thisclassnum;
};

struct contexttree
{
  int depth;
  int branch_cnt;               /* count of subbranches of this node */
  struct ct_branch *branches;
  struct fpst_rule *ends_here;
  int rule_cnt;                 /* count of rules which are active here */
  struct ct_subs *rules;
  int pending_pos;
  OTLookup *applymarkedsubs;
  OTLookup *applycursubs;
  uint16_t marked_index, cur_index;
  uint8_t markme;
  int state, next_state;
  struct contexttree *parent;
};

        /* TrueType Composite glyph flags */
#define _ARGS_ARE_WORDS	1
#define _ARGS_ARE_XY	2
#define _ROUND		4       /* round offsets so componant is on grid */
#define _SCALE		8
/* 0x10 is reserved */
#define _MORE		0x20
#define _XY_SCALE	0x40
#define _MATRIX		0x80
#define _INSTR		0x100
#define _USE_MY_METRICS	0x200
#define _OVERLAP_COMPOUND	0x400   /* Used in Apple GX fonts */
            /* Means the components overlap (which? this one and what other?) */
/* Described in OpenType specs, not by Apple */
/* amusingly, Apple supports but MS does not */
/* MS says they support this after Win 2000 */
#define _SCALED_OFFSETS		0x800   /* Use Apple definition of offset interpretation */
#define _UNSCALED_OFFSETS	0x1000  /* Use MS definition */

VISIBLE int ttfFixupRef (SplineChar **chars, int i);
extern const char *cffnames[];
extern const int nStdStrings;

    /* Open type Advanced Typography Tables */
void otf_dumpgpos (struct alltabs *at, SplineFont *sf);
void otf_dumpgsub (struct alltabs *at, SplineFont *sf);
void otf_dumpgdef (struct alltabs *at, SplineFont *sf);
void otf_dumpbase (struct alltabs *at, SplineFont *sf);
void otf_dumpjstf (struct alltabs *at, SplineFont *sf);
void otf_dump_dummydsig (struct alltabs *at, SplineFont *sf);
VISIBLE int gdefclass (SplineChar *sc);

void ttf_dumpkerns (struct alltabs *at, SplineFont *sf);

    /* TrueType instructions */
VISIBLE struct ttf_table *SFFindTable (SplineFont *sf, uint32_t tag);
int32_t memlong (uint8_t *data, int table_len, int offset);
VISIBLE int memushort (uint8_t *data, int table_len, int offset);
VISIBLE void memputshort (uint8_t *data, int offset, uint16_t val);
int TTF__getcvtval (SplineFont *sf, int val);
int TTF_getcvtval (SplineFont *sf, int val);
void SCinitforinstrs (SplineChar *sc);
int SSAddPoints (SplineSet *ss, int ptcnt, BasePoint *bp, char *flags);

    /* Used by both otf and apple */
int LigCaretCnt (SplineChar *sc);
uint16_t *ClassesFromNames (SplineFont *sf, char **classnames, int class_cnt,
                            int numGlyphs, SplineChar ***glyphs, int apple_kc);
SplineChar **SFGlyphsFromNames (SplineFont *sf, char *names);


void AnchorClassOrder (SplineFont *sf);
VISIBLE SplineChar **EntryExitDecompose (SplineFont *sf, AnchorClass *ac,
                                         struct glyphinfo *gi);
VISIBLE void AnchorClassDecompose (SplineFont *sf, AnchorClass *_ac,
                                   int classcnt, int *subcnts,
                                   SplineChar ***marks, SplineChar ***base,
                                   SplineChar ***lig, SplineChar ***mkmk,
                                   struct glyphinfo *gi);

#ifdef HAVE_LONG_LONG_INT
void cvt_unix_to_1904 (long long time, int32_t result[2]);
#else
void cvt_unix_to_1904 (long time, int32_t result[2]);
#endif


    /* Non-standard tables */
        /* My PfEd table for FontForge/PfaEdit specific info */
void pfed_dump (struct alltabs *at, SplineFont *sf);
void pfed_read (FILE *ttf, struct ttfinfo *info);
 /* The TeX table, to contain stuff the TeX people want */
void tex_dump (struct alltabs *at, SplineFont *sf);
void tex_read (FILE *ttf, struct ttfinfo *info);
 /* The BDF table, to contain bdf properties the X people want */
int ttf_bdf_dump (SplineFont *sf, struct alltabs *at, int32_t *sizes);
void ttf_bdf_read (FILE *ttf, struct ttfinfo *info);
 /* The FFTM table, to some timestamps I'd like */
int ttf_fftm_dump (SplineFont *sf, struct alltabs *at);

    /* The MATH table */
void otf_dump_math (struct alltabs *at, SplineFont *sf);
void otf_read_math (FILE *ttf, struct ttfinfo *info);
void otf_read_math_used (FILE *ttf, struct ttfinfo *info);
void GuessNamesFromMATH (FILE *ttf, struct ttfinfo *info);

    /* Parsing advanced typography */
void readttfkerns (FILE *ttf, struct ttfinfo *info);
void readttfgsubUsed (FILE *ttf, struct ttfinfo *info);
void GuessNamesFromGSUB (FILE *ttf, struct ttfinfo *info);
void readttfgpossub (FILE *ttf, struct ttfinfo *info, int gpos);
void readttfgdef (FILE *ttf, struct ttfinfo *info);
void readttfbase (FILE *ttf, struct ttfinfo *info);
void readttfjstf (FILE *ttf, struct ttfinfo *info);

struct otfname *FindAllLangEntries (FILE *ttf, struct ttfinfo *info, int id);

/* Known font parameters for 'TeX ' table (fontdims, spacing params, whatever you want to call them) */
    /* Used by all fonts */
#define	TeX_Slant	CHR('S','l','n','t')
#define TeX_Space	CHR('S','p','a','c')
#define TeX_Stretch	CHR('S','t','r','e')
#define TeX_Shrink	CHR('S','h','n','k')
#define TeX_XHeight	CHR('X','H','g','t')
#define TeX_Quad	CHR('Q','u','a','d')
    /* Used by text fonts */
#define TeX_ExtraSp	CHR('E','x','S','p')
    /* Used by all math fonts */
#define TeX_MathSp	CHR('M','t','S','p')
    /* Used by math fonts */
#define TeX_Num1	CHR('N','u','m','1')
#define TeX_Num2	CHR('N','u','m','2')
#define TeX_Num3	CHR('N','u','m','3')
#define TeX_Denom1	CHR('D','n','m','1')
#define TeX_Denom2	CHR('D','n','m','2')
#define TeX_Sup1	CHR('S','u','p','1')
#define TeX_Sup2	CHR('S','u','p','2')
#define TeX_Sup3	CHR('S','u','p','3')
#define TeX_Sub1	CHR('S','u','b','1')
#define TeX_Sub2	CHR('S','u','b','2')
#define TeX_SupDrop	CHR('S','p','D','p')
#define TeX_SubDrop	CHR('S','b','D','p')
#define TeX_Delim1	CHR('D','l','m','1')
#define TeX_Delim2	CHR('D','l','m','2')
#define TeX_AxisHeight	CHR('A','x','H','t')
    /* Used by math extension fonts */
#define TeX_DefRuleThick	CHR('R','l','T','k')
#define TeX_BigOpSpace1		CHR('B','O','S','1')
#define TeX_BigOpSpace2		CHR('B','O','S','2')
#define TeX_BigOpSpace3		CHR('B','O','S','3')
#define TeX_BigOpSpace4		CHR('B','O','S','4')
#define TeX_BigOpSpace5		CHR('B','O','S','5')

void SFDummyUpCIDs (struct glyphinfo *gi, SplineFont *sf);
