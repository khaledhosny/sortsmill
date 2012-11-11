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
#ifndef _GGADGET_H
#define _GGADGET_H

#include <config.h>

#include "gdraw.h"
#include "intl.h"

struct giocontrol;

typedef struct gtextinfo {
    uint32_t *text;
    GImage *image;
    Color fg;
    Color bg;
    void *userdata;
    GFont *font;
    unsigned int disabled: 1;
    unsigned int image_precedes: 1;
    unsigned int checkable: 1;			/* Only for menus */
    unsigned int checked: 1;			/* Only for menus */
    unsigned int selected: 1;			/* Only for lists (used internally for menu(bar)s, when cursor is on the line) */
    unsigned int line: 1;			/* Only for menus */
    unsigned int text_is_1byte: 1;		/* If passed in as 1byte (ie. iso-8859-1) text, will be converted */
    unsigned int text_in_resource: 1;		/* the text field is actually an index into the string resource table */
    unsigned int changed: 1;			/* If a row/column widget changed this */
    uint32_t mnemonic;				/* Only for menus and menubars */
						/* should really be in menuitem, but that wastes space and complicates GTextInfoDraw */
} GTextInfo;

#define GTEXTINFO_EMPTY { NULL, NULL, 0x000000, 0x000000, NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0, '\0' }


typedef struct gtextinfo2 {
    uint32_t *text;
    GImage *image;
    Color fg;
    Color bg;
    void *userdata;
    GFont *font;
    unsigned int disabled: 1;
    unsigned int image_precedes: 1;
    unsigned int checkable: 1;			/* Only for menus */
    unsigned int checked: 1;			/* Only for menus */
    unsigned int selected: 1;			/* Only for lists (used internally for menu(bar)s, when cursor is on the line) */
    unsigned int line: 1;			/* Only for menus */
    unsigned int text_is_1byte: 1;		/* If passed in as 1byte (ie. iso-8859-1) text, will be converted */
    unsigned int text_in_resource: 1;		/* the text field is actually an index into the string resource table */
    unsigned int changed: 1;			/* If a row/column widget changed this */
    unsigned int sort_me_first_in_list: 1;	/* used for directories in file chooser widgets */
    uint32_t mnemonic;				/* Only for menus and menubars */
						/* should really be in menuitem, but that wastes space and complicates GTextInfoDraw */
} GTextInfo2;

#define GTEXTINFO2_EMPTY { NULL, NULL, 0x000000, 0x000000, NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '\0' }


typedef struct gmenuitem {
    GTextInfo ti;
    uint32_t shortcut;
    short short_mask;
    struct gmenuitem *sub;
    void (*moveto)(struct gwindow *base,struct gmenuitem *mi,GEvent *);	/* called before creating submenu */
    void (*invoke)(struct gwindow *base,struct gmenuitem *mi,GEvent *);	/* called on mouse release */
    int mid;
} GMenuItem;

#define GMENUITEM_EMPTY { GTEXTINFO_EMPTY, '\0', 0, NULL, NULL, NULL, 0 }


typedef struct gmenuitem2 {
    GTextInfo ti;
    char *shortcut;
    struct gmenuitem2 *sub;
    void (*moveto)(struct gwindow *base,struct gmenuitem *mi,GEvent *);	/* called before creating submenu */
    void (*invoke)(struct gwindow *base,struct gmenuitem *mi,GEvent *);	/* called on mouse release */
    int mid;
} GMenuItem2;

#define GMENUITEM2_EMPTY { GTEXTINFO_EMPTY, NULL, NULL, NULL, NULL, 0 }


typedef struct tabinfo {
    uint32_t *text;
    struct ggadgetcreatedata *gcd;
    unsigned int disabled: 1;
    unsigned int selected: 1;
    unsigned int text_is_1byte: 1;		/* If passed in as 1byte (ie. iso-8859-1) text, will be converted */
    unsigned int text_in_resource: 1;		/* the text field is actually an index into the string resource table */
    unsigned char nesting;
} GTabInfo;

#define GTABINFO_EMPTY { NULL, NULL, 0, 0, 0, 0, 0 }


enum border_type { bt_none, bt_box, bt_raised, bt_lowered, bt_engraved,
	    bt_embossed, bt_double };
enum border_shape { bs_rect, bs_roundrect, bs_elipse, bs_diamond };
enum box_flags {
    box_foreground_border_inner = 1,	/* 1 point line */
    box_foreground_border_outer = 2,	/* 1 point line */
    box_active_border_inner = 4,		/* 1 point line */
    box_foreground_shadow_outer = 8,	/* 1 point line, bottom&right */
    box_do_depressed_background = 0x10,
    box_draw_default = 0x20,	/* if a default button draw a depressed rect around button */
    box_generate_colors = 0x40,	/* use border_brightest to compute other border cols */
    box_gradient_bg = 0x80
    };
typedef struct gbox {
    enum border_type border_type;
    enum border_shape border_shape;	
    unsigned char border_width;	/* In points */
    unsigned char padding;	/* In points */
    unsigned char rr_radius;	/* In points */
    unsigned char flags;
    Color border_brightest;		/* used for left upper part of elipse */
    Color border_brighter;
    Color border_darkest;		/* used for right lower part of elipse */
    Color border_darker;
    Color main_background;
    Color main_foreground;
    Color disabled_background;
    Color disabled_foreground;
    Color active_border;
    Color depressed_background;
    Color gradient_bg_end;
    Color border_inner;
    Color border_outer;
} GBox;

#define GBOX_EMPTY { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ,0 ,0 ,0 ,0 }


typedef struct ggadget GGadget;
typedef struct ggadget *GGadgetSet;

enum sb_type { sb_upline, sb_downline, sb_uppage, sb_downpage, sb_track, sb_trackrelease };
struct scrollbarinit { int32_t sb_min, sb_max, sb_pagesize, sb_pos; };

typedef int (*GGadgetHandler)(GGadget *,GEvent *);
typedef uint32_t **(*GTextCompletionHandler)(GGadget *,int from_tab);

enum gg_flags { gg_visible=1, gg_enabled=2, gg_pos_in_pixels=4,
		gg_sb_vert=8, gg_line_vert=gg_sb_vert,
		gg_but_default=0x10, gg_but_cancel=0x20,
		gg_cb_on=0x40, gg_rad_startnew=0x80,
		gg_rad_continueold=0x100,	/* even if not previous */
		gg_list_alphabetic=0x100, gg_list_multiplesel=0x200,
		gg_list_exactlyone=0x400, gg_list_internal=0x800,
		gg_group_prevlabel=0x1000, gg_group_end=0x2000,
		gg_textarea_wrap=0x4000,
		gg_tabset_scroll=0x8000, gg_tabset_filllines=0x10000, gg_tabset_fill1line = 0x20000,
		gg_tabset_nowindow=gg_textarea_wrap,
		gg_rowcol_alphabetic=gg_list_alphabetic,
		gg_rowcol_vrules=0x40000, gg_rowcol_hrules=0x800000,
		gg_rowcol_displayonly=0x1000000,
		gg_dontcopybox=0x10000000,
		gg_pos_use0=0x20000000, gg_pos_under=0x40000000,
		/* Reuse some flag values for different widgets */
		gg_file_pulldown=gg_sb_vert, gg_file_multiple = gg_list_multiplesel,
		gg_text_xim = gg_tabset_scroll,
		gg_tabset_vert = gg_sb_vert,
		gg_utf8_popup = gg_rowcol_displayonly
};

typedef struct ggadgetdata {
    GRect pos;
    GBox *box;
    uint32_t mnemonic;
    uint32_t shortcut;
    uint8_t short_mask;
    uint8_t cols;			/* for rowcol */
    short cid;
    GTextInfo *label;		/* Overloaded with a GGadgetCreateData * for hvboxes (their label is a gadget) */
    union {
	GTextInfo *list;	/* for List Widgets (and ListButtons, RowCols etc) */
	GTabInfo *tabs;		/* for Tab Widgets */
	GMenuItem *menu;	/* for menus */
	GMenuItem2 *menu2;	/* for menus (alternate) */
	struct ggadgetcreatedata **boxelements;	/* An array of things to go in the box */
	struct matrixinit *matrix;
	GDrawEH drawable_e_h;	/* Drawable event handler */
	GTextCompletionHandler completion;
	struct scrollbarinit *sbinit;
	Color col;
	int radiogroup;
    } u;
//    enum gg_flags flags;
    unsigned int flags;
    const uint32_t *popup_msg;		/* Brief help message */
    GGadgetHandler handle_controlevent;
} GGadgetData;

#define GGADGETDATA_EMPTY { GRECT_EMPTY, NULL, '\0', '\0', 0, 0, 0, NULL, { NULL }, 0, NULL, NULL }


typedef struct ggadgetcreatedata {
    GGadget *(*creator)(struct gwindow *base, GGadgetData *gd,void *data);
    GGadgetData gd;
    void *data;
    GGadget *ret;
} GGadgetCreateData;

#define GGADGETCREATEDATA_EMPTY { NULL, GGADGETDATA_EMPTY, NULL, NULL }


#define GCD_Glue	((GGadgetCreateData *) -1)	/* Special entries */
#define GCD_ColSpan	((GGadgetCreateData *) -2)	/* for box elements */
#define GCD_RowSpan	((GGadgetCreateData *) -3)
#define GCD_HPad10	((GGadgetCreateData *) -4)

enum ghvbox_expand { gb_expandglue=-4, gb_expandgluesame=-3, gb_samesize=-2,
	gb_expandall=-1 };
enum editor_commands { ec_cut, ec_clear, ec_copy, ec_paste, ec_undo, ec_redo,
	ec_selectall, ec_search, ec_backsearch, ec_backword, ec_deleteword,
	ec_max };

    /* return values from file chooser filter functions */
enum fchooserret { fc_hide, fc_show, fc_showdisabled };

enum me_type { me_int, me_enum, me_real, me_string, me_bigstr, me_func,
	       me_funcedit,
	       me_stringchoice, me_stringchoicetrans, me_stringchoicetag,
	       me_button,
	       me_hex, me_uhex, me_addr, me_onlyfuncedit };

struct col_init {
    enum me_type me_type;
    char *(*func)(GGadget *,int r,int c);
    GTextInfo *enum_vals;
    void (*enable_enum)(GGadget *,GMenuItem *, int r, int c);
    char *title;
};

struct matrix_data {
    union {
	intptr_t md_ival;
	double md_real;
	char *md_str;
	void *md_addr;
    } u;
    uint8_t frozen;
    uint8_t user_bits;
    uint8_t current;
};

struct matrixinit {
    int col_cnt;
    struct col_init *col_init;
    int initial_row_cnt;
    struct matrix_data *matrix_data;
    void (*initrow)(GGadget *g,int row);
    int  (*candelete)(GGadget *g,int row);
    void (*finishedit)(GGadget *g,int r, int c, int wasnew);
    void (*popupmenu)(GGadget *g,GEvent *e,int row,int col);
    int  (*handle_key)(GGadget *g,GEvent *e);
    char *(*bigedittitle)(GGadget *g,int r, int c);
};

#define COL_INIT_EMPTY { 0, NULL, NULL, NULL, NULL }
#define MATRIX_DATA_EMPTY { { 0 }, 0, 0, 0 }
#define MATRIXINIT_EMPTY { 0, NULL, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL }

#define GME_NoChange	0x80000000

struct gdirentry;
typedef enum fchooserret (*GFileChooserFilterType)(GGadget *g,struct gdirentry *ent,
	const uint32_t *dir);

#define _NUM_Buttonsize		0
#define _NUM_ScaleFactor	1
#define __NUM_LastStd		1

extern GBox _GGadget_button_box, _ggadget_Default_Box;

extern void GTextInfoFree(GTextInfo *ti);
VISIBLE extern void GTextInfoListFree(GTextInfo *ti);
VISIBLE extern void GTextInfoArrayFree(GTextInfo **ti);
VISIBLE extern GTextInfo **GTextInfoFromChars(char **array, int len);
VISIBLE extern const uint32_t *GStringGetResource(int index,uint32_t *mnemonic);
VISIBLE extern int GGadgetScale(int xpos);
VISIBLE extern int GIntGetResource(int index);
extern int GStringSetResourceFileV(char *filename,uint32_t checksum);
extern int GStringSetResourceFile(char *filename);	/* returns 1 for success, 0 for failure */
/* fallback string arrays are null terminated. mnemonics is same length as string */
/* fallback integer arrays are terminated by 0x80000000 (negative infinity) */
extern void GStringSetFallbackArray(const uint32_t **array,const uint32_t *mn,
	const int *ires);
uint32_t *GStringFileGetResource(char *filename, int index,uint32_t *mnemonic);
extern void *GResource_font_cvt(char *val, void *def);
VISIBLE extern FontInstance *GResourceFindFont(char *resourcename,FontInstance *deffont);

void GGadgetDestroy(GGadget *g);
VISIBLE void GGadgetSetVisible(GGadget *g,int visible);
VISIBLE int GGadgetIsVisible(GGadget *g);
VISIBLE void GGadgetSetEnabled(GGadget *g,int enabled);
VISIBLE int GGadgetIsEnabled(GGadget *g);
VISIBLE GWindow GGadgetGetWindow(GGadget *g);
VISIBLE void *GGadgetGetUserData(GGadget *g);
VISIBLE void GGadgetSetUserData(GGadget *g, void *d);
void GGadgetSetPopupMsg(GGadget *g, const uint32_t *msg);
GRect *GGadgetGetInnerSize(GGadget *g,GRect *rct);
VISIBLE GRect *GGadgetGetSize(GGadget *g,GRect *rct);
VISIBLE void GGadgetGetDesiredVisibleSize(GGadget *g,GRect *outer, GRect *inner);
VISIBLE void GGadgetGetDesiredSize(GGadget *g,GRect *outer, GRect *inner);
VISIBLE void GGadgetSetDesiredSize(GGadget *g,GRect *outer, GRect *inner);
VISIBLE int GGadgetGetCid(GGadget *g);
VISIBLE void GGadgetResize(GGadget *g,int32_t width, int32_t height );
VISIBLE void GGadgetMove(GGadget *g,int32_t x, int32_t y );
VISIBLE void GGadgetRedraw(GGadget *g);
VISIBLE void GGadgetsCreate(GWindow base, GGadgetCreateData *gcd);
int  GGadgetFillsWindow(GGadget *g);
int  GGadgetIsDefault(GGadget *g);

VISIBLE void GGadgetSetTitle(GGadget *g,const uint32_t *title);
VISIBLE void GGadgetSetTitle8(GGadget *g,const char *title);
VISIBLE void GGadgetSetTitle8WithMn(GGadget *g,const char *title);
VISIBLE const uint32_t *_GGadgetGetTitle(GGadget *g);	/* Do not free!!! */
VISIBLE uint32_t *GGadgetGetTitle(GGadget *g);		/* Free the return */
VISIBLE char *GGadgetGetTitle8(GGadget *g);		/* Free the return (utf8) */
VISIBLE void GGadgetSetFont(GGadget *g,GFont *font);
GFont *GGadgetGetFont(GGadget *g);
int GGadgetEditCmd(GGadget *g,enum editor_commands cmd);
VISIBLE int GGadgetActiveGadgetEditCmd(GWindow gw,enum editor_commands cmd);
void GGadgetSetHandler(GGadget *g, GGadgetHandler handler);
GGadgetHandler GGadgetGetHandler(GGadget *g);

VISIBLE void GTextFieldSelect(GGadget *g,int sel_start, int sel_end);
VISIBLE void GTextFieldShow(GGadget *g,int pos);
VISIBLE void GTextFieldReplace(GGadget *g,const uint32_t *txt);
VISIBLE void GCompletionFieldSetCompletion(GGadget *g,GTextCompletionHandler completion);
VISIBLE void GCompletionFieldSetCompletionMode(GGadget *g,int enabled);
VISIBLE void GGadgetClearList(GGadget *g);
VISIBLE void GGadgetSetList(GGadget *g, GTextInfo **ti, int32_t copyit);
VISIBLE GTextInfo **GGadgetGetList(GGadget *g,int32_t *len);	/* Do not free!!! */
VISIBLE GTextInfo *GGadgetGetListItem(GGadget *g,int32_t pos);
VISIBLE GTextInfo *GGadgetGetListItemSelected(GGadget *g);
VISIBLE void GGadgetSelectListItem(GGadget *g,int32_t pos,int32_t sel);
VISIBLE void GGadgetSelectOneListItem(GGadget *g,int32_t pos);
VISIBLE int32_t GGadgetIsListItemSelected(GGadget *g,int32_t pos);
VISIBLE int32_t GGadgetGetFirstListSelectedItem(GGadget *g);
VISIBLE void GGadgetScrollListToPos(GGadget *g,int32_t pos);
void GGadgetScrollListToText(GGadget *g,const uint32_t *lab,int32_t sel);
void GGadgetSetListOrderer(GGadget *g,int (*orderer)(const void *, const void *));

void GColorButtonSetColor(GGadget *g, Color col);
Color GColorButtonGetColor(GGadget *g);

VISIBLE void GGadgetSetChecked(GGadget *g, int ison);
VISIBLE int GGadgetIsChecked(GGadget *g);

VISIBLE int GListIndexFromY(GGadget *g,int y);
VISIBLE void GListSetSBAlwaysVisible(GGadget *g,int always);
VISIBLE void GListSetPopupCallback(GGadget *g,void (*callback)(GGadget *,int));

VISIBLE int GTabSetGetSel(GGadget *g);
VISIBLE void GTabSetSetSel(GGadget *g,int sel);
VISIBLE void GTabSetSetEnabled(GGadget *g,int pos, int enabled);
VISIBLE GWindow GTabSetGetSubwindow(GGadget *g,int pos);
int GTabSetGetTabLines(GGadget *g);
void GTabSetSetNestedExpose(GGadget *g, void (*)(GWindow,GGadget *,GEvent *));
void GTabSetSetNestedMouse(GGadget *g, int (*)(GGadget *,GEvent *));
VISIBLE void GTabSetChangeTabName(GGadget *g, char *name, int pos);
VISIBLE void GTabSetRemetric(GGadget *g);
VISIBLE void GTabSetRemoveTabByPos(GGadget *g, int pos);
void GTabSetRemoveTabByName(GGadget *g, char *name);

int32_t GScrollBarGetPos(GGadget *g);
VISIBLE int32_t GScrollBarSetPos(GGadget *g,int32_t pos);
void GScrollBarSetMustShow(GGadget *g, int32_t sb_min, int32_t sb_max, int32_t sb_pagesize,
	int32_t sb_mustshow);
VISIBLE void GScrollBarSetBounds(GGadget *g, int32_t sb_min, int32_t sb_max, int32_t sb_pagesize );
VISIBLE void GScrollBarGetBounds(GGadget *g, int32_t *sb_min, int32_t *sb_max, int32_t *sb_pagesize );

void GMenuBarSetItemChecked(GGadget *g, int mid, int check);
void GMenuBarSetItemEnabled(GGadget *g, int mid, int enabled);
void GMenuBarSetItemName(GGadget *g, int mid, const uint32_t *name);
VISIBLE void GMenuSetShortcutDomain(const char *domain);
const char *GMenuGetShortcutDomain(void);
VISIBLE int GMenuIsCommand(GEvent *event,char *shortcut);
VISIBLE int GMenuMask(void);
int GMenuAnyUnmaskedShortcuts(GGadget *mb1, GGadget *mb2);


VISIBLE void GFileChooserPopupCheck(GGadget *g,GEvent *e);
void GFileChooserFilterIt(GGadget *g);
VISIBLE void GFileChooserRefreshList(GGadget *g);
VISIBLE int GFileChooserFilterEh(GGadget *g,GEvent *e);
VISIBLE void GFileChooserConnectButtons(GGadget *g,GGadget *ok, GGadget *filter);
VISIBLE void GFileChooserSetFilterText(GGadget *g,const uint32_t *filter);
void GFileChooserSetFilterFunc(GGadget *g,GFileChooserFilterType filter);
VISIBLE void GFileChooserSetDir(GGadget *g,uint32_t *dir);
VISIBLE struct giocontrol *GFileChooserReplaceIO(GGadget *g,struct giocontrol *gc);
VISIBLE uint32_t *GFileChooserGetDir(GGadget *g);
uint32_t *GFileChooserGetFilterText(GGadget *g);
GFileChooserFilterType GFileChooserGetFilterFunc(GGadget *g);
void GFileChooserSetMimetypes(GGadget *g, char **mimetypes);
char **GFileChooserGetMimetypes(GGadget *g);
VISIBLE void GFileChooserGetChildren(GGadget *g,GGadget **pulldown, GGadget **list, GGadget **tf);
VISIBLE int GFileChooserPosIsDir(GGadget *g, int pos);
VISIBLE uint32_t *GFileChooserFileNameOfPos(GGadget *g, int pos);
VISIBLE void GFileChooserSetShowHidden(int sh);
VISIBLE int GFileChooserGetShowHidden(void);
VISIBLE void GFileChooserSetDirectoryPlacement(int dp);
VISIBLE int GFileChooserGetDirectoryPlacement(void);
VISIBLE void GFileChooserSetBookmarks(uint32_t **b);
void GFileChooserSetPaths(GGadget *g, char **path);
VISIBLE uint32_t **GFileChooserGetBookmarks(void);
VISIBLE void GFileChooserSetPrefsChangedCallback(void *data, void (*p_c)(void *));

VISIBLE void GHVBoxSetExpandableCol(GGadget *g,int col);
VISIBLE void GHVBoxSetExpandableRow(GGadget *g,int row);
VISIBLE void GHVBoxSetPadding(GGadget *g,int hpad, int vpad);
VISIBLE void GHVBoxFitWindow(GGadget *g);
VISIBLE void GHVBoxFitWindowCentered(GGadget *g);
VISIBLE void GHVBoxReflow(GGadget *g);

VISIBLE void GMatrixEditSet(GGadget *g,struct matrix_data *data, int rows, int copy_it);
VISIBLE struct matrix_data *GMatrixEditGet(GGadget *g, int *rows);
VISIBLE struct matrix_data *_GMatrixEditGet(GGadget *g, int *rows);
VISIBLE GGadget *_GMatrixEditGetActiveTextField(GGadget *g);
VISIBLE int GMatrixEditGetColCnt(GGadget *g);
VISIBLE int GMatrixEditGetActiveRow(GGadget *g);
VISIBLE int GMatrixEditGetActiveCol(GGadget *g);
VISIBLE void GMatrixEditActivateRowCol(GGadget *g, int r, int c);
VISIBLE void GMatrixEditDeleteRow(GGadget *g,int row);
VISIBLE void GMatrixEditScrollToRowCol(GGadget *g,int r, int c);
VISIBLE int GMatrixEditStringDlg(GGadget *g,int row,int col);
VISIBLE void GMatrixEditSetNewText(GGadget *g, const char *text);
VISIBLE void GMatrixEditSetOtherButtonEnable(GGadget *g, void (*sob)(GGadget *g, int r, int c));
VISIBLE void GMatrixEditSetMouseMoveReporter(GGadget *g, void (*rmm)(GGadget *g, int r, int c));
VISIBLE void GMatrixEditSetTextChangeReporter(GGadget *g, void (*tcr)(GGadget *g, int r, int c, GGadget *text));
VISIBLE void GMatrixEditSetValidateStr(GGadget *g, char *(*validate)(GGadget *g, int r, int c, int wasnew, char *str));
VISIBLE void GMatrixEditSetBeforeDelete(GGadget *g, void (*predelete)(GGadget *g, int r));
VISIBLE void GMatrixEditSetRowMotionCallback(GGadget *g, void (*rowmotion)(GGadget *g, int oldr, int newr));
void GMatrixEditUp(GGadget *g);
void GMatrixEditDown(GGadget *g);
enum gme_updown { ud_up_enabled=1, ud_down_enabled=2 };
VISIBLE void GMatrixEditSetCanUpDown(GGadget *g, enum gme_updown (*canupdown)(GGadget *g, int r));
VISIBLE void GMatrixEditSetUpDownVisible(GGadget *g, int visible);
VISIBLE void GMatrixEditAddButtons(GGadget *g, GGadgetCreateData *gcd);
VISIBLE void GMatrixEditEnableColumn(GGadget *g, int col, int enabled);
VISIBLE void GMatrixEditShowColumn(GGadget *g, int col, int visible);
VISIBLE void GMatrixEditSetColumnChoices(GGadget *g, int col, GTextInfo *ti);
VISIBLE GMenuItem *GMatrixEditGetColumnChoices(GGadget *g, int col);
VISIBLE void GMatrixEditSetColumnCompletion(GGadget *g, int col, GTextCompletionHandler completion);
VISIBLE void GMatrixEditSetEditable(GGadget *g, int editable);

VISIBLE GWindow GDrawableGetWindow(GGadget *g);


VISIBLE extern void GGadgetPreparePopupImage(GWindow base,const uint32_t *msg,
	const void *data,
	GImage *(*get_image)(const void *data),
	void (*free_image)(const void *data,GImage *img));
VISIBLE extern void GGadgetPreparePopup(GWindow base,const uint32_t *msg);
extern void GGadgetPreparePopupR(GWindow base,int msg);
VISIBLE extern void GGadgetPreparePopup8(GWindow base,char *msg);
VISIBLE extern void GGadgetEndPopup(void);
VISIBLE extern void GGadgetPopupExternalEvent(GEvent *e);

VISIBLE extern int GGadgetDispatchEvent(GGadget *g,GEvent *e);
VISIBLE extern void GGadgetTakesKeyboard(GGadget *g, int takes_keyboard);

/* Handles *?{}[] wildcards */
VISIBLE int GGadgetWildMatch(uint32_t *pattern, uint32_t *name,int ignorecase);
VISIBLE enum fchooserret GFileChooserDefFilter(GGadget *g,struct gdirentry *ent,
	const uint32_t *dir);

VISIBLE GWindow GMenuCreatePopupMenu(GWindow owner,GEvent *event, GMenuItem *mi);
GWindow _GMenuCreatePopupMenu(GWindow owner,GEvent *event, GMenuItem *mi,
	void (*donecallback)(GWindow owner));

VISIBLE GGadget *GLineCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GGroupCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GSpacerCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GLabelCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GButtonCreate(struct gwindow *base, GGadgetData *gd,void *data);
GGadget *GImageButtonCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GListButtonCreate(struct gwindow *base, GGadgetData *gd,void *data);
GGadget *GColorButtonCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GRadioCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GCheckBoxCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GScrollBarCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GListCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GTextFieldCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GPasswordCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GNumericFieldCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GTextCompletionCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GTextAreaCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GListFieldCreate(struct gwindow *base, GGadgetData *gd,void *data);
GGadget *GSimpleListFieldCreate(struct gwindow *base, GGadgetData *gd,void *data);
GGadget *GMenuBarCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GMenu2BarCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GTabSetCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GFileChooserCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GHBoxCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GVBoxCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GHVBoxCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GHVGroupCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GMatrixEditCreate(struct gwindow *base, GGadgetData *gd,void *data);
VISIBLE GGadget *GDrawableCreate(struct gwindow *base, GGadgetData *gd,void *data);

GGadget *CreateSlider(struct gwindow *base, GGadgetData *gd,void *data);
GGadget *CreateFileChooser(struct gwindow *base, GGadgetData *gd,void *data);
GGadget *CreateGadgets(struct gwindow *base, GGadgetCreateData *gcd);

VISIBLE GTextInfo **GTextInfoArrayFromList(GTextInfo *ti, uint16_t *cnt);
VISIBLE void GTextInfoImageLookup(GTextInfo *ti);
typedef struct gresimage {
    GImage *image;
    char *filename;
} GResImage;
GResImage *GGadgetResourceFindImage(char *name, char *def);

VISIBLE void GGadgetSetImageDir(char *dir);
void GGadgetSetImagePath(char *path);
VISIBLE GImage *GGadgetImageCache(char *filename);
VISIBLE bool TryGGadgetImageCache(GImage *image, char *name);

VISIBLE extern uint32_t *utf82u_mncopy(const char *utf8buf,uint32_t *mn);

VISIBLE extern double GetCalmReal8(GWindow gw,int cid,char *namer,int *err);
VISIBLE extern double GetReal8(GWindow gw,int cid,char *namer,int *err);
extern int GetCalmInt8(GWindow gw,int cid,char *name,int *err);
VISIBLE extern int GetInt8(GWindow gw,int cid,char *namer,int *err);
VISIBLE extern int GetUnicodeChar8(GWindow gw,int cid,char *namer,int *err);
VISIBLE extern void GGadgetProtest8(char *labelr);

extern void GMenuItemParseShortCut(GMenuItem *mi,char *shortcut);
VISIBLE extern int GMenuItemParseMask(char *shortcut);

extern int GGadgetUndoMacEnglishOptionCombinations(GEvent *event);

/* Among other things, this routine sets global icon cache up. */
VISIBLE extern void GGadgetInit(void);
VISIBLE extern int GGadgetWithin(GGadget *g, int x, int y);
VISIBLE extern void GMenuItemArrayFree(GMenuItem *mi);
VISIBLE extern void GMenuItem2ArrayFree(GMenuItem2 *mi);
extern GMenuItem *GMenuItemArrayCopy(GMenuItem *mi, uint16_t *cnt);
VISIBLE extern GMenuItem *GMenuItem2ArrayCopy(GMenuItem2 *mi, uint16_t *cnt);

#endif
