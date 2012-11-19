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
#include "ggadget.h"
#include "gresedit.h"

struct gfuncs {
    bool is_widget;
    uint16_t size;
    int (*handle_expose)(GWindow pixmap,GGadget *g,GEvent *event);
    int (*handle_mouse)(GGadget *g,GEvent *event);
    int (*handle_key)(GGadget *g,GEvent *event);
    int (*handle_editcmd)(GGadget *g,enum editor_commands);
    int (*handle_focus)(GGadget *g,GEvent *event);
    int (*handle_timer)(GGadget *g,GEvent *event);
    int (*handle_sel)(GGadget *g,GEvent *event);

    void (*redraw)(GGadget *g);
    void (*move)(GGadget *g,int32_t x, int32_t y);
    void (*resize)(GGadget *g,int32_t width, int32_t height);
    void (*setvisible)(GGadget *g,int);
    void (*setenabled)(GGadget *g,int);

    GRect *(*getsize)(GGadget *g,GRect *);
    GRect *(*getinnersize)(GGadget *g,GRect *);

    void (*destroy)(GGadget *g);

    void (*set_title)(GGadget *g,const uint32_t *str);
    const uint32_t *(*_get_title)(GGadget *g);
    uint32_t *(*get_title)(GGadget *g);
    void (*set_imagetitle)(GGadget *g,GImage *,const uint32_t *str,int before);
    GImage *(*get_image)(GGadget *g);

    void (*set_font)(GGadget *g,GFont *);
    GFont *(*get_font)(GGadget *g);

    void (*clear_list)(GGadget *g);
    void (*set_list)(GGadget *g, GTextInfo **ti, int32_t copyit);
    GTextInfo **(*get_list)(GGadget *g,int32_t *len);
    GTextInfo *(*get_list_item)(GGadget *g,int32_t pos);
    void (*select_list_item)(GGadget *g,int32_t pos, int32_t sel);
    void (*select_one_list_item)(GGadget *g,int32_t pos);
    int32_t (*is_list_item_selected)(GGadget *g,int32_t pos);
    int32_t (*get_first_selection)(GGadget *g);
    void (*scroll_list_to_pos)(GGadget *g,int32_t pos);
    void (*scroll_list_to_text)(GGadget *g,const uint32_t *lab,int32_t sel);
    void (*set_list_orderer)(GGadget *g,int (*orderer)(const void *, const void *));

    void (*get_desired_size)(GGadget *g, GRect *outer, GRect *inner);
    void (*set_desired_size)(GGadget *g, GRect *outer, GRect *inner);
    int (*fills_window)(GGadget *g);
    int (*is_default)(GGadget *g);
};

enum gadget_state {gs_invisible, gs_disabled, gs_enabled, gs_active,
		   gs_focused, gs_pressedactive };

struct ggadget {
    struct gfuncs *funcs;
    struct gwindow *base;
    GRect r;
    GRect inner;
    uint32_t mnemonic;
    uint32_t shortcut;
    short short_mask;
    struct ggadget *prev;
    bool takes_input;
    bool takes_keyboard;
    bool focusable;
    bool has_focus;
    bool free_box;
    bool was_disabled;
    bool vert;			/* For lines & scrollbars */
    bool opengroup;			/* For groupboxes */
    bool prevlabel;			/* For groupboxes */
    bool contained;			/* is part of a bigger ggadget (ie. a scrollbar is part of a listbox) */
    short cid;
    void *data;
    GBox *box;
    enum gadget_state state;
    uint32_t *popup_msg;
    GGadgetHandler handle_controlevent;
    int16_t desired_width, desired_height;
};

typedef struct ggadget GLine;
typedef struct ggadget GGroup;

typedef struct ggadget GSpacer;		/* a blank space of a given size, used in box layout */

typedef struct glabel {		/* or simple text, or groupbox */
    GGadget g;
    unsigned int fh:8;
    unsigned int as: 8;
    bool image_precedes;
    bool is_default;
    bool is_cancel;
    bool pressed;
    bool within;
    unsigned int labeltype: 2;	/* 0=>label/button(this), 1=>imagebutton, 2=>listbutton, 3=>colorbutton */
    bool shiftonpress;
    FontInstance *font;
    uint32_t *label;
    GImage *image;
    GTextInfo **ti;
    uint16_t ltot;
} GLabel, GButton;

typedef struct gimagebutton {
    GGadget g;
    unsigned int fh:8;
    unsigned int as: 8;
    bool image_precedes;
    bool is_default;
    bool is_cancel;
    bool pressed;
    bool within;
    unsigned int labeltype: 2;	/* 0=>label, 1=>imagebutton(this), 2=>listbutton */
    bool shiftonpress;
    FontInstance *font;
    uint32_t *label;
    GImage *image, *img_within, *active, *disabled;
} GImageButton;

typedef struct glistbutton {
    GGadget g;
    unsigned int fh:8;
    unsigned int as: 8;
    bool image_precedes;
    bool is_default;
    bool is_cancel;
    bool pressed;
    bool within;
    unsigned int labeltype: 2;	/* 0=>label, 1=>imagebutton, 2=>listbutton(this) */
    bool shiftonpress;
    FontInstance *font;
    uint32_t *label;
    GImage *image;
    GTextInfo **ti;
    uint16_t ltot;
    GWindow popup;
} GListButton;

typedef struct gcolorbutton {
    GGadget g;
    unsigned int fh:8;
    unsigned int as: 8;
    bool image_precedes;
    bool is_default;
    bool is_cancel;
    bool pressed;
    bool within;
    unsigned int labeltype: 2;	/* 0=>label/button, 1=>imagebutton, 2=>listbutton, 3=>colorbutton(this) */
    bool shiftonpress;
    FontInstance *font;
    uint32_t *label;
    GImage *image;
    Color col;
} GColorButton;

typedef struct gcheck {
    GGadget g;
    unsigned int fh:8;
    unsigned int as: 8;
    bool image_precedes;
    bool pressed;
    bool within;
    bool isradio;
    bool ison;
    FontInstance *font;
    uint32_t *label;
    GImage *image;
    GRect onoffrect, onoffinner;
    GBox *onbox, *offbox;
    GResImage *on, *off, *ondis, *offdis;
} GCheckBox;

typedef struct gradio {
    GGadget g;
    unsigned int fh:8;
    unsigned int as: 8;
    bool image_precedes;
    bool pressed;
    bool within;
    bool isradio;
    bool ison;
    FontInstance *font;
    uint32_t *label;
    GImage *image;
    GRect onoffrect, onoffinner;
    GBox *onbox, *offbox;
    GResImage *on, *off, *ondis, *offdis;
    struct gradio *post;
    int radiogroup;
} GRadio;

typedef struct gscrollbar {		/* and slider */
    struct ggadget g;
    int32_t sb_min, sb_max, sb_pagesize, sb_pos;
    int32_t sb_mustshow;			/* normally this is sb_pagesize, but might be the height of a single line */
		    /* if we want people to be able to scroll to see white space */
		    /* after the document */
    /*bool vert; */	/* Moved to GGadget, shared with line */
    bool thumbpressed;
    bool ignorenext45;
    int8_t repeatcmd;		/*  sb event to be generated on timer interupts (ie. upline)*/
    int8_t thumbborder;		/* Size of the border of the thumbbox */
    int8_t sbborder;		/* Size of the border of the main scrollbar */
    int16_t thumboff;		/* Offset from where the thumb was pressed to top of thumb */
    int16_t arrowsize;		
    int16_t thumbsize;		/* Current thumb size, refigured after every call to setbounds */
    int16_t thumbpos;		/* Current thumb pos */
    GTimer *pressed;
    GBox *thumbbox;
} GScrollBar;

typedef struct glist {
    GGadget g;
    uint8_t fh;
    uint8_t as;
    uint8_t sofar_max, sofar_pos;
    uint16_t ltot, loff, lcnt;
    uint16_t xoff, xmax;
    uint16_t start, end;			/* current selection drag */
    uint16_t hmax;		/* maximum line height */
    FontInstance *font;
    GTextInfo **ti;
    struct gscrollbar *vsb;
    int (*orderer)(const void *, const void *);
    bool backwards;		/* reverse the order given by orderer */
    bool multiple_sel;	/* Allow multiple selections */
    bool exactly_one;	/* List must always have something selected */
    bool parentpressed;	/* For listbuttons, pressed in parent */
    bool freeti;		/* Free the ti array when we're destroyed */
    bool ispopup;		/* respond to Return and Escape */
    bool sameheight;		/* all lines are the same height */
    bool always_show_sb;	/* display scrollbar even if we don't need it */
    uint32_t *sofar;			/* user input */
    GTimer *enduser;
    GTimer *pressed;
    void (*popup_callback)(GGadget *g,int pos);
} GList;

typedef struct gtextfield {
    GGadget g;
    bool cursor_on;
    bool wordsel;
    bool linesel;
    bool listfield;
    bool drag_and_drop;
    bool has_dd_cursor;
    bool hidden_cursor;
    bool multi_line;
    bool accepts_tabs;
    bool accepts_returns;
    bool wrap;
    bool password;
    bool dontdraw;	/* Used when the tf is part of a larger control, and the control determines when to draw the tf */
    bool numericfield;
    bool incr_down;	/* Direction of increments when numeric_scroll events happen */
    bool completionfield;
    bool was_completing;
    uint8_t fh;
    uint8_t as;
    uint8_t nw;			/* Width of one character (an "n") */
    int16_t xoff_left, loff_top;
    int16_t sel_start, sel_end, sel_base;
    int16_t sel_oldstart, sel_oldend, sel_oldbase;
    int16_t dd_cursor_pos;
    uint32_t *text, *oldtext;
    FontInstance *font;
    GTimer *pressed;
    GTimer *cursor;
    GCursor old_cursor;
    GScrollBar *hsb, *vsb;
    int16_t lcnt, lmax;
    int32_t *lines;		/* offsets in text to the start of the nth line */
    int16_t xmax;
    GIC *gic;
    GTimer *numeric_scroll;
    char *utf8_text;		/* For Pango */
    int32_t *lines8;		/* offsets in utf8_text */
} GTextField;

typedef struct glistfield {
    GTextField gt;
    GRect fieldrect, buttonrect;
    GTextInfo **ti;
    uint16_t ltot;
    GWindow popup;
} GListField;

typedef struct gcompletionfield {
    GListField gl;
    uint32_t **choices;
    uint16_t ctot; int16_t selected;
    GWindow choice_popup;
    GTextCompletionHandler completion;
} GCompletionField;

typedef struct gnumericfield {
    GTextField gt;
    GRect fieldrect, buttonrect;
} GNumericField;

typedef struct gmenubar {
    GGadget g;
    GMenuItem *mi;
    uint16_t *xs;			/* locations at which to draw each name (+1 to give us width of last one) */
    uint16_t mtot;
    int16_t entry_with_mouse;
    int16_t lastmi;		/* If the menubar doesn't fit across the top the make some of it be vertical. Start here */
    struct gmenu *child;
    bool pressed;
    bool initial_press;
    bool any_unmasked_shortcuts;
    FontInstance *font;
    GMenuItem fake[2];		/* Used if not enough room for menu... */
} GMenuBar;

struct tabs { uint32_t *name; int16_t x, width, tw, nesting; bool disabled; GWindow w; };

typedef struct gtabset {
    struct ggadget g;
    struct tabs *tabs;
    int16_t *rowstarts;		/* for each row, index into tab array of its first tab, one extra entry at end with tabcnt */
    int16_t tabcnt;		/* number of tabs */
    int16_t sel;			/* active tab */
    int16_t rcnt;			/* number of rows */
    int16_t active_row;		/* row which is closest to the display area */
    int16_t offset_per_row;	/* stagger tabs by this much */
    int16_t rowh;			/* height of each row */
    int16_t toff;			/* amount things are scrolled off left (x, tabs) */
    int16_t arrow_width;		/* width of arrow tab (for scrolling) */
    int16_t arrow_size;		/* size of the actual arrow itself */
    int16_t ds;
    int16_t pressed_sel;
    bool scrolled;	/* big tabsets either get scrolled or appear in multiple rows */
    bool haslarrow;
    bool hasrarrow;
    bool pressed;
    bool filllines;	/* If we have multiple lines then fill them so that each row takes up the entire width of the tabset */
    bool fill1line;
    bool vertical;
    bool nowindow;
    FontInstance *font;
    void (*nested_expose)(GWindow pixmap, GGadget *g, GEvent *event);
    int (*nested_mouse)(GGadget *g, GEvent *event);
    int16_t vert_list_width;
    int16_t as, fh, offtop;
    GGadget *vsb;
} GTabSet;

struct gdirentry;
typedef struct gfilechooser {
    struct ggadget g;
    GTextField *name;
    GList *files, *subdirs;
    GListButton *directories;
    GButton *ok, *filterb;	/* Not created by us, can be set by user to give chooser a better appearance */
    char **mimetypes;
    uint32_t *wildcard;
    uint32_t *lastname;
    GFileChooserFilterType filter;
    /*enum fchooserret (*filter)(GGadget *chooser,struct gdirentry *file,const uint32_t *dir);*/
    struct giocontrol *outstanding;
    GCursor old_cursor;
    GButton *up, *home;
    GButton *bookmarks, *config;
    struct ghvbox *topbox;
    uint32_t **history;
    uint32_t **paths;
    int hpos, hcnt, hmax;
} GFileChooser;

typedef struct ghvbox {
    GGadget g;
    int rows, cols;
    int hpad, vpad;			/* Internal padding */
    int grow_col, grow_row;		/* -1 => all */
    GGadget **children;			/* array of rows*cols */
    GGadget *label;
    int label_height;
} GHVBox;

struct col_data {
    enum me_type me_type;
    char *(*func)(GGadget *,int r,int c); /* Produces a string to display if md_str==NULL */
    GMenuItem *enum_vals;
    void (*enable_enum)(GGadget *,GMenuItem *, int r, int c);
    GTextCompletionHandler completer;
    char *title;
    int16_t width, x;			/* Relative to inner.x */
    uint8_t fixed;
    uint8_t disabled;
    uint8_t hidden;
};

typedef struct gmatrixedit {
    GGadget g;
    int rows, cols;
    int row_max;
    struct col_data *col_data;
    int hpad, vpad;			/* Internal padding */
    bool has_titles;
    bool lr_pointer;
    bool wasnew;		/* So we need to call newafter when finished editing */
    bool big_done;
    bool edit_active;
    bool no_edit;
    int pressed_col;			/* For changing column spacing */
    struct matrix_data *data;
    int16_t as, fh;
    int16_t font_as, font_fh;
    FontInstance *font;
    FontInstance *titfont;
    GGadget *tf;
    int active_col, active_row;
    int off_top, off_left;
    GGadget *vsb, *hsb;
    GGadget *del;
    GGadget *up, *down;
    GGadget **buttonlist;
    GWindow nested;
    int16_t mark_length, mark_size, mark_skip;
    char *newtext;
    void (*initrow)(GGadget *g,int row);
    int  (*candelete)(GGadget *g,int row);
    enum gme_updown (*canupdown)(GGadget *g,int row);
    void (*popupmenu)(GGadget *g,GEvent *e,int row,int col);
    int  (*handle_key)(GGadget *g,GEvent *e);
    char *(*bigedittitle)(GGadget *g,int r, int c);
    void (*finishedit)(GGadget *g,int r, int c, int wasnew);
    char *(*validatestr)(GGadget *g,int r, int c, int wasnew, char *str);
    void (*setotherbuttons)(GGadget *g, int r, int c);
    void (*reportmousemove)(GGadget *g, int r, int c);
    void (*reporttextchanged)(GGadget *g, int r, int c, GGadget *textfield);
    void (*predelete)(GGadget *g, int r);
    void (*rowmotion)(GGadget *g, int oldr, int newr);
} GMatrixEdit;

typedef struct gdrawable {
    GGadget g;
    GWindow gw;
    GDrawEH e_h;
} GDrawable;

typedef struct rowcol {
    GGadget g;
    int rows, cols;
    GFont *font;
    int as, fh;
    bool hrules;		/* Draw horizontal lines between each row */
    bool vrules;		/* Draw vertical lines between each column */
    bool display_only;
    bool order_entries;	/* normally order rows based on first column entry */
    uint8_t hpad;
    int *colx;				/* col+1 entries, last is xmax */
    GTextInfo **labels;
    GTextInfo **ti;
    GTextField *tf;
    GScrollBar *vsb, *hsb;
    int loff, xoff;
    int tfr, tfc;			/* row,col of textfield (or -1) */
    int (*orderer)(const void *, const void *);
} RowCol;


/* ColorPicker */

extern int _GScrollBar_StartTime,_GScrollBar_RepeatTime;	/* in millisecs */
VISIBLE extern int _GScrollBar_Width;		/* in points */
extern int _GListMarkSize;		/* in points, def width of popup mark in buttons */
extern int _GGadget_Skip;		/* in points, def hor space between gadgets */
extern int _GGadget_TextImageSkip;	/* in points, def hor space text and image */
extern GBox _GListMark_Box, _GGroup_LineBox, _GGadget_defaultbutton_box;
extern GResImage *_GListMark_Image;
VISIBLE extern FontInstance *_ggadget_default_font;

void _GWidget_AddGGadget(GWindow gw,struct ggadget *g);
void _GWidget_RemoveGadget(struct ggadget *g);
void _GWidget_SetMenuBar(GGadget *g);
void _GWidget_SetDefaultButton(GGadget *g);
void _GWidget_MakeDefaultButton(GGadget *g);
void _GWidget_SetCancelButton(GGadget *g);
VISIBLE void _GWidget_SetGrabGadget(GGadget *g);
VISIBLE void _GWidget_ClearGrabGadget(GGadget *g);
void _GWidget_SetPopupOwner(GGadget *g);
void _GWidget_ClearPopupOwner(GGadget *g);

VISIBLE extern void _GGadgetCopyDefaultBox(GBox *box);
VISIBLE extern FontInstance *_GGadgetInitDefaultBox(char *class,GBox *box,FontInstance *deffont);
extern void _ggadget_underlineMnemonic(GWindow gw,int32_t x,int32_t y,uint32_t *label,
	uint32_t mneumonic, Color fg,int ymax);
VISIBLE extern void _ggadgetFigureSize(GWindow gw, GBox *design, GRect *r, int isdef);
extern void _ggadgetSetRects(GGadget *g, GRect *outer, GRect *inner, int xjust, int yjust );
VISIBLE extern void _GGadgetCloseGroup(GGadget *g);
VISIBLE extern void _ggadget_redraw(GGadget *g);
extern int _ggadget_noop(GGadget *g, GEvent *event);
VISIBLE extern void _ggadget_move(GGadget *g, int32_t x, int32_t y );
VISIBLE extern void _ggadget_resize(GGadget *g, int32_t width, int32_t height );
VISIBLE extern void _ggadget_setvisible(GGadget *g,int visible);
VISIBLE extern void _ggadget_setenabled(GGadget *g,int enabled);
VISIBLE extern GRect *_ggadget_getsize(GGadget *g,GRect *rct);
VISIBLE extern GRect *_ggadget_getinnersize(GGadget *g,GRect *rct);
extern void _ggadget_getDesiredSize(GGadget *g, GRect *outer, GRect *inner);
extern void _ggadget_setDesiredSize(GGadget *g,GRect *outer, GRect *inner);
void _GGroup_Init(void);

VISIBLE extern uint32_t *_GGadgetFileToUString(char *filename,int max);

VISIBLE extern int GBoxDrawBorder(GWindow gw,GRect *pos,GBox *design,
	enum gadget_state state,int is_default);
VISIBLE extern void GBoxDrawBackground(GWindow gw,GRect *pos,GBox *design,
	enum gadget_state state,int is_default);
extern void GBoxDrawTabOutline(GWindow pixmap, GGadget *g, int x, int y,
	int width, int rowh, int active );
extern int GBoxDrawHLine(GWindow gw,GRect *pos,GBox *design);
extern int GBoxDrawVLine(GWindow gw,GRect *pos,GBox *design);
VISIBLE extern int GBoxBorderWidth(GWindow gw, GBox *box);
extern int GBoxExtraSpace(GGadget *g);
extern int GBoxDrawnWidth(GWindow gw, GBox *box);

VISIBLE extern int GGadgetInnerWithin(GGadget *g, int x, int y);

extern int GTextInfoGetWidth(GWindow base,GTextInfo *ti,FontInstance *font);
extern int GTextInfoGetMaxWidth(GWindow base,GTextInfo **ti,FontInstance *font);
extern int GTextInfoGetHeight(GWindow base,GTextInfo *ti,FontInstance *font);
extern int GTextInfoGetMaxHeight(GWindow base,GTextInfo **ti,FontInstance *font,int *allsame);
extern int GTextInfoGetAs(GWindow base,GTextInfo *ti, FontInstance *font);
extern int GTextInfoDraw(GWindow base,int x,int y,GTextInfo *ti,
	FontInstance *font,Color fg,Color sel,int ymax);
extern GTextInfo *GTextInfoCopy(GTextInfo *ti);
VISIBLE extern GTextInfo **GTextInfoArrayFromList(GTextInfo *ti, uint16_t *cnt);
extern GTextInfo **GTextInfoArrayCopy(GTextInfo **ti);
extern int GTextInfoArrayCount(GTextInfo **ti);
extern int GTextInfoCompare(GTextInfo *ti1, GTextInfo *ti2);
extern int GMenuItemArrayMask(GMenuItem *mi);
extern int GMenuItemArrayAnyUnmasked(GMenuItem *mi);

VISIBLE extern GGadget *_GGadget_Create(GGadget *g, struct gwindow *base, GGadgetData *gd,void *data, GBox *def);
VISIBLE extern void _GGadget_FinalPosition(GGadget *g, struct gwindow *base, GGadgetData *gd);
VISIBLE extern void _ggadget_destroy(GGadget *g);

extern GWindow GListPopupCreate(GGadget *owner,void (*inform)(GGadget *,int), GTextInfo **ti);

enum mark_type { mt_arrow, mt_plus, mt_minus };

extern int GMenuPopupCheckKey(GEvent *event);
extern int GMenuBarCheckKey(GGadget *g, GEvent *event);
extern void _GButton_SetDefault(GGadget *g,int32_t is_default);
VISIBLE extern void _GButtonInit(void);
extern void GListMarkDraw(GWindow pixmap,int x, int y, int height, enum gadget_state state, enum mark_type);
extern char **_GGadget_GetImagePath(void);
extern int _GGadget_ImageInCache(GImage *image);

extern GResInfo ggadget_ri, listmark_ri;
extern GResInfo *_GGadgetRIHead(void), *_GButtonRIHead(void), *_GTextFieldRIHead(void);
extern GResInfo *_GRadioRIHead(void), *_GScrollBarRIHead(void), *_GLineRIHead(void);
extern GResInfo *_GMenuRIHead(void), *_GTabSetRIHead(void), *_GHVBoxRIHead(void);
extern GResInfo *_GListRIHead(void), *_GMatrixEditRIHead(void), *_GDrawableRIHead(void);
extern GResInfo *_GProgressRIHead(void);

#define MENU_ICON_SIZE		18
#define MENU_ICON_SEP		6
