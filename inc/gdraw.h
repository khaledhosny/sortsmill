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
#ifndef _GDRAW_H
#define _GDRAW_H

#include <config.h>

#include "gimage.h"
#include <cairo/cairo.h>

enum font_style { fs_none, fs_italic=1, fs_smallcaps=2, fs_condensed=4, fs_extended=8, fs_rotated=16 };
enum font_type { ft_unknown, ft_serif, ft_sans, ft_mono, ft_cursive, ft_max };
enum text_mods { tm_none, tm_upper=1, tm_lower=2, tm_initialcaps=4, tm_showsofthyphen=8 };
enum text_lines { tl_none, tl_under=1, tl_strike=2, tl_over=4, tl_dash=8 };

typedef struct {
    const unichar_t *family_name;	/* may be more than one */
    int16 point_size;			/* negative values are in pixels */
    int16 weight;
    enum font_style style;
    char *utf8_family_name;
} FontRequest;

typedef struct font_instance FontInstance, GFont;
enum gic_style { gic_overspot=2, gic_root=1, gic_hidden=0, gic_orlesser=4, gic_type=3 };
typedef struct ginput_context GIC;

enum draw_func { df_copy, df_xor };

typedef struct ggc {
    struct gwindow *w;
    int32 xor_base;
    Color fg;
    Color bg;
    GRect clip;
    enum draw_func func;
    unsigned int copy_through_sub_windows: 1;
    unsigned int bitmap_col: 1;			/* window is mapped for bitmap */
    int16 skip_len, dash_len;
    int16 line_width;
    int16 ts;
    int32 ts_xoff, ts_yoff;
    int dash_offset;
    GFont *fi;
} GGC;

typedef struct gtextbounds {
    int16 lbearing;		/* of first character */
				/* origin to left edge of first char's raster */
    int32 rbearing;		/* origin to right edge of last char's raster */
    int16 as,ds;		/* maximum ascent and maximum descent */
    				/*  (both numbers will be positive for "g" */
			        /* so total height = as+ds */
    int16 fas, fds;		/* font ascent and descent */
			        /* total width = rbearing-lbearing */
    int32 width;	        /* above are for the bounding rect, not the text */
			        /*  "width" which may be totally different */
} GTextBounds;

enum selnames { sn_primary, sn_clipboard, sn_drag_and_drop, sn_user1, sn_user2, sn_max };
typedef struct gwindow *GWindow;
typedef struct gdisplay GDisplay;
typedef struct gtimer GTimer;

enum keystate_mask { ksm_shift=1, ksm_capslock=2, ksm_control=4, ksm_meta=8,
	ksm_cmdsuse=0x8,
/* Suse X on a Mac maps command to meta. As of Mac 10.2, the command key is 0x10 */
/*  In 10.0 the command key was 0x20 */
	ksm_cmdmacosx=0x10,	/* But not the command key under suse ppc linux*/
	ksm_numlock=0x10,	/* It's numlock on my 386 system */
	ksm_super=0x40,		/* RedHat mask for the key with the windows flag on it */
	ksm_hyper=0x80,
/* Both Suse and Mac OS/X.2 now map option to 0x2000, but under 10.0 it was meta */
/* Under 10.4 it is the meta mask again */
/* Under 10.6 it is 0x2000 again. I wish they'd be consistent */
	ksm_option=0x2000,	/* sometimes */
	ksm_menumask=(ksm_control|ksm_meta|ksm_cmdmacosx|0xf0),

	ksm_button1=(1<<8), ksm_button2=(1<<9), ksm_button3=(1<<10),
	ksm_button4=(1<<11), ksm_button5=(1<<12),
	ksm_buttons=(ksm_button1|ksm_button2|ksm_button3|ksm_button4|ksm_button5)
	};
enum mnemonic_focus { mf_normal, mf_tab, mf_mnemonic, mf_shortcut };

enum event_type { et_noevent = -1, et_char, et_charup,
		  et_mousemove, et_mousedown, et_mouseup,
		  et_crossing,	/* these four are assumed to be consecutive */
		  et_focus,
		  et_expose, et_visibility, et_resize, et_timer,
		  et_close/*request by user*/, et_create,
		  et_map, et_destroy/*window being freed*/,
		  et_selclear,
		  et_drag, et_dragout, et_drop,
		  et_lastnativeevent=et_drop,
		  et_controlevent, et_user };

enum visibility_state { vs_unobscured, vs_partially, vs_obscured };

enum et_subtype { et_buttonpress, et_buttonactivate, et_radiochanged,
		  et_listselected, et_listdoubleclick,
		  et_scrollbarchange,
		  et_textchanged, et_textfocuschanged,
		  et_lastsubtype };

enum sb { et_sb_top, et_sb_uppage, et_sb_up, et_sb_left=et_sb_up,
	  et_sb_down, et_sb_right=et_sb_down, et_sb_downpage,
	  et_sb_bottom,
	  et_sb_thumb, et_sb_thumbrelease };

struct sbevent {
    enum sb type;
    int32 pos;
};

typedef struct gevent {
    enum event_type type;
#define _GD_EVT_CHRLEN	10
    GWindow w;
    union {
	struct {
	    char *device;		/* for wacom devices */
	    uint32 time;
	    uint16 state;
	    int16 x,y;
	    uint16 keysym;
	    int16 autorepeat;
	    unichar_t chars[_GD_EVT_CHRLEN];
	} chr;
	struct {
	    char *device;		/* for wacom devices */
	    uint32 time;
	    int16 state;
	    int16 x,y;
	    int16 button;
	    int16 clicks;
	    int32 pressure, xtilt, ytilt, separation;
	} mouse;
	struct {
	    GRect rect;
	} expose;
	struct {
	    enum visibility_state state;
	} visibility;
	struct {
	    GRect size;
	    int16 dx, dy, dwidth, dheight;
	    unsigned int moved: 1;
	    unsigned int sized: 1;
	} resize;
	struct {
	    char *device;		/* for wacom devices */
	    uint32 time;
	    int16 state;
	    int16 x,y;
	    unsigned int entered: 1;
	} crossing;
	struct {
	    unsigned int gained_focus: 1;
	    unsigned int mnemonic_focus: 2;
	} focus;
	struct {
	    unsigned int is_visible: 1;
	} map;
	struct {
	    enum selnames sel;
	} selclear;
	struct {
	    int32 x,y;
	} drag_drop;
	struct {
	    GTimer *timer;
	    void *userdata;
	} timer;
	struct {
	    enum et_subtype subtype;
	    struct ggadget *g;
	    union {
		struct sbevent sb;
		struct {
		    int gained_focus;
		} tf_focus;
		struct {
		    int from_pulldown;	/* -1 normally, else index into pulldown list */
		} tf_changed;
		struct {
		    int clicks;
		    int16 button, state;
		} button;
		struct {
		    int from_mouse, changed_index;
		} list;
	    } u;
	} control;
	struct {
	    long subtype;
	    void *userdata;
	} user;
    } u;
    void *native_window;
} GEvent;

typedef enum cursor_types { ct_default, ct_pointer, ct_backpointer, ct_hand,
	ct_question, ct_cross, ct_4way, ct_text, ct_watch, ct_draganddrop,
	ct_invisible, 
	ct_user, ct_user2 /* and so on */ } GCursor;

enum window_attr_mask { wam_events=0x2, wam_bordwidth=0x4,
			wam_bordcol=0x8, wam_backcol=0x10, wam_cursor=0x20, wam_wtitle=0x40,
			wam_ititle=0x80, wam_icon=0x100, wam_nodecor=0x200,
			wam_positioned=0x400, wam_centered=0x800, wam_undercursor=0x1000,
			wam_noresize=0x2000, wam_restrict=0x4000, wam_redirect=0x8000,
			wam_isdlg=0x10000, wam_notrestricted=0x20000,
			wam_transient=0x40000,
			wam_utf8_wtitle=0x80000, wam_utf8_ititle=0x100000,
			wam_verytransient=0x400000 };

typedef struct gwindow_attrs {
//    enum window_attr_mask mask;
    unsigned int mask;
    uint32 event_masks;			/* (1<<et_char) | (1<<et_mouseup) etc */
    int16 border_width;
    Color border_color;			/* Color_UNKNOWN if unspecified */
    Color background_color;
    GCursor cursor;
    /* Remainder is only for top level windows */
    const unichar_t *window_title;
    const unichar_t *icon_title;
    struct gwindow *icon;		/* A bitmap pixmap, or NULL */
    unsigned int nodecoration: 1;	/* no wm decoration */
    unsigned int positioned: 1;		/* position information is important */
    unsigned int centered: 2;		/* center the window on the screen. pos.width&pos.height are used */
    unsigned int undercursor: 1;	/* center the window under the cursor. */
    unsigned int noresize: 1;		/* set min and max sizes to current size */
    unsigned int restrict_input_to_me: 1;/* for dialogs, no input outside of dlg */
    unsigned int redirect_chars_to_me: 1;/* ditto, we get any input outside of us */
    unsigned int is_dlg: 1;		/* 1 for dlg, 0 for main window */
    unsigned int not_restricted: 1;	/* gets events if if a restricted (modal) dlg is up */
    GWindow redirect_from;		/* only redirect input from this window and its children */
    GWindow transient;			/* the Transient_FOR hint */
    const char *utf8_window_title;
    const char *utf8_icon_title;
} GWindowAttrs;

#define GWINDOWATTRS_EMPTY { 0, 0, 0, 0, 0, 0, NULL, NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0, NULL, NULL, NULL, NULL }


typedef struct gdeveventmask {
    int event_mask;
    char *device_name;
} GDevEventMask;

enum gzoom_flags { gzf_pos=1, gzf_size=2 };

typedef int (*GDrawEH)(GWindow,GEvent *);

extern unichar_t *GDrawKeysyms[];
VISIBLE extern GDisplay *screen_display, *printer_display;

VISIBLE extern void GDrawCreateDisplays(char *displayname,char *programname);
VISIBLE extern void *GDrawNativeDisplay(GDisplay *);
VISIBLE extern void GDrawTerm(GDisplay *disp);

VISIBLE extern int GDrawGetRes(GWindow gw);
VISIBLE extern int GDrawPointsToPixels(GWindow gw,int points);
VISIBLE extern int GDrawPixelsToPoints(GWindow gw,int pixels);

VISIBLE extern void GDrawSetDefaultIcon(GWindow icon);
VISIBLE extern GWindow GDrawCreateTopWindow(GDisplay *gdisp, GRect *pos, int (*eh)(GWindow,GEvent *), void *user_data, GWindowAttrs *wattrs);
VISIBLE extern GWindow GDrawCreateSubWindow(GWindow w, GRect *pos, int (*eh)(GWindow,GEvent *), void *user_data, GWindowAttrs *wattrs);
VISIBLE extern GWindow GDrawCreatePixmap(GDisplay *gdisp, uint16 width, uint16 height);
VISIBLE extern GWindow GDrawCreateBitmap(GDisplay *gdisp, uint16 width, uint16 height, uint8 *data);
VISIBLE extern GCursor GDrawCreateCursor(GWindow src,GWindow mask,Color fg,Color bg,
	int16 x, int16 y );
VISIBLE extern void GDrawDestroyWindow(GWindow w);
VISIBLE extern void GDrawDestroyCursor(GDisplay *gdisp, GCursor ct);
VISIBLE extern int  GDrawNativeWindowExists(GDisplay *gdisp, void *native);
VISIBLE extern void GDrawSetZoom(GWindow w, GRect *zoomsize, enum gzoom_flags);
VISIBLE extern void GDrawSetWindowBorder(GWindow w, int width, Color color);
VISIBLE extern void GDrawSetWindowBackground(GWindow w, Color color);
VISIBLE extern int  GDrawSetDither(GDisplay *gdisp, int dither);
VISIBLE extern void GDrawReparentWindow(GWindow child,GWindow newparent, int x,int y);
VISIBLE extern void GDrawSetVisible(GWindow w, int visible);
VISIBLE extern int  GDrawIsVisible(GWindow w);
VISIBLE extern void GDrawTrueMove(GWindow w, int32 x, int32 y);
VISIBLE extern void GDrawMove(GWindow w, int32 x, int32 y);
VISIBLE extern void GDrawResize(GWindow w, int32 width, int32 height);
VISIBLE extern void GDrawMoveResize(GWindow w, int32 x, int32 y, int32 width, int32 height);
VISIBLE extern GWindow GDrawGetRoot(GDisplay *);
VISIBLE extern Color GDrawGetDefaultBackground(GDisplay *);
VISIBLE extern Color GDrawGetDefaultForeground(GDisplay *);
VISIBLE extern GRect *GDrawGetSize(GWindow w, GRect *ret);
VISIBLE extern GDrawEH GDrawGetEH(GWindow w);
VISIBLE extern void GDrawSetEH(GWindow w,GDrawEH e_h);
VISIBLE extern void GDrawGetPointerPosition(GWindow w, GEvent *mouse);
VISIBLE extern GWindow GDrawGetPointerWindow(GWindow w);
VISIBLE extern void GDrawRaise(GWindow w);
VISIBLE extern void GDrawRaiseAbove(GWindow w,GWindow below);
VISIBLE extern int  GDrawIsAbove(GWindow w,GWindow other);
VISIBLE extern void GDrawLower(GWindow w);
VISIBLE extern void GDrawSetWindowTitles(GWindow w, const unichar_t *title, const unichar_t *icontit);
VISIBLE extern void GDrawSetWindowTitles8(GWindow w, const char *title, const char *icontit);
VISIBLE extern unichar_t *GDrawGetWindowTitle(GWindow w);
VISIBLE extern char *GDrawGetWindowTitle8(GWindow w);
VISIBLE extern void GDrawSetTransientFor(GWindow transient,GWindow owner);
VISIBLE extern void GDrawSetCursor(GWindow w, GCursor ct);
VISIBLE extern GCursor GDrawGetCursor(GWindow w);
VISIBLE extern GWindow GDrawGetRedirectWindow(GDisplay *gd);
VISIBLE extern GWindow GDrawGetParentWindow(GWindow gw);
VISIBLE extern int GDrawWindowIsAncestor(GWindow ancester, GWindow descendent);
VISIBLE extern void GDrawSetUserData(GWindow gw, void *ud);
VISIBLE extern void *GDrawGetUserData(GWindow gw);
VISIBLE extern GDisplay *GDrawGetDisplayOfWindow(GWindow);
VISIBLE extern void GDrawTranslateCoordinates(GWindow from,GWindow to, GPoint *pt);
VISIBLE extern int32 GDrawEventInWindow(GWindow inme,GEvent *event);
VISIBLE extern void GDrawBeep(GDisplay *gdisp);
VISIBLE extern void GDrawFlush(GDisplay *gdisp);

VISIBLE extern void GDrawGetClip(GWindow w, GRect *ret);
VISIBLE extern void GDrawSetClip(GWindow w, GRect *rct);
VISIBLE extern void GDrawPushClip(GWindow w, GRect *rct, GRect *old);
VISIBLE extern void GDrawPopClip(GWindow w, GRect *old);
VISIBLE extern GGC *GDrawGetWindowGGC(GWindow w);
VISIBLE extern void GDrawSetXORBase(GWindow w,Color col);
VISIBLE extern void GDrawSetXORMode(GWindow w);
VISIBLE extern void GDrawSetCopyMode(GWindow w);
VISIBLE extern void GDrawSetCopyThroughSubWindows(GWindow w,int16 through);
VISIBLE extern void GDrawSetDashedLine(GWindow w,int16 dash_len, int16 skip_len, int16 off);
VISIBLE extern void GDrawSetStippled(GWindow w,int16 ts, int32 yoff,int32 xoff);
VISIBLE extern void GDrawSetLineWidth(GWindow w,int16 width);
VISIBLE extern void GDrawSetForeground(GWindow w,Color col);
VISIBLE extern void GDrawSetBackground(GWindow w,Color col);

VISIBLE extern GFont *GDrawSetFont(GWindow gw, GFont *fi);
VISIBLE extern GFont *GDrawInstanciateFont(GWindow gw, FontRequest *rq);
VISIBLE extern GFont *GDrawNewFont(GWindow gw, char *family_name, int point_size, int weight, enum font_style style);
VISIBLE extern FontRequest *GDrawDecomposeFont(GFont *fi, FontRequest *rq);
VISIBLE extern void GDrawGetFontMetrics(GWindow gw,GFont *fi,int *as, int *ds, int *ld);

VISIBLE extern int32 GDrawGetTextBounds(GWindow gw,const unichar_t *text, int32 cnt, GTextBounds *size);
VISIBLE extern int32 GDrawGetTextWidth(GWindow gw, const unichar_t *text, int32 cnt);
VISIBLE extern int32 GDrawDrawText(GWindow gw, int32 x, int32 y, const unichar_t *txt, int32 cnt, Color col);

/* UTF8 routines */
VISIBLE extern int32 GDrawGetText8Bounds(GWindow gw, const char *text, int32 cnt, GTextBounds *size);
VISIBLE extern int32 GDrawGetText8Width(GWindow gw, const char *text, int32 cnt);
VISIBLE extern int32 GDrawDrawText8(GWindow gw, int32 x, int32 y, const char *txt, int32 cnt, Color col);

VISIBLE extern GIC *GDrawCreateInputContext(GWindow w,enum gic_style def_style);
VISIBLE extern void GDrawSetGIC(GWindow w,GIC *gic,int x, int y);

VISIBLE extern void GDrawClear(GWindow w, GRect *rect);
VISIBLE extern void GDrawDrawLine(GWindow w, int32 x,int32 y, int32 xend,int32 yend, Color col);
VISIBLE extern void GDrawDrawRect(GWindow w, GRect *rect, Color col);
VISIBLE extern void GDrawFillRect(GWindow w, GRect *rect, Color col);
VISIBLE extern void GDrawFillRoundRect(GWindow w, GRect *rect, int radius, Color col);
VISIBLE extern void GDrawDrawElipse(GWindow w, GRect *rect, Color col);
VISIBLE extern void GDrawFillElipse(GWindow w, GRect *rect, Color col);
VISIBLE extern void GDrawDrawArc(GWindow w, GRect *rect, int32 sangle, int32 tangle, Color col);
VISIBLE extern void GDrawDrawPoly(GWindow w, GPoint *pts, int16 cnt, Color col);
VISIBLE extern void GDrawFillPoly(GWindow w, GPoint *pts, int16 cnt, Color col);
VISIBLE extern void GDrawScroll(GWindow w, GRect *rect, int32 hor, int32 vert);
VISIBLE extern void GDrawDrawImage(GWindow w, GImage *img, GRect *src, int32 x, int32 y);
VISIBLE extern void GDrawDrawGlyph(GWindow w, GImage *img, GRect *src, int32 x, int32 y);
VISIBLE extern void GDrawDrawScaledImage(GWindow w, GImage *img, int32 x, int32 y);
VISIBLE extern void GDrawDrawImageMagnified(GWindow w, GImage *img, GRect *src, int32 x, int32 y,
	int32 width, int32 height);
VISIBLE extern void GDrawDrawPixmap(GWindow w, GWindow pixmap, GRect *src, int32 x, int32 y);

VISIBLE extern void GDrawGrabSelection(GWindow w,enum selnames sel);
VISIBLE extern void GDrawAddSelectionType(GWindow w,enum selnames sel,char *type,
	void *data,int32 cnt,int32 unitsize,void *(*gendata)(void *,int32 *len),
	void (*freedata)(void *));
VISIBLE extern void *GDrawRequestSelection(GWindow w,enum selnames sn, char *typename_, int32 *len);
VISIBLE extern int GDrawSelectionHasType(GWindow w,enum selnames sn, char *typename_);
VISIBLE extern void GDrawBindSelection(GDisplay *disp,enum selnames sel, char *atomname);
VISIBLE extern int GDrawSelectionOwned(GDisplay *disp,enum selnames sel);
VISIBLE extern void GDrawPropertyToSelectionOwner(GDisplay *disp,enum selnames sel,
	char *property, char *type, int format, int mode,
	uint8 *data, int nelements);

VISIBLE extern void GDrawPointerUngrab(GDisplay *disp);
VISIBLE extern void GDrawPointerGrab(GWindow w);
VISIBLE extern int GDrawEnableExposeRequests(GWindow w,int enabled);
VISIBLE extern void GDrawRequestExpose(GWindow w, GRect *rect, int doclear);
VISIBLE extern void GDrawSync(GDisplay *gdisp);
VISIBLE extern void GDrawForceUpdate(GWindow w);
VISIBLE extern void GDrawProcessOneEvent(GDisplay *disp);
VISIBLE extern void GDrawProcessPendingEvents(GDisplay *disp);
VISIBLE extern void GDrawProcessWindowEvents(GWindow w);
VISIBLE extern void GDrawSkipMouseMoveEvents(GWindow w,GEvent *last);
VISIBLE extern void GDrawEventLoop(GDisplay *disp);
VISIBLE extern void GDrawPostEvent(GEvent *e);
VISIBLE extern void GDrawPostDragEvent(GWindow gw,GEvent *e,enum event_type);

VISIBLE extern GTimer *GDrawRequestTimer(GWindow w,int32 time_from_now,int32 frequency,
	void *userdata);
VISIBLE extern void GDrawCancelTimer(GTimer *timer);

VISIBLE extern void GDrawSyncThread(GDisplay *gd, void (*func)(void *), void *data);

VISIBLE extern void GDrawSetBuildCharHooks(void (*hook)(GDisplay *), void (*inshook)(GDisplay *,unichar_t));

VISIBLE extern int GDrawRequestDeviceEvents(GWindow w,int devcnt,struct gdeveventmask *de);

VISIBLE extern void GDrawPathStroke(GWindow w,Color col);
VISIBLE extern void GDrawPathFill(GWindow w,Color col);

VISIBLE extern void GDrawLayoutInit(GWindow w, char *text, int cnt, GFont *fi);
VISIBLE extern void GDrawLayoutDraw(GWindow w, int32 x, int32 y, Color fg);
VISIBLE extern void GDrawLayoutIndexToPos(GWindow w, int index, GRect *pos);
VISIBLE extern int  GDrawLayoutXYToIndex(GWindow w, int x, int y);
VISIBLE extern void GDrawLayoutExtents(GWindow w, GRect *size);
extern void GDrawLayoutSetWidth(GWindow w, int width);
extern int  GDrawLayoutLineCount(GWindow w);
extern int  GDrawLayoutLineStart(GWindow w,int line);
VISIBLE extern cairo_t *GDrawGetCairo(GWindow w);

extern void GDrawFatalError(const char *fmt,...);
VISIBLE extern void GDrawIError(const char *fmt,...);
VISIBLE extern int GDrawKeyState(int keysym);

extern int GImageGetScaledWidth(GWindow gw, GImage *img);
extern int GImageGetScaledHeight(GWindow gw, GImage *img);
#endif
