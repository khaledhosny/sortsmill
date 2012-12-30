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

#ifndef _FF_INTERNAL_GDRAW_H
#define _FF_INTERNAL_GDRAW_H

#include <config.h>

#include "gimage.h"
#include <cairo/cairo.h>

struct ggadget;

typedef enum font_style
{
  fs_none = 0,
  fs_italic = 1,
  fs_smallcaps = 2,
  fs_condensed = 4,
  fs_extended = 8,
  fs_rotated = 16
} FontStyle;

typedef enum font_type
{
  ft_unknown = 0,
  ft_serif,
  ft_sans,
  ft_mono,
  ft_cursive,
  ft_max
} FontType;

typedef enum text_mods
{
  tm_none = 0,
  tm_upper = 1,
  tm_lower = 2,
  tm_initialcaps = 4,
  tm_showsofthyphen = 8
} TextMods;

typedef enum text_lines
{
  tl_none = 0,
  tl_under = 1,
  tl_strike = 2,
  tl_over = 4,
  tl_dash = 8
} TextLines;

typedef struct
{
  const uint32_t *family_name;  /* may be more than one */
  int16_t point_size;           /* negative values are in pixels */
  int16_t weight;
  enum font_style style;
  char *utf8_family_name;
} FontRequest;

typedef struct font_instance FontInstance;
typedef struct font_instance GFont;

typedef enum gic_style
{
  gic_overspot = 2,
  gic_root = 1,
  gic_hidden = 0,
  gic_orlesser = 4,
  gic_type = 3
} GIC_Style;

typedef struct ginput_context GIC;

typedef enum draw_func
{
  df_copy = 0,
  df_xor
} DrawFunc;

typedef struct ggc
{
  struct gwindow *w;
  int32_t xor_base;
  Color fg;
  Color bg;
  GRect clip;
  enum draw_func func;
  bool copy_through_sub_windows;
  bool bitmap_col;              /* window is mapped for bitmap */
  int16_t skip_len;
  int16_t dash_len;
  int16_t line_width;
  int16_t ts;
  int32_t ts_xoff;
  int32_t ts_yoff;
  int dash_offset;
  GFont *fi;
} GGC;

typedef struct gtextbounds
{
  int16_t lbearing;             /* Origin to left edge of first char's
                                   raster. FIXME: WHY IS THIS STORED IN A 16-BIT
                                   INTEGER? */
  int32_t rbearing;             /* Origin to right edge of last char's
                                   raster. */

  /* Both of the following numbers will be positive for "g", so total
     height = as + ds. */
  int16_t as;                   /* maximum ascent */
  int16_t ds;                   /* maximum descent */

  int16_t fas;                  /* font ascent */
  int16_t fds;                  /* font descent */

  /* total width = rbearing - lbearing */
  int32_t width;

  /* Above are for the bounding rect, not the text "width" which may
     be totally different. FIXME: WHAT DOES THIS COMMENT MEAN? */
} GTextBounds;

typedef enum selnames
{
  sn_primary,
  sn_clipboard,
  sn_drag_and_drop,
  sn_user1,
  sn_user2,
  sn_max
} SelNames;

typedef struct gwindow *GWindow;
typedef struct gdisplay GDisplay;
typedef struct gtimer GTimer;

typedef enum keystate_mask
{
  ksm_shift = 1,
  ksm_capslock = 2,
  ksm_control = 4,
  ksm_meta = 8,
  ksm_cmdsuse = 0x8,
  /* Suse X on a Mac maps command to meta. As of Mac 10.2, the command
     key is 0x10. In 10.0 the command key was 0x20. */
  ksm_cmdmacosx = 0x10,         /* But not the command key under suse ppc linux */
  ksm_numlock = 0x10,           /* It's numlock on my 386 system */
  ksm_super = 0x40,             /* RedHat mask for the key with the windows flag on it */
  ksm_hyper = 0x80,
  /* Both Suse and Mac OS/X.2 now map option to 0x2000, but under 10.0
     it was meta.  Under 10.4 it is the meta mask again. Under 10.6 it
     is 0x2000 again. I wish they'd be consistent. */
  ksm_option = 0x2000,          /* sometimes */
  ksm_menumask = (ksm_control | ksm_meta | ksm_cmdmacosx | 0xf0),

  ksm_button1 = (1 << 8), ksm_button2 = (1 << 9), ksm_button3 = (1 << 10),
  ksm_button4 = (1 << 11), ksm_button5 = (1 << 12),
  ksm_buttons =
    (ksm_button1 | ksm_button2 | ksm_button3 | ksm_button4 | ksm_button5)
} KeystateMask;

typedef enum mnemonic_focus
{
  mf_normal = 0,
  mf_tab,
  mf_mnemonic,
  mf_shortcut
} MnemonicFocus;

typedef enum event_type
{
  et_noevent = -1,
  et_char,
  et_charup,
  et_mousemove,
  et_mousedown,
  et_mouseup,
  et_crossing,                  /* these four are assumed to be consecutive */
  et_focus,
  et_expose,
  et_visibility,
  et_resize,
  et_timer,
  et_close,                     /*request by user */
  et_create,
  et_map,
  et_destroy,                   /*window being freed */
  et_selclear,
  et_drag,
  et_dragout,
  et_drop,
  et_lastnativeevent = et_drop,
  et_controlevent,
  et_user
} EventType;

typedef enum visibility_state
{
  vs_unobscured = 0,
  vs_partially,
  vs_obscured
} VisibilityState;

typedef enum et_subtype
{
  et_buttonpress = 0,
  et_buttonactivate,
  et_radiochanged,
  et_listselected,
  et_listdoubleclick,
  et_scrollbarchange,
  et_textchanged,
  et_textfocuschanged,
  et_lastsubtype
} ET_Subtype;

typedef enum sb
{
  et_sb_top = 0,
  et_sb_uppage,
  et_sb_up,
  et_sb_left = et_sb_up,
  et_sb_down,
  et_sb_right = et_sb_down,
  et_sb_downpage,
  et_sb_bottom,
  et_sb_thumb,
  et_sb_thumbrelease
} SB;

typedef struct sbevent
{
  SB type;
  int32_t pos;
} SBEvent;

#define _GD_EVT_CHRLEN	10

typedef struct
{
  char *device;                 /* for wacom devices */
  uint32_t time;
  uint16_t state;
  int16_t x;
  int16_t y;
  uint16_t keysym;
  int16_t autorepeat;
  uint32_t chars[_GD_EVT_CHRLEN];
} GEvent_chr;

typedef struct
{
  char *device;                 /* for wacom devices */
  uint32_t time;
  int16_t state;
  int16_t x;
  int16_t y;
  int16_t button;
  int16_t clicks;
  int32_t pressure;
  int32_t xtilt;
  int32_t ytilt;
  int32_t separation;
} GEvent_mouse;

typedef struct
{
  GRect rect;
} GEvent_expose;

typedef struct
{
  enum visibility_state state;
} GEvent_visibility;

typedef struct
{
  GRect size;
  int16_t dx;
  int16_t dy;
  int16_t dwidth;
  int16_t dheight;
  bool moved;
  bool sized;
} GEvent_resize;

typedef struct
{
  char *device;                 /* for wacom devices */
  uint32_t time;
  int16_t state;
  int16_t x;
  int16_t y;
  bool entered;
} GEvent_crossing;

typedef struct
{
  bool gained_focus;
  uint8_t mnemonic_focus;
} GEvent_focus;

typedef struct
{
  bool is_visible;
} GEvent_map;

typedef struct
{
  enum selnames sel;
} GEvent_selclear;

typedef struct
{
  int32_t x;
  int32_t y;
} GEvent_drag_drop;

typedef struct
{
  GTimer *timer;
  void *userdata;
} GEvent_timer;

typedef struct
{
  int gained_focus;
} GEvent_control_tf_focus;

typedef struct
{
  int from_pulldown;            /* -1 normally, else index into pulldown list */
} GEvent_control_tf_changed;

typedef struct
{
  int clicks;
  int16_t button;
  int16_t state;
} GEvent_control_button;

typedef struct
{
  int from_mouse;
  int changed_index;
} GEvent_control_list;

typedef union
{
  SBEvent sb;
  GEvent_control_tf_focus tf_focus;
  GEvent_control_tf_changed tf_changed;
  GEvent_control_button button;
  GEvent_control_list list;
} GEvent_control_union;

typedef struct
{
  ET_Subtype subtype;
  struct ggadget *g;
  GEvent_control_union u;
} GEvent_control;

typedef struct
{
  long subtype;
  void *userdata;
} GEvent_user;

typedef union
{
  GEvent_chr chr;
  GEvent_mouse mouse;
  GEvent_expose expose;
  GEvent_visibility visibility;
  GEvent_resize resize;
  GEvent_crossing crossing;
  GEvent_focus focus;
  GEvent_map map;
  GEvent_selclear selclear;
  GEvent_drag_drop drag_drop;
  GEvent_timer timer;
  GEvent_control control;
  GEvent_user user;
} GEvent_union;

typedef struct gevent
{
  enum event_type type;
  GWindow w;
  GEvent_union u;
  void *native_window;
} GEvent;

typedef enum cursor_types
{
  ct_default,
  ct_pointer,
  ct_backpointer,
  ct_hand,
  ct_question,
  ct_cross,
  ct_4way,
  ct_text,
  ct_watch,
  ct_draganddrop,
  ct_leftright,
  ct_updown,
  ct_topleft,
  ct_topright,
  ct_bottomleft,
  ct_bottomright,
  ct_leftside,
  ct_rightside,
  ct_topside,
  ct_bottomside,
  ct_invisible,
  ct_user,
  ct_user2                      /* and so on */
} GCursor;

enum window_attr_mask
{
  wam_events = 0x2,
  wam_bordwidth = 0x4,
  wam_bordcol = 0x8,
  wam_backcol = 0x10,
  wam_cursor = 0x20,
  wam_wtitle = 0x40,
  wam_ititle = 0x80,
  wam_icon = 0x100,
  wam_nodecor = 0x200,
  wam_positioned = 0x400,
  wam_centered = 0x800,
  wam_undercursor = 0x1000,
  wam_noresize = 0x2000,
  wam_restrict = 0x4000,
  wam_redirect = 0x8000,
  wam_isdlg = 0x10000,
  wam_notrestricted = 0x20000,
  wam_transient = 0x40000,
  wam_utf8_wtitle = 0x80000,
  wam_utf8_ititle = 0x100000,
  wam_verytransient = 0x400000
};

typedef struct gwindow_attrs
{
//    enum window_attr_mask mask;
  unsigned int mask;
  uint32_t event_masks;         /* (1<<et_char) | (1<<et_mouseup) etc */
  int16_t border_width;
  Color border_color;           /* Color_UNKNOWN if unspecified */
  Color background_color;
  GCursor cursor;
  /* Remainder is only for top level windows */
  const uint32_t *window_title;
  const uint32_t *icon_title;
  struct gwindow *icon;         /* A bitmap pixmap, or NULL */
  bool nodecoration;            /* no wm decoration */
  bool positioned;              /* position information is important */
  bool centered;                /* center the window on the screen. pos.width&pos.height are used */
  bool undercursor;             /* center the window under the cursor. */
  bool noresize;                /* set min and max sizes to current size */
  bool restrict_input_to_me;    /* for dialogs, no input outside of dlg */
  bool redirect_chars_to_me;    /* ditto, we get any input outside of us */
  bool is_dlg;                  /* 1 for dlg, 0 for main window */
  bool not_restricted;          /* gets events if if a restricted (modal) dlg is up */
  GWindow redirect_from;        /* only redirect input from this window and its children */
  GWindow transient;            /* the Transient_FOR hint */
  const char *utf8_window_title;
  const char *utf8_icon_title;
} GWindowAttrs;

#define GWINDOWATTRS_EMPTY { 0, 0, 0, 0, 0, 0, NULL, NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0, NULL, NULL, NULL, NULL }


typedef struct gdeveventmask
{
  int event_mask;
  char *device_name;
} GDevEventMask;

enum gzoom_flags
{
  gzf_pos = 1,
  gzf_size = 2
};

typedef int (*GDrawEH) (GWindow, GEvent *);

extern uint32_t *GDrawKeysyms[];

VISIBLE extern GDisplay *screen_display;
VISIBLE extern GDisplay *printer_display;

VISIBLE extern void GDrawCreateDisplays (char *displayname,
                                         char *programname);
VISIBLE extern void *GDrawNativeDisplay (GDisplay *);
VISIBLE extern void GDrawTerm (GDisplay *disp);

VISIBLE extern int GDrawGetRes (GWindow gw);
VISIBLE extern int GDrawPointsToPixels (GWindow gw, int points);
VISIBLE extern int GDrawPixelsToPoints (GWindow gw, int pixels);

VISIBLE extern void GDrawSetDefaultIcon (GWindow icon);
VISIBLE extern GWindow GDrawCreateTopWindow (GDisplay *gdisp, GRect *pos,
                                             int (*eh) (GWindow, GEvent *),
                                             void *user_data,
                                             GWindowAttrs *wattrs);
VISIBLE extern GWindow GDrawCreateSubWindow (GWindow w, GRect *pos,
                                             int (*eh) (GWindow, GEvent *),
                                             void *user_data,
                                             GWindowAttrs *wattrs);
VISIBLE extern GWindow GDrawCreatePixmap (GDisplay *gdisp, uint16_t width,
                                          uint16_t height);
VISIBLE extern GWindow GDrawCreateBitmap (GDisplay *gdisp, uint16_t width,
                                          uint16_t height, uint8_t *data);
VISIBLE extern GCursor GDrawCreateCursor (char *name);
VISIBLE extern void GDrawDestroyWindow (GWindow w);
VISIBLE extern void GDrawDestroyCursor (GDisplay *gdisp, GCursor ct);
VISIBLE extern int GDrawNativeWindowExists (GDisplay *gdisp, void *native);
VISIBLE extern void GDrawSetZoom (GWindow w, GRect *zoomsize,
                                  enum gzoom_flags);
VISIBLE extern void GDrawSetWindowBorder (GWindow w, int width, Color color);
VISIBLE extern void GDrawSetWindowBackground (GWindow w, Color color);
VISIBLE extern int GDrawSetDither (GDisplay *gdisp, int dither);
VISIBLE extern void GDrawReparentWindow (GWindow child, GWindow newparent,
                                         int x, int y);
VISIBLE extern void GDrawSetVisible (GWindow w, int visible);
VISIBLE extern int GDrawIsVisible (GWindow w);
VISIBLE extern void GDrawTrueMove (GWindow w, int32_t x, int32_t y);
VISIBLE extern void GDrawMove (GWindow w, int32_t x, int32_t y);
VISIBLE extern void GDrawResize (GWindow w, int32_t width, int32_t height);
VISIBLE extern void GDrawMoveResize (GWindow w, int32_t x, int32_t y,
                                     int32_t width, int32_t height);
VISIBLE extern GWindow GDrawGetRoot (GDisplay *);
VISIBLE extern Color GDrawGetDefaultBackground (GDisplay *);
VISIBLE extern Color GDrawGetDefaultForeground (GDisplay *);
VISIBLE extern GRect *GDrawGetSize (GWindow w, GRect *ret);
VISIBLE extern GDrawEH GDrawGetEH (GWindow w);
VISIBLE extern void GDrawSetEH (GWindow w, GDrawEH e_h);
VISIBLE extern void GDrawGetPointerPosition (GWindow w, GEvent *mouse);
VISIBLE extern GWindow GDrawGetPointerWindow (GWindow w);
VISIBLE extern void GDrawRaise (GWindow w);
VISIBLE extern void GDrawRaiseAbove (GWindow w, GWindow below);
VISIBLE extern int GDrawIsAbove (GWindow w, GWindow other);
VISIBLE extern void GDrawLower (GWindow w);
VISIBLE extern void GDrawSetWindowTitles (GWindow w, const uint32_t *title,
                                          const uint32_t *icontit);
VISIBLE extern void GDrawSetWindowTitles8 (GWindow w, const char *title,
                                           const char *icontit);
VISIBLE extern uint32_t *GDrawGetWindowTitle (GWindow w);
VISIBLE extern char *GDrawGetWindowTitle8 (GWindow w);
VISIBLE extern void GDrawSetTransientFor (GWindow transient, GWindow owner);
VISIBLE extern void GDrawSetCursor (GWindow w, GCursor ct);
VISIBLE extern GCursor GDrawGetCursor (GWindow w);
VISIBLE extern GWindow GDrawGetRedirectWindow (GDisplay *gd);
VISIBLE extern GWindow GDrawGetParentWindow (GWindow gw);
VISIBLE extern int GDrawWindowIsAncestor (GWindow ancester,
                                          GWindow descendent);
VISIBLE extern void GDrawSetUserData (GWindow gw, void *ud);
VISIBLE extern void *GDrawGetUserData (GWindow gw);
VISIBLE extern GDisplay *GDrawGetDisplayOfWindow (GWindow);
VISIBLE extern void GDrawTranslateCoordinates (GWindow from, GWindow to,
                                               GPoint *pt);
VISIBLE extern int32_t GDrawEventInWindow (GWindow inme, GEvent *event);
VISIBLE extern void GDrawBeep (GDisplay *gdisp);
VISIBLE extern void GDrawFlush (GDisplay *gdisp);

VISIBLE extern void GDrawGetClip (GWindow w, GRect *ret);
VISIBLE extern void GDrawSetClip (GWindow w, GRect *rct);
VISIBLE extern void GDrawPushClip (GWindow w, GRect *rct, GRect *old);
VISIBLE extern void GDrawPopClip (GWindow w, GRect *old);
VISIBLE extern GGC *GDrawGetWindowGGC (GWindow w);
VISIBLE extern void GDrawSetXORBase (GWindow w, Color col);
VISIBLE extern void GDrawSetXORMode (GWindow w);
VISIBLE extern void GDrawSetCopyMode (GWindow w);
VISIBLE extern void GDrawSetCopyThroughSubWindows (GWindow w,
                                                   int16_t through);
VISIBLE extern void GDrawSetDashedLine (GWindow w, int16_t dash_len,
                                        int16_t skip_len, int16_t off);
VISIBLE extern void GDrawSetStippled (GWindow w, int16_t ts, int32_t yoff,
                                      int32_t xoff);
VISIBLE extern void GDrawSetLineWidth (GWindow w, int16_t width);
VISIBLE extern void GDrawSetForeground (GWindow w, Color col);
VISIBLE extern void GDrawSetBackground (GWindow w, Color col);

VISIBLE extern GFont *GDrawSetFont (GWindow gw, GFont *fi);
VISIBLE extern GFont *GDrawInstanciateFont (GWindow gw, FontRequest *rq);
VISIBLE extern GFont *GDrawNewFont (GWindow gw, char *family_name,
                                    int point_size, int weight,
                                    enum font_style style);
VISIBLE extern FontRequest *GDrawDecomposeFont (GFont *fi, FontRequest *rq);
VISIBLE extern void GDrawGetFontMetrics (GWindow gw, GFont *fi, int *as,
                                         int *ds, int *ld);

VISIBLE extern int32_t GDrawGetTextBounds (GWindow gw, const uint32_t *text,
                                           int32_t cnt, GTextBounds *size);
VISIBLE extern int32_t GDrawGetTextWidth (GWindow gw, const uint32_t *text,
                                          int32_t cnt);
VISIBLE extern int32_t GDrawDrawText (GWindow gw, int32_t x, int32_t y,
                                      const uint32_t *txt, int32_t cnt,
                                      Color col);

/* UTF8 routines */
VISIBLE extern int32_t GDrawGetText8Bounds (GWindow gw, const char *text,
                                            int32_t cnt, GTextBounds *size);
VISIBLE extern int32_t GDrawGetText8Width (GWindow gw, const char *text,
                                           int32_t cnt);
VISIBLE extern int32_t GDrawDrawText8 (GWindow gw, int32_t x, int32_t y,
                                       const char *txt, int32_t cnt,
                                       Color col);

VISIBLE extern GIC *GDrawCreateInputContext (GWindow w,
                                             enum gic_style def_style);
VISIBLE extern void GDrawSetGIC (GWindow w, GIC * gic, int x, int y);

VISIBLE extern void GDrawClear (GWindow w, GRect *rect);
VISIBLE extern void GDrawDrawLine (GWindow w, int32_t x, int32_t y,
                                   int32_t xend, int32_t yend, Color col);
VISIBLE extern void GDrawDrawRect (GWindow w, GRect *rect, Color col);
VISIBLE extern void GDrawFillRect (GWindow w, GRect *rect, Color col);
VISIBLE extern void GDrawFillRoundRect (GWindow w, GRect *rect, int radius,
                                        Color col);
VISIBLE extern void GDrawDrawElipse (GWindow w, GRect *rect, Color col);
VISIBLE extern void GDrawFillElipse (GWindow w, GRect *rect, Color col);
VISIBLE extern void GDrawDrawArc (GWindow w, GRect *rect, int32_t sangle,
                                  int32_t tangle, Color col);
VISIBLE extern void GDrawDrawPoly (GWindow w, GPoint *pts, int16_t cnt,
                                   Color col);
VISIBLE extern void GDrawFillPoly (GWindow w, GPoint *pts, int16_t cnt,
                                   Color col);
VISIBLE extern void GDrawScroll (GWindow w, GRect *rect, int32_t hor,
                                 int32_t vert);
VISIBLE extern void GDrawDrawImage (GWindow w, GImage *img, GRect *src,
                                    int32_t x, int32_t y);
VISIBLE extern void GDrawDrawImage2 (GWindow w, char *name, GRect *src,
                                     int32_t x, int32_t y);
VISIBLE extern void GDrawDrawGlyph (GWindow w, GImage *img, GRect *src,
                                    int32_t x, int32_t y);
VISIBLE extern void GDrawDrawScaledImage (GWindow w, GImage *img, int32_t x,
                                          int32_t y);
VISIBLE extern void GDrawDrawImageMagnified (GWindow w, GImage *img,
                                             GRect *src, int32_t x, int32_t y,
                                             int32_t width, int32_t height);
VISIBLE extern void GDrawDrawPixmap (GWindow w, GWindow pixmap, GRect *src,
                                     int32_t x, int32_t y);

VISIBLE extern void GDrawGrabSelection (GWindow w, enum selnames sel);
VISIBLE extern void GDrawAddSelectionType (GWindow w, enum selnames sel,
                                           char *type, void *data,
                                           int32_t cnt, int32_t unitsize,
                                           void *(*gendata) (void *,
                                                             int32_t *len),
                                           void (*freedata) (void *));
VISIBLE extern void *GDrawRequestSelection (GWindow w, enum selnames sn,
                                            char *typename_, int32_t *len);
VISIBLE extern int GDrawSelectionHasType (GWindow w, enum selnames sn,
                                          char *typename_);
VISIBLE extern void GDrawBindSelection (GDisplay *disp, enum selnames sel,
                                        char *atomname);
VISIBLE extern int GDrawSelectionOwned (GDisplay *disp, enum selnames sel);
VISIBLE extern void GDrawPropertyToSelectionOwner (GDisplay *disp,
                                                   enum selnames sel,
                                                   char *property, char *type,
                                                   int format, int mode,
                                                   uint8_t *data,
                                                   int nelements);

VISIBLE extern void GDrawPointerUngrab (GDisplay *disp);
VISIBLE extern void GDrawPointerGrab (GWindow w);
VISIBLE extern int GDrawEnableExposeRequests (GWindow w, int enabled);
VISIBLE extern void GDrawRequestExpose (GWindow w, GRect *rect, int doclear);
VISIBLE extern void GDrawSync (GDisplay *gdisp);
VISIBLE extern void GDrawForceUpdate (GWindow w);
VISIBLE extern void GDrawProcessOneEvent (GDisplay *disp);
VISIBLE extern void GDrawProcessPendingEvents (GDisplay *disp);
VISIBLE extern void GDrawProcessWindowEvents (GWindow w);
VISIBLE extern void GDrawSkipMouseMoveEvents (GWindow w, GEvent *last);
VISIBLE extern void GDrawEventLoop (GDisplay *disp);
VISIBLE extern void GDrawPostEvent (GEvent *e);
VISIBLE extern void GDrawPostDragEvent (GWindow gw, GEvent *e,
                                        enum event_type);

VISIBLE extern GTimer *GDrawRequestTimer (GWindow w, int32_t time_from_now,
                                          int32_t frequency, void *userdata);
VISIBLE extern void GDrawCancelTimer (GTimer * timer);

VISIBLE extern void GDrawSyncThread (GDisplay *gd, void (*func) (void *),
                                     void *data);

VISIBLE extern void GDrawSetBuildCharHooks (void (*hook) (GDisplay *),
                                            void (*inshook) (GDisplay *,
                                                             uint32_t));

VISIBLE extern int GDrawRequestDeviceEvents (GWindow w, int devcnt,
                                             struct gdeveventmask *de);

VISIBLE extern void GDrawPathStroke (GWindow w, Color col);
VISIBLE extern void GDrawPathFill (GWindow w, Color col);

VISIBLE extern void GDrawLayoutInit (GWindow w, char *text, int cnt,
                                     GFont *fi);
VISIBLE extern void GDrawLayoutDraw (GWindow w, int32_t x, int32_t y,
                                     Color fg);
VISIBLE extern void GDrawLayoutIndexToPos (GWindow w, int index, GRect *pos);
VISIBLE extern int GDrawLayoutXYToIndex (GWindow w, int x, int y);
VISIBLE extern void GDrawLayoutExtents (GWindow w, GRect *size);
extern void GDrawLayoutSetWidth (GWindow w, int width);
extern int GDrawLayoutLineCount (GWindow w);
extern int GDrawLayoutLineStart (GWindow w, int line);
VISIBLE extern cairo_t *GDrawGetCairo (GWindow w);

extern void GDrawFatalError (const char *fmt, ...);
VISIBLE extern void GDrawIError (const char *fmt, ...);
VISIBLE extern int GDrawKeyState (int keysym);

extern int GImageGetScaledWidth (GWindow gw, GImage *img);
extern int GImageGetScaledHeight (GWindow gw, GImage *img);

#endif // _FF_INTERNAL_GDRAW_H
