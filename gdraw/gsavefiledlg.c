#include <config.h>

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
#include <stdlib.h>
#include <string.h>
#include <xunistring.h>
#include "ustring.h"
#include "gfile.h"
#include "gdraw.h"
#include "gwidget.h"
#include "ggadget.h"
#include "ggadgetP.h"
#include "gio.h"

struct gfc_data {
    int done;
    uint32_t *ret;
    GGadget *gfc;
};

static void GFD_doesnt(GIOControl *gio) {
    /* The filename the user chose doesn't exist, so everything is happy */
    struct gfc_data *d = gio->userdata;
    d->done = true;
    GFileChooserReplaceIO(d->gfc,NULL);
}

static void GFD_exists(GIOControl *gio)
{
  /* The filename the user chose exists, ask user if s/he wants to overwrite */
  struct gfc_data *d = gio->userdata;

  const char *rcb[3];
  rcb[2]=NULL;
  rcb[0] = _("Replace");
  rcb[1] = _("Cancel");

  if ( GWidgetAsk8(_("File Exists"),rcb,0,1,_("File, %s, exists. Replace it?"),
		   x_gc_u32_to_u8 (u32_GFileBaseName(d->ret)))==0 )
      d->done = true;
  GFileChooserReplaceIO (d->gfc, NULL);
}

static int GFD_SaveOk(GGadget *g, GEvent *e) {
    if ( e->type==et_controlevent && e->u.control.subtype == et_buttonactivate ) {
	struct gfc_data *d = GDrawGetUserData(GGadgetGetWindow(g));
	GGadget *tf;
	GFileChooserGetChildren(d->gfc,NULL,NULL,&tf);
	if ( *_GGadgetGetTitle(tf)!='\0' ) {
	    d->ret = GGadgetGetTitle(d->gfc);
	    GIOfileExists(GFileChooserReplaceIO(d->gfc,
		    GIOCreate(d->ret,d,GFD_exists,GFD_doesnt)));
	}
    }
return( true );
}

static int GFD_Cancel(GGadget *g, GEvent *e) {
    if ( e->type==et_controlevent && e->u.control.subtype == et_buttonactivate ) {
	struct gfc_data *d = GDrawGetUserData(GGadgetGetWindow(g));
	d->done = true;
    }
return( true );
}

static void GFD_dircreated(GIOControl *gio) {
    struct gfc_data *d = gio->userdata;
    uint32_t *dir = x_u32_strdup_or_null(gio->path);

    GFileChooserReplaceIO(d->gfc,NULL);
    GFileChooserSetDir(d->gfc,dir);
    free(dir);
}

static void GFD_dircreatefailed(GIOControl *gio)
{
  /* We couldn't create the directory */
  struct gfc_data *d = gio->userdata;

  GWidgetError8(_("Couldn't create directory"),
		_("Couldn't create directory: %1$s\n%2$s\n%3$s"),
		(gio->error != NULL ? (char *) x_gc_u32_to_u8 (gio->error) : ""),
		(char *) NULL_PASSTHRU (gio->status, x_gc_u32_to_u8 (gio->status)));

  GFileChooserReplaceIO(d->gfc,NULL);
}

static int GFD_NewDir(GGadget *g, GEvent *e) {
    if ( e->type==et_controlevent && e->u.control.subtype == et_buttonactivate ) {
	struct gfc_data *d = GDrawGetUserData(GGadgetGetWindow(g));
	uint32_t *newdir;
	char *temp;
	temp = GWidgetAskString8(_("Create directory..."),NULL,_("Directory name?"));
	newdir = utf82u_copy(temp);
	free(temp);

	if ( newdir==NULL )
return( true );
	if ( !GFileIsAbsolute(x_gc_u32_strconv_to_locale(newdir))) {
	    uint32_t *temp = u32_GFileAppendFile(GFileChooserGetDir(d->gfc),newdir,false);
	    free(newdir);
	    newdir = x_u32_strdup_or_null (temp);
	}
	GIOmkDir(GFileChooserReplaceIO(d->gfc,
		GIOCreate(newdir,d,GFD_dircreated,GFD_dircreatefailed)));
	free(newdir);
    }
return( true );
}

static int e_h(GWindow gw, GEvent *event) {
    if ( event->type==et_close ) {
	struct gfc_data *d = GDrawGetUserData(gw);
	d->done = true;
    } else if ( event->type == et_map ) {
	/* Above palettes */
	GDrawRaise(gw);
    } else if ( event->type == et_char ) {
return( false );
    } else if ( event->type == et_mousemove ||
	    (event->type==et_mousedown && event->u.mouse.button==3 )) {
	struct gfc_data *d = GDrawGetUserData(gw);
	GFileChooserPopupCheck(d->gfc,event);
    } else if (( event->type==et_mouseup || event->type==et_mousedown ) &&
	    (event->u.mouse.button>=4 && event->u.mouse.button<=7) ) {
	struct gfc_data *d = GDrawGetUserData(gw);
return( GGadgetDispatchEvent((GGadget *) (d->gfc),event));
    }
return( true );
}

static uint32_t *GWidgetSaveAsFileWithGadget(const uint32_t *title, const uint32_t *defaultfile,
	const uint32_t *initial_filter, char **mimetypes,
	GFileChooserFilterType filter, GGadgetCreateData *optional_gcd) {
    GRect pos;
    GWindow gw;
    GWindowAttrs wattrs;
    GGadgetCreateData gcd[7], boxes[3], *varray[7], *harray[10];
    GTextInfo label[5];
    struct gfc_data d;
    GGadget *pulldown, *files, *tf;
    int bs = GIntGetResource(_NUM_Buttonsize), bsbigger, totwid;
    int vi;

    GProgressPauseTimer();
    memset(&wattrs,0,sizeof(wattrs));
    wattrs.mask = wam_events|wam_cursor|wam_wtitle|wam_undercursor|wam_restrict|wam_isdlg;
    wattrs.event_masks = ~(1<<et_charup);
    wattrs.restrict_input_to_me = 1;
    wattrs.is_dlg = 1;
    wattrs.undercursor = 1;
    wattrs.cursor = ct_pointer;
    wattrs.window_title = (uint32_t *) title;
    pos.x = pos.y = 0;
    totwid = GGadgetScale(223);
    bsbigger = 3*bs+4*14>totwid; totwid = bsbigger?3*bs+4*12:totwid;
    pos.width = GDrawPointsToPixels(NULL,totwid);
    pos.height = GDrawPointsToPixels(NULL,255);
    gw = GDrawCreateTopWindow(NULL,&pos,e_h,&d,&wattrs);

    memset(&label,0,sizeof(label));
    memset(&gcd,0,sizeof(gcd));
    memset(&boxes,0,sizeof(boxes));
    gcd[0].gd.pos.x = 12; gcd[0].gd.pos.y = 6;
    gcd[0].gd.pos.width = 223-24; gcd[0].gd.pos.height = 180;
    gcd[0].gd.flags = gg_visible | gg_enabled;
    gcd[0].gd.cid = 1000;
    gcd[0].creator = GFileChooserCreate;
    varray[0] = &gcd[0]; varray[1] = NULL; vi=2;

    if ( optional_gcd!=NULL ) {
	varray[vi++] = optional_gcd; varray[vi++] = NULL;
    }

    gcd[1].gd.pos.x = 12; gcd[1].gd.pos.y = 222-3;
    gcd[1].gd.pos.width = -1;
    gcd[1].gd.flags = gg_visible | gg_enabled | gg_but_default;
    label[1].text = (uint32_t *) _("_Save");
    label[1].text_is_1byte = true;
    label[1].text_has_mnemonic = true;
    gcd[1].gd.label = &label[1];
    gcd[1].gd.handle_controlevent = GFD_SaveOk;
    gcd[1].creator = GButtonCreate;
    harray[0] = GCD_Glue; harray[1] = &gcd[1];

    gcd[2].gd.pos.x = (totwid-bs)*100/GIntGetResource(_NUM_ScaleFactor)/2; gcd[2].gd.pos.y = 222;
    gcd[2].gd.pos.width = -1;
    gcd[2].gd.flags = gg_visible | gg_enabled;
    label[2].text = (uint32_t *) _("_Filter");
    label[2].text_is_1byte = true;
    label[2].text_has_mnemonic = true;
    gcd[2].gd.label = &label[2];
    gcd[2].gd.handle_controlevent = GFileChooserFilterEh;
    gcd[2].creator = GButtonCreate;
    harray[2] = GCD_Glue; harray[3] = &gcd[2];

    gcd[3].gd.pos.x = gcd[2].gd.pos.x; gcd[3].gd.pos.y = 192;
    gcd[3].gd.pos.width = -1;
    gcd[3].gd.flags = gg_visible | gg_enabled;
    label[3].text = (uint32_t *) C_("Directory", "_New");
    label[3].text_is_1byte = true;
    label[3].text_has_mnemonic = true;
    label[3].image = (GImage *) "chooserdir.png";
    label[3].image_precedes = false;
    gcd[3].gd.label = &label[3];
    gcd[3].gd.handle_controlevent = GFD_NewDir;
    gcd[3].creator = GButtonCreate;
    harray[4] = GCD_Glue; harray[5] = &gcd[3];

    gcd[4].gd.pos.x = -gcd[1].gd.pos.x; gcd[4].gd.pos.y = 222;
    gcd[4].gd.pos.width = -1;
    gcd[4].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
    label[4].text = (uint32_t *) _("_Cancel");
    label[4].text_is_1byte = true;
    label[4].text_has_mnemonic = true;
    gcd[4].gd.label = &label[4];
    gcd[4].gd.handle_controlevent = GFD_Cancel;
    gcd[4].creator = GButtonCreate;
    harray[6] = GCD_Glue; harray[7] = &gcd[4]; harray[8] = GCD_Glue; harray[9] = NULL;

    boxes[2].gd.flags = gg_visible | gg_enabled;
    boxes[2].gd.u.boxelements = harray;
    boxes[2].creator = GHBoxCreate;
    varray[vi++] = &boxes[2]; varray[vi++] = NULL;
    varray[vi++] = NULL;

    boxes[0].gd.pos.x = boxes[0].gd.pos.y = 2;
    boxes[0].gd.flags = gg_visible | gg_enabled;
    boxes[0].gd.u.boxelements = varray;
    boxes[0].creator = GHVGroupCreate;

    gcd[5].gd.pos.x = 2; gcd[5].gd.pos.y = 2;
    gcd[5].gd.pos.width = pos.width-4; gcd[5].gd.pos.height = pos.height-4;
    gcd[5].gd.flags = gg_enabled | gg_visible | gg_pos_in_pixels;
    gcd[5].creator = GGroupCreate;

    GGadgetsCreate(gw,boxes);
    GGadgetSetUserData(gcd[2].ret,gcd[0].ret);
    GHVBoxSetExpandableRow(boxes[0].ret,0);
    GHVBoxSetExpandableCol(boxes[2].ret,gb_expandgluesame);
    GHVBoxFitWindow(boxes[0].ret);

    GFileChooserConnectButtons(gcd[0].ret,gcd[1].ret,gcd[2].ret);
    GFileChooserSetFilterText(gcd[0].ret,initial_filter);
    GFileChooserSetFilterFunc(gcd[0].ret,filter);
    GFileChooserSetMimetypes(gcd[0].ret,mimetypes);
    GGadgetSetTitle(gcd[0].ret,defaultfile);
    GFileChooserGetChildren(gcd[0].ret,&pulldown,&files,&tf);
    GWidgetIndicateFocusGadget(tf);

    memset(&d,'\0',sizeof(d));
    d.gfc = gcd[0].ret;

    GDrawSetVisible(gw,true);
    while ( !d.done )
	GDrawProcessOneEvent(NULL);
    GDrawDestroyWindow(gw);
    GProgressResumeTimer();
return(d.ret);
}

char *GWidgetSaveAsFileWithGadget8(const char *title, const char *defaultfile,
	const char *initial_filter, char **mimetypes,
	GFileChooserFilterType filter, GGadgetCreateData *optional_gcd) {
    uint32_t *tit=NULL, *def=NULL, *filt=NULL, *ret;
    char *utf8_ret;

    if ( title!=NULL )
	tit = utf82u_copy(title);
    if ( defaultfile!=NULL )
	def = utf82u_copy(defaultfile);
    if ( initial_filter!=NULL )
	filt = utf82u_copy(initial_filter);
    ret = GWidgetSaveAsFileWithGadget(tit,def,filt,mimetypes,filter,optional_gcd);
    free(filt); free(def); free(tit);
    utf8_ret = NULL_PASSTHRU(ret, x_u32_to_u8 (ret));
    free(ret);
return( utf8_ret );
}

char *GWidgetSaveAsFile8(const char *title, const char *defaultfile,
	const char *initial_filter, char **mimetypes,
	GFileChooserFilterType filter) {
return( GWidgetSaveAsFileWithGadget8(title,defaultfile,initial_filter,mimetypes,
		filter, NULL ));
}
