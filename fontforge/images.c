#include <config.h>

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
#include "fontforgeui.h"

GImage GIcon_press2ptr;
GImage GIcon_hand;
GImage GIcon_line;
GImage GIcon_pencil;
GImage GIcon_shift;
GImage GIcon_star;
GImage GIcon_poly;
GImage GIcon_elipse;
GImage GIcon_rrect;
GImage GIcon_rect;
GImage GIcon_squarecap;
GImage GIcon_roundjoin;
GImage GIcon_roundcap;
GImage GIcon_miterjoin;
GImage GIcon_buttcap;
GImage GIcon_beveljoin;
GImage GIcon_freehand;
GImage GIcon_greyfree;
GImage GIcon_pen;
GImage GIcon_knife;
GImage GIcon_scale;
GImage GIcon_flip;
GImage GIcon_skew;
GImage GIcon_rotate;
GImage GIcon_3drotate;
GImage GIcon_perspective;
GImage GIcon_tangent;
GImage GIcon_curve;
GImage GIcon_hvcurve;
GImage GIcon_corner;
GImage GIcon_spirocorner;
GImage GIcon_spirocurve;
GImage GIcon_spirog2curve;
GImage GIcon_spiroright;
GImage GIcon_spiroleft;
GImage GIcon_spirodisabled;
GImage GIcon_spiroup;
GImage GIcon_spirodown;
GImage GIcon_ruler;
GImage GIcon_pointer;
GImage GIcon_magnify;

GImage GIcon_midtangent;
GImage GIcon_midcurve;
GImage GIcon_midhvcurve;
GImage GIcon_midcorner;

/* Small (16x12) images */
GImage def_image;
GImage red_image;
GImage blue_image;
GImage green_image;
GImage magenta_image;
GImage yellow_image;
GImage cyan_image;
GImage white_image;
GImage customcolor_image;

GImage GIcon_small3drotate;
GImage GIcon_smallperspective;
GImage GIcon_smallskew;
GImage GIcon_smallscale;
GImage GIcon_smallrotate;
GImage GIcon_smallflip;
GImage GIcon_smalltangent;
GImage GIcon_smallcorner;
GImage GIcon_smallcurve;
GImage GIcon_smallhvcurve;
GImage GIcon_smallspirocorner;
GImage GIcon_smallspirog2curve;
GImage GIcon_smallspirocurve;
GImage GIcon_smallspiroright;
GImage GIcon_smallspiroleft;
GImage GIcon_smallmag;
GImage GIcon_smallknife;
GImage GIcon_smallhand;
GImage GIcon_smallpen;
GImage GIcon_smallpencil;
GImage GIcon_smallpointer;
GImage GIcon_smallruler;
GImage GIcon_smallelipse;
GImage GIcon_smallrect;
GImage GIcon_smallpoly;
GImage GIcon_smallstar;
GImage GIcon_FontForgeLogo;
GImage GIcon_FontForgeBack;
GImage GIcon_FontForgeGuide;

GImage GIcon_continue;
GImage GIcon_stepout;
GImage GIcon_stepover;
GImage GIcon_stepinto;
GImage GIcon_watchpnt;
GImage GIcon_menudelta;
GImage GIcon_exit;

GImage GIcon_Stopped;
GImage GIcon_Stop;

GImage GIcon_lock;

GImage OFL_logo;

GImage GIcon_mag;
GImage GIcon_angle;
GImage GIcon_distance;
GImage GIcon_selectedpoint;
GImage GIcon_sel2ptr;
GImage GIcon_rightpointer;

GImage GIcon_u45fItalic;
GImage GIcon_u452Italic;
GImage GIcon_u448Italic;
GImage GIcon_u444Italic;
GImage GIcon_u442Italic;
GImage GIcon_u43fItalic;
GImage GIcon_u438Italic;
GImage GIcon_pItalic;
GImage GIcon_f2Italic;
GImage GIcon_fItalic;
GImage GIcon_aItalic;
GImage GIcon_FlatSerif;
GImage GIcon_SlantSerif;
GImage GIcon_PenSerif;
GImage GIcon_TopSerifs;
GImage GIcon_BottomSerifs;
GImage GIcon_DiagSerifs;

// FIXME: what to do when an icon cannot be found?
void InitToolIcons(void) {
    static bool done = false;
    
    if (!done) {
        done = true;
        GGadgetInit();

        TryGGadgetImageCache(&GIcon_FontForgeLogo, "fflogo.png");
        TryGGadgetImageCache(&GIcon_FontForgeBack, "ffback.png");
        TryGGadgetImageCache(&GIcon_FontForgeGuide, "ffguide.png");
        TryGGadgetImageCache(&OFL_logo, "ofllogo.png");

        /* Large 24x24 icons for CharView tool palettes */
        TryGGadgetImageCache(&GIcon_hand, "palettehand.png");
        TryGGadgetImageCache(&GIcon_line, "paletteline.png");
        TryGGadgetImageCache(&GIcon_pencil, "palettepencil.png");
        TryGGadgetImageCache(&GIcon_shift, "paletteshift.png");
        TryGGadgetImageCache(&GIcon_star, "palettestar.png");
        TryGGadgetImageCache(&GIcon_poly, "palettepoly.png");
        TryGGadgetImageCache(&GIcon_elipse, "paletteelipse.png");
        TryGGadgetImageCache(&GIcon_rect, "paletterect.png");
        TryGGadgetImageCache(&GIcon_freehand, "palettefreehand.png");
        TryGGadgetImageCache(&GIcon_greyfree, "palettegreyfree.png");
        TryGGadgetImageCache(&GIcon_pen, "palettepen.png");
        TryGGadgetImageCache(&GIcon_knife, "paletteknife.png");
        TryGGadgetImageCache(&GIcon_scale, "palettescale.png");
        TryGGadgetImageCache(&GIcon_flip, "paletteflip.png");
        TryGGadgetImageCache(&GIcon_skew, "paletteskew.png");
        TryGGadgetImageCache(&GIcon_rotate, "paletterotate.png");
        TryGGadgetImageCache(&GIcon_3drotate, "palette3drotate.png");
        TryGGadgetImageCache(&GIcon_perspective, "paletteperspective.png");
        TryGGadgetImageCache(&GIcon_tangent, "palettetangent.png");
        TryGGadgetImageCache(&GIcon_curve, "palettecurve.png");
        TryGGadgetImageCache(&GIcon_hvcurve, "palettehvcurve.png");
        TryGGadgetImageCache(&GIcon_corner, "palettecorner.png");
        TryGGadgetImageCache(&GIcon_spirocorner, "palettespirocorner.png");
        TryGGadgetImageCache(&GIcon_spirocurve, "palettespirocurve.png");
        TryGGadgetImageCache(&GIcon_spirog2curve, "palettespirog2curve.png");
        TryGGadgetImageCache(&GIcon_spiroright, "palettespiroright.png");
        TryGGadgetImageCache(&GIcon_spiroleft, "palettespiroleft.png");
        TryGGadgetImageCache(&GIcon_spirodisabled, "palettespirodisabled.png");
        TryGGadgetImageCache(&GIcon_spiroup, "palettespiroup.png");
        TryGGadgetImageCache(&GIcon_spirodown, "palettespirodown.png");
        TryGGadgetImageCache(&GIcon_ruler, "paletteruler.png");
        TryGGadgetImageCache(&GIcon_pointer, "palettepointer.png");
        TryGGadgetImageCache(&GIcon_magnify, "palettemagnify.png");

        /* Small 16x12 icons for CharView tool palettes */
        TryGGadgetImageCache(&GIcon_small3drotate, "palettesmall3drotate.png");
        TryGGadgetImageCache(&GIcon_smallperspective, "palettesmallperspective.png");
        TryGGadgetImageCache(&GIcon_smallskew, "palettesmallskew.png");
        TryGGadgetImageCache(&GIcon_smallscale, "palettesmallscale.png");
        TryGGadgetImageCache(&GIcon_smallrotate, "palettesmallrotate.png");
        TryGGadgetImageCache(&GIcon_smallflip, "palettesmallflip.png");
        TryGGadgetImageCache(&GIcon_smalltangent, "palettesmalltangent.png");
        TryGGadgetImageCache(&GIcon_smallcorner, "palettesmallcorner.png");
        TryGGadgetImageCache(&GIcon_smallcurve, "palettesmallcurve.png");
        TryGGadgetImageCache(&GIcon_smallhvcurve, "palettesmallhvcurve.png");
        TryGGadgetImageCache(&GIcon_smallspirocorner, "palettesmallspirocorner.png");
        TryGGadgetImageCache(&GIcon_smallspirog2curve, "palettesmallspirog2curve.png");
        TryGGadgetImageCache(&GIcon_smallspirocurve, "palettesmallspirocurve.png");
        TryGGadgetImageCache(&GIcon_smallspiroright, "palettesmallspiroright.png");
        TryGGadgetImageCache(&GIcon_smallspiroleft, "palettesmallspiroleft.png");
        TryGGadgetImageCache(&GIcon_smallmag, "palettesmallmag.png");
        TryGGadgetImageCache(&GIcon_smallknife, "palettesmallknife.png");
        TryGGadgetImageCache(&GIcon_smallhand, "palettesmallhand.png");
        TryGGadgetImageCache(&GIcon_smallpen, "palettesmallpen.png");
        TryGGadgetImageCache(&GIcon_smallpencil, "palettesmallpencil.png");
        TryGGadgetImageCache(&GIcon_smallpointer, "palettesmallpointer.png");
        TryGGadgetImageCache(&GIcon_smallruler, "palettesmallruler.png");
        TryGGadgetImageCache(&GIcon_smallelipse, "palettesmallelipse.png");
        TryGGadgetImageCache(&GIcon_smallrect, "palettesmallrect.png");
        TryGGadgetImageCache(&GIcon_smallpoly, "palettesmallpoly.png");
        TryGGadgetImageCache(&GIcon_smallstar, "palettesmallstar.png");

        /* 16x16 icons for point types in point info window */
        /* Note: spiro mode uses smaller (16x12) icons from CharView tool palette. */
        TryGGadgetImageCache(&GIcon_midtangent, "ptinfotangent.png");
        TryGGadgetImageCache(&GIcon_midcurve, "ptinfocurve.png");
        TryGGadgetImageCache(&GIcon_midhvcurve, "ptinfohvcurve.png");
        TryGGadgetImageCache(&GIcon_midcorner, "ptinfocorner.png");

        /* Icons for truetype debugger */
        TryGGadgetImageCache(&GIcon_continue, "ttdebugcontinue.png");
        TryGGadgetImageCache(&GIcon_stepout, "ttdebugstepout.png");
        TryGGadgetImageCache(&GIcon_stepover, "ttdebugstepover.png");
        TryGGadgetImageCache(&GIcon_stepinto, "ttdebugstepinto.png");
        TryGGadgetImageCache(&GIcon_watchpnt, "ttdebugwatchpnt.png");
        TryGGadgetImageCache(&GIcon_menudelta, "ttdebugmenudelta.png");
        TryGGadgetImageCache(&GIcon_exit, "ttdebugexit.png");
        TryGGadgetImageCache(&GIcon_Stopped, "ttdebugstopped.png");
        TryGGadgetImageCache(&GIcon_Stop, "ttdebugstop.png");

        /* Icons for color menu */
        TryGGadgetImageCache(&def_image, "colordef.png");
        TryGGadgetImageCache(&red_image, "colorred.png");
        TryGGadgetImageCache(&blue_image, "colorblue.png");
        TryGGadgetImageCache(&green_image, "colorgreen.png");
        TryGGadgetImageCache(&magenta_image, "colormagenta.png");
        TryGGadgetImageCache(&yellow_image, "coloryellow.png");
        TryGGadgetImageCache(&cyan_image, "colorcyan.png");
        TryGGadgetImageCache(&white_image, "colorwhite.png");
        TryGGadgetImageCache(&customcolor_image, "colorcustom.png");

        /* icons for bitmap and char views */
        TryGGadgetImageCache(&GIcon_press2ptr, "bvpress2ptr.png");

        TryGGadgetImageCache(&GIcon_lock, "cvlock.png");
        TryGGadgetImageCache(&GIcon_mag, "cvmag.png");
        TryGGadgetImageCache(&GIcon_angle, "cvangle.png");
        TryGGadgetImageCache(&GIcon_distance, "cvdistance.png");
        TryGGadgetImageCache(&GIcon_selectedpoint, "cvselectedpoint.png");
        TryGGadgetImageCache(&GIcon_sel2ptr, "cvsel2ptr.png");
        TryGGadgetImageCache(&GIcon_rightpointer, "cvrightpointer.png");

        /* icons for scstylesui */
        TryGGadgetImageCache(&GIcon_u45fItalic, "scu45fitalic.png");
        TryGGadgetImageCache(&GIcon_u452Italic, "scu452italic.png");
        TryGGadgetImageCache(&GIcon_u448Italic, "scu448italic.png");
        TryGGadgetImageCache(&GIcon_u444Italic, "scu444italic.png");
        TryGGadgetImageCache(&GIcon_u442Italic, "scu442italic.png");
        TryGGadgetImageCache(&GIcon_u43fItalic, "scu43fitalic.png");
        TryGGadgetImageCache(&GIcon_u438Italic, "scu438italic.png");
        TryGGadgetImageCache(&GIcon_pItalic, "scpitalic.png");
        TryGGadgetImageCache(&GIcon_f2Italic, "scf2italic.png");
        TryGGadgetImageCache(&GIcon_fItalic, "scfitalic.png");
        TryGGadgetImageCache(&GIcon_aItalic, "scaitalic.png");
        TryGGadgetImageCache(&GIcon_FlatSerif, "scflatserif.png");
        TryGGadgetImageCache(&GIcon_SlantSerif, "scslantserif.png");
        TryGGadgetImageCache(&GIcon_PenSerif, "scpenserif.png");
        TryGGadgetImageCache(&GIcon_TopSerifs, "sctopserifs.png");
        TryGGadgetImageCache(&GIcon_BottomSerifs, "scbottomserifs.png");
        TryGGadgetImageCache(&GIcon_DiagSerifs, "scdiagserifs.png");
    }
}
