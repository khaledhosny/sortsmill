/* Copyright (C) 2013 by Barry Schwartz */
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

#ifndef FF_INTERNAL_MOVETO_FUNCS_H
#define FF_INTERNAL_MOVETO_FUNCS_H

#include <ggadget.h>

// Not belonging anywhere in particular.
_FF_GMENUITEM_FUNC (MenuRecentBuild);

// bitmapview.c
_FF_GMENUITEM_FUNC (BVWindowMenuBuild);
_FF_GMENUITEM_FUNC (MenuRecentBuild);
_FF_GMENUITEM_FUNC (edlistcheck_bv);
_FF_GMENUITEM_FUNC (ellistcheck_bv);
_FF_GMENUITEM_FUNC (fllistcheck_bv);
_FF_GMENUITEM_FUNC (mtlistcheck_bv);
_FF_GMENUITEM_FUNC (pllistcheck_bv);
_FF_GMENUITEM_FUNC (vwlistcheck_bv);

// charview.c
_FF_GMENUITEM_FUNC (CVWindowMenuBuild);
_FF_GMENUITEM_FUNC (allistcheck_cv);
_FF_GMENUITEM_FUNC (ap2listbuild_cv);
_FF_GMENUITEM_FUNC (aplistcheck_cv);
_FF_GMENUITEM_FUNC (balistcheck_cv);
_FF_GMENUITEM_FUNC (cblistcheck_cv);
_FF_GMENUITEM_FUNC (cv_tools_list_check);
_FF_GMENUITEM_FUNC (cvtoollist_check);
_FF_GMENUITEM_FUNC (delistcheck_cv);
_FF_GMENUITEM_FUNC (edlistcheck_cv);
_FF_GMENUITEM_FUNC (ellistcheck_cv);
_FF_GMENUITEM_FUNC (fllistcheck_cv);
_FF_GMENUITEM_FUNC (gflistcheck_cv);
_FF_GMENUITEM_FUNC (htlistcheck_cv);
_FF_GMENUITEM_FUNC (mmlistcheck_cv);
_FF_GMENUITEM_FUNC (mtlistcheck_cv);
_FF_GMENUITEM_FUNC (mvlistcheck_cv);
_FF_GMENUITEM_FUNC (nplistcheck_cv);
_FF_GMENUITEM_FUNC (orlistcheck_cv);
_FF_GMENUITEM_FUNC (pllistcheck_cv);
_FF_GMENUITEM_FUNC (ptlistcheck_cv);
_FF_GMENUITEM_FUNC (rndlistcheck_cv);
_FF_GMENUITEM_FUNC (sllistcheck_cv);
_FF_GMENUITEM_FUNC (smlistcheck_cv);
_FF_GMENUITEM_FUNC (swlistcheck_cv);
_FF_GMENUITEM_FUNC (vwlistcheck_cv);

// fontview.c
_FF_GMENUITEM_FUNC (FVEncodingMenuBuild);
_FF_GMENUITEM_FUNC (FVForceEncodingMenuBuild);
_FF_GMENUITEM_FUNC (FVWindowMenuBuild);
_FF_GMENUITEM_FUNC (aplistbuild_fv);
_FF_GMENUITEM_FUNC (balistcheck_fv);
_FF_GMENUITEM_FUNC (cblistcheck_fv);
_FF_GMENUITEM_FUNC (cdlistcheck_fv);
_FF_GMENUITEM_FUNC (cflistcheck_fv);
_FF_GMENUITEM_FUNC (delistcheck_fv);
_FF_GMENUITEM_FUNC (edlistcheck_fv);
_FF_GMENUITEM_FUNC (ellistcheck_fv);
_FF_GMENUITEM_FUNC (enlistcheck_fv);
_FF_GMENUITEM_FUNC (fllistcheck_fv);
_FF_GMENUITEM_FUNC (fv_tools_list_check);
_FF_GMENUITEM_FUNC (gllistcheck_fv);
_FF_GMENUITEM_FUNC (htlistcheck_fv);
_FF_GMENUITEM_FUNC (infolistcheck_fv);
_FF_GMENUITEM_FUNC (lylistcheck_fv);
_FF_GMENUITEM_FUNC (mmlistcheck_fv);
_FF_GMENUITEM_FUNC (mtlistcheck_fv);
_FF_GMENUITEM_FUNC (sllistcheck_fv);
_FF_GMENUITEM_FUNC (trlistcheck_fv);
_FF_GMENUITEM_FUNC (validlistcheck_fv);
_FF_GMENUITEM_FUNC (vwlistcheck_fv);

// metricsview.c
_FF_GMENUITEM_FUNC (MVWindowMenuBuild);
_FF_GMENUITEM_FUNC (aplistbuild_mv);
_FF_GMENUITEM_FUNC (balistcheck_mv);
_FF_GMENUITEM_FUNC (cblistcheck_mv);
_FF_GMENUITEM_FUNC (edlistcheck_mv);
_FF_GMENUITEM_FUNC (ellistcheck_mv);
_FF_GMENUITEM_FUNC (fllistcheck_mv);
_FF_GMENUITEM_FUNC (gdlistcheck_mv);
_FF_GMENUITEM_FUNC (lylistcheck_mv);
_FF_GMENUITEM_FUNC (mtlistcheck_mv);
_FF_GMENUITEM_FUNC (tylistcheck_mv);
_FF_GMENUITEM_FUNC (vwlistcheck_mv);


#endif	/* FF_INTERNAL_MOVETO_FUNCS_H */
