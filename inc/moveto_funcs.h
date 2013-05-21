/*
 * Copyright (C) 2013 Khaled Hosny and Barry Schwartz
 * This file is part of the Sorts Mill Tools.
 * 
 * Sorts Mill Tools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Sorts Mill Tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
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
