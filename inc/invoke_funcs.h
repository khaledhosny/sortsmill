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

#ifndef FF_INTERNAL_INVOKE_FUNCS_H
#define FF_INTERNAL_INVOKE_FUNCS_H

#include <ggadget.h>

// Not belonging anywhere in particular.
_FF_GMENUITEM_FUNC (MenuAbout);
_FF_GMENUITEM_FUNC (MenuHelp);
_FF_GMENUITEM_FUNC (MenuIndex);
_FF_GMENUITEM_FUNC (MenuSaveAll);
_FF_GMENUITEM_FUNC (MenuExit);
_FF_GMENUITEM_FUNC (MenuNew);
_FF_GMENUITEM_FUNC (MenuOpen);
_FF_GMENUITEM_FUNC (MenuPrefs);
_FF_GMENUITEM_FUNC (MenuXRes);
_FF_GMENUITEM_FUNC (_MenuWarnings);

// bitmapview.c
_FF_GMENUITEM_FUNC (BVClear);
_FF_GMENUITEM_FUNC (BVCopy);
_FF_GMENUITEM_FUNC (BVCopyRef);
_FF_GMENUITEM_FUNC (BVCut);
_FF_GMENUITEM_FUNC (BVMenuBDFInfo);
_FF_GMENUITEM_FUNC (BVMenuBitmaps);
_FF_GMENUITEM_FUNC (BVMenuChangeChar);
_FF_GMENUITEM_FUNC (BVMenuChangePixelSize);
_FF_GMENUITEM_FUNC (BVMenuClose);
_FF_GMENUITEM_FUNC (BVMenuExport);
_FF_GMENUITEM_FUNC (BVMenuFindInFontView);
_FF_GMENUITEM_FUNC (BVMenuFontInfo);
_FF_GMENUITEM_FUNC (BVMenuGenerate);
_FF_GMENUITEM_FUNC (BVMenuGenerateFamily);
_FF_GMENUITEM_FUNC (BVMenuGenerateTTC);
_FF_GMENUITEM_FUNC (BVMenuGetInfo);
_FF_GMENUITEM_FUNC (BVMenuGotoChar);
_FF_GMENUITEM_FUNC (BVMenuImport);
_FF_GMENUITEM_FUNC (BVMenuOpenMetrics);
_FF_GMENUITEM_FUNC (BVMenuOpenOutline);
_FF_GMENUITEM_FUNC (BVMenuPaletteShow);
_FF_GMENUITEM_FUNC (BVMenuPalettesDock);
_FF_GMENUITEM_FUNC (BVMenuRevert);
_FF_GMENUITEM_FUNC (BVMenuRmGlyph);
_FF_GMENUITEM_FUNC (BVMenuRotateInvoked);
_FF_GMENUITEM_FUNC (BVMenuSave);
_FF_GMENUITEM_FUNC (BVMenuSaveAs);
_FF_GMENUITEM_FUNC (BVMenuScale);
_FF_GMENUITEM_FUNC (BVMenuSetWidth);
_FF_GMENUITEM_FUNC (BVPaste);
_FF_GMENUITEM_FUNC (BVRedo);
_FF_GMENUITEM_FUNC (BVRemoveUndoes);
_FF_GMENUITEM_FUNC (BVSelectAll);
_FF_GMENUITEM_FUNC (BVUndo);
_FF_GMENUITEM_FUNC (BVUnlinkRef);

// charview.c
_FF_GMENUITEM_FUNC (CVClear);
_FF_GMENUITEM_FUNC (CVClearBackground);
_FF_GMENUITEM_FUNC (CVCopy);
_FF_GMENUITEM_FUNC (CVCopyFgBg);
_FF_GMENUITEM_FUNC (CVCopyLookupData);
_FF_GMENUITEM_FUNC (CVCopyRef);
_FF_GMENUITEM_FUNC (CVCopyWidth);
_FF_GMENUITEM_FUNC (CVCut);
_FF_GMENUITEM_FUNC (CVJoin);
_FF_GMENUITEM_FUNC (CVMenuAPDetach);
_FF_GMENUITEM_FUNC (CVMenuAPAttachSC);
_FF_GMENUITEM_FUNC (CVMenuAcceptableExtrema);
_FF_GMENUITEM_FUNC (CVMenuAddAnchor);
_FF_GMENUITEM_FUNC (CVMenuAddExtrema);
_FF_GMENUITEM_FUNC (CVMenuAddHint);
_FF_GMENUITEM_FUNC (CVMenuAnchorPairs);
_FF_GMENUITEM_FUNC (CVMenuAutoCounter);
_FF_GMENUITEM_FUNC (CVMenuAutoHint);
_FF_GMENUITEM_FUNC (CVMenuAutoHintSubs);
_FF_GMENUITEM_FUNC (CVMenuAutotrace);
_FF_GMENUITEM_FUNC (CVMenuBitmaps);
_FF_GMENUITEM_FUNC (CVMenuBuildAccent);
_FF_GMENUITEM_FUNC (CVMenuBuildComposite);
_FF_GMENUITEM_FUNC (CVMenuCanonicalContours);
_FF_GMENUITEM_FUNC (CVMenuCanonicalStart);
_FF_GMENUITEM_FUNC (CVMenuCenter);
_FF_GMENUITEM_FUNC (CVMenuCenterCP);
_FF_GMENUITEM_FUNC (CVMenuChangeChar);
_FF_GMENUITEM_FUNC (CVMenuChangeGlyph);
_FF_GMENUITEM_FUNC (CVMenuChangePointSize);
_FF_GMENUITEM_FUNC (CVMenuChangeXHeight);
_FF_GMENUITEM_FUNC (CVMenuCharInfo);
_FF_GMENUITEM_FUNC (CVMenuCheckSelf);
_FF_GMENUITEM_FUNC (CVMenuCleanupGlyph);
_FF_GMENUITEM_FUNC (CVMenuClearHints);
_FF_GMENUITEM_FUNC (CVMenuClearInstrs);
_FF_GMENUITEM_FUNC (CVMenuClipPath);
_FF_GMENUITEM_FUNC (CVMenuClose);
_FF_GMENUITEM_FUNC (CVMenuCloseTab);
_FF_GMENUITEM_FUNC (CVMenuCluster);
_FF_GMENUITEM_FUNC (CVMenuCompareL2L);
_FF_GMENUITEM_FUNC (CVMenuCondense);
_FF_GMENUITEM_FUNC (CVMenuConstrain);
_FF_GMENUITEM_FUNC (CVMenuCopyGridFit);
_FF_GMENUITEM_FUNC (CVMenuCopyL2L);
_FF_GMENUITEM_FUNC (CVMenuCorrectDir);
_FF_GMENUITEM_FUNC (CVMenuCreateHint);
_FF_GMENUITEM_FUNC (CVMenuDebug);
_FF_GMENUITEM_FUNC (CVMenuDefineAlmost);
_FF_GMENUITEM_FUNC (CVMenuDeltas);
_FF_GMENUITEM_FUNC (CVMenuDir);
_FF_GMENUITEM_FUNC (CVMenuDontAutoHint);
_FF_GMENUITEM_FUNC (CVMenuEditInstrs);
_FF_GMENUITEM_FUNC (CVMenuEmbolden);
_FF_GMENUITEM_FUNC (CVMenuExecute);
_FF_GMENUITEM_FUNC (CVMenuExport);
_FF_GMENUITEM_FUNC (CVMenuFill);
_FF_GMENUITEM_FUNC (CVMenuFindInFontView);
_FF_GMENUITEM_FUNC (CVMenuFindProblems);
_FF_GMENUITEM_FUNC (CVMenuFontInfo);
_FF_GMENUITEM_FUNC (CVMenuGenerate);
_FF_GMENUITEM_FUNC (CVMenuGenerateFamily);
_FF_GMENUITEM_FUNC (CVMenuGenerateTTC);
_FF_GMENUITEM_FUNC (CVMenuGetInfo);
_FF_GMENUITEM_FUNC (CVMenuGlyphSelfIntersects);
_FF_GMENUITEM_FUNC (CVMenuGotoChar);
_FF_GMENUITEM_FUNC (CVMenuImplicit);
_FF_GMENUITEM_FUNC (CVMenuImport);
_FF_GMENUITEM_FUNC (CVMenuInline);
_FF_GMENUITEM_FUNC (CVMenuInsertPt);
_FF_GMENUITEM_FUNC (CVMenuInsertText);
_FF_GMENUITEM_FUNC (CVMenuItalic);
_FF_GMENUITEM_FUNC (CVMenuKPCloseup);
_FF_GMENUITEM_FUNC (CVMenuKernPairs);
_FF_GMENUITEM_FUNC (CVMenuLigatures);
_FF_GMENUITEM_FUNC (CVMenuMakeFirst);
_FF_GMENUITEM_FUNC (CVMenuMakeLine);
_FF_GMENUITEM_FUNC (CVMenuMakeParallel);
_FF_GMENUITEM_FUNC (CVMenuMarkExtrema);
_FF_GMENUITEM_FUNC (CVMenuMarkPointsOfInflection);
_FF_GMENUITEM_FUNC (CVMenuNLTransform);
_FF_GMENUITEM_FUNC (CVMenuNameContour);
_FF_GMENUITEM_FUNC (CVMenuNextPrevCPt);
_FF_GMENUITEM_FUNC (CVMenuNextPrevPt);
_FF_GMENUITEM_FUNC (CVMenuNowakAutoInstr);
_FF_GMENUITEM_FUNC (CVMenuNumberPoints);
_FF_GMENUITEM_FUNC (CVMenuOblique);
_FF_GMENUITEM_FUNC (CVMenuOpenBitmap);
_FF_GMENUITEM_FUNC (CVMenuOpenMetrics);
_FF_GMENUITEM_FUNC (CVMenuOrder);
_FF_GMENUITEM_FUNC (CVMenuOutline);
_FF_GMENUITEM_FUNC (CVMenuOverlap);
_FF_GMENUITEM_FUNC (CVMenuPOV);
_FF_GMENUITEM_FUNC (CVMenuPaletteShow);
_FF_GMENUITEM_FUNC (CVMenuPalettesDock);
_FF_GMENUITEM_FUNC (CVMenuPatternTile);
_FF_GMENUITEM_FUNC (CVMenuPointType);
_FF_GMENUITEM_FUNC (CVMenuPreview);
_FF_GMENUITEM_FUNC (CVMenuPrint);
_FF_GMENUITEM_FUNC (CVMenuReblend);
_FF_GMENUITEM_FUNC (CVMenuRemoveKern);
_FF_GMENUITEM_FUNC (CVMenuRemoveVKern);
_FF_GMENUITEM_FUNC (CVMenuReverseDir);
_FF_GMENUITEM_FUNC (CVMenuRevert);
_FF_GMENUITEM_FUNC (CVMenuRevertGlyph);
_FF_GMENUITEM_FUNC (CVMenuReviewHints);
_FF_GMENUITEM_FUNC (CVMenuRound2Hundredths);
_FF_GMENUITEM_FUNC (CVMenuRound2Int);
_FF_GMENUITEM_FUNC (CVMenuSave);
_FF_GMENUITEM_FUNC (CVMenuSaveAs);
_FF_GMENUITEM_FUNC (CVMenuScale);
_FF_GMENUITEM_FUNC (CVMenuSelectContours);
_FF_GMENUITEM_FUNC (CVMenuSelectPointAt);
_FF_GMENUITEM_FUNC (CVMenuSetWidth);
_FF_GMENUITEM_FUNC (CVMenuShadow);
_FF_GMENUITEM_FUNC (CVMenuShowAlmostHV);
_FF_GMENUITEM_FUNC (CVMenuShowAlmostHVCurves);
_FF_GMENUITEM_FUNC (CVMenuShowCPInfo);
_FF_GMENUITEM_FUNC (CVMenuShowDependentRefs);
_FF_GMENUITEM_FUNC (CVMenuShowDependentSubs);
_FF_GMENUITEM_FUNC (CVMenuShowGridFit);
_FF_GMENUITEM_FUNC (CVMenuShowHide);
_FF_GMENUITEM_FUNC (CVMenuShowHideRulers);
_FF_GMENUITEM_FUNC (CVMenuShowHints);
_FF_GMENUITEM_FUNC (CVMenuShowMMMask);
_FF_GMENUITEM_FUNC (CVMenuShowRefNames);
_FF_GMENUITEM_FUNC (CVMenuShowSideBearings);
_FF_GMENUITEM_FUNC (CVMenuShowTabs);
_FF_GMENUITEM_FUNC (CVMenuSimplify);
_FF_GMENUITEM_FUNC (CVMenuSimplifyMore);
_FF_GMENUITEM_FUNC (CVMenuSnapOutlines);
_FF_GMENUITEM_FUNC (CVMenuSpiroMakeFirst);
_FF_GMENUITEM_FUNC (CVMenuStroke);
_FF_GMENUITEM_FUNC (CVMenuTilePath);
_FF_GMENUITEM_FUNC (CVMenuTransform);
_FF_GMENUITEM_FUNC (CVMenuWireframe);
_FF_GMENUITEM_FUNC (CVMerge);
_FF_GMENUITEM_FUNC (CVPaste);
_FF_GMENUITEM_FUNC (CVRedo);
_FF_GMENUITEM_FUNC (CVRemoveUndoes);
_FF_GMENUITEM_FUNC (CVSelectAll);
_FF_GMENUITEM_FUNC (CVSelectHM);
_FF_GMENUITEM_FUNC (CVSelectInvert);
_FF_GMENUITEM_FUNC (CVSelectNone);
_FF_GMENUITEM_FUNC (CVSelectOpenContours);
_FF_GMENUITEM_FUNC (CVSelectVWidth);
_FF_GMENUITEM_FUNC (CVSelectWidth);
_FF_GMENUITEM_FUNC (CVUndo);
_FF_GMENUITEM_FUNC (CVUnlinkRef);

// combinations.c
_FF_GMENUITEM_FUNC (KPMenuACB);
_FF_GMENUITEM_FUNC (KPMenuACM);
_FF_GMENUITEM_FUNC (KPMenuKPCloseup);
_FF_GMENUITEM_FUNC (KPMenuRemove);

// cvdebug.c
_FF_GMENUITEM_FUNC (DVMenuCreate);

// cvpalettes.c
_FF_GMENUITEM_FUNC (CVMenuSpiroSet);
_FF_GMENUITEM_FUNC (CVMenuTool);

// fontinfo.c
_FF_GMENUITEM_FUNC (lookupmenu_dispatch);

// fontview.c
_FF_GMENUITEM_FUNC (FVMenuAddEncodingName);
_FF_GMENUITEM_FUNC (FVMenuAddExtrema);
_FF_GMENUITEM_FUNC (FVMenuAddUnencoded);
_FF_GMENUITEM_FUNC (FVMenuAnchorPairs);
_FF_GMENUITEM_FUNC (FVMenuAutoCounter);
_FF_GMENUITEM_FUNC (FVMenuAutoHint);
_FF_GMENUITEM_FUNC (FVMenuAutoHintSubs);
_FF_GMENUITEM_FUNC (FVMenuAutoInstr);
_FF_GMENUITEM_FUNC (FVMenuAutotrace);
_FF_GMENUITEM_FUNC (FVMenuAutoWidth);
_FF_GMENUITEM_FUNC (FVMenuBaseHoriz);
_FF_GMENUITEM_FUNC (FVMenuBaseVert);
_FF_GMENUITEM_FUNC (FVMenuBDFInfo);
_FF_GMENUITEM_FUNC (FVMenuBitmaps);
_FF_GMENUITEM_FUNC (FVMenuBlendToNew);
_FF_GMENUITEM_FUNC (FVMenuBuildAccent);
_FF_GMENUITEM_FUNC (FVMenuBuildComposite);
_FF_GMENUITEM_FUNC (FVMenuBuildDuplicate);
_FF_GMENUITEM_FUNC (FVMenuCanonicalContours);
_FF_GMENUITEM_FUNC (FVMenuCanonicalStart);
_FF_GMENUITEM_FUNC (FVMenuCenter);
_FF_GMENUITEM_FUNC (FVMenuChangeChar);
_FF_GMENUITEM_FUNC (FVMenuChangeGlyph);
_FF_GMENUITEM_FUNC (FVMenuChangeLayer);
_FF_GMENUITEM_FUNC (FVMenuChangeMMBlend);
_FF_GMENUITEM_FUNC (FVMenuChangeSupplement);
_FF_GMENUITEM_FUNC (FVMenuChangeXHeight);
_FF_GMENUITEM_FUNC (FVMenuCharInfo);
_FF_GMENUITEM_FUNC (FVMenuCIDFontInfo);
_FF_GMENUITEM_FUNC (FVMenuCleanup);
_FF_GMENUITEM_FUNC (FVMenuClear);
_FF_GMENUITEM_FUNC (FVMenuClearBackground);
_FF_GMENUITEM_FUNC (FVMenuClearHints);
_FF_GMENUITEM_FUNC (FVMenuClearInstrs);
_FF_GMENUITEM_FUNC (FVMenuClose);
_FF_GMENUITEM_FUNC (FVMenuCluster);
_FF_GMENUITEM_FUNC (FVMenuCompact);
_FF_GMENUITEM_FUNC (FVMenuCompareFonts);
_FF_GMENUITEM_FUNC (FVMenuCompareL2L);
_FF_GMENUITEM_FUNC (FVMenuCondense);
_FF_GMENUITEM_FUNC (FVMenuContextualHelp);
_FF_GMENUITEM_FUNC (FVMenuConvert2CID);
_FF_GMENUITEM_FUNC (FVMenuConvertByCMap);
_FF_GMENUITEM_FUNC (FVMenuCopy);
_FF_GMENUITEM_FUNC (FVMenuCopyFgBg);
_FF_GMENUITEM_FUNC (FVMenuCopyFrom);
_FF_GMENUITEM_FUNC (FVMenuCopyL2L);
_FF_GMENUITEM_FUNC (FVMenuCopyLookupData);
_FF_GMENUITEM_FUNC (FVMenuCopyRef);
_FF_GMENUITEM_FUNC (FVMenuCopyWidth);
_FF_GMENUITEM_FUNC (FVMenuCorrectDir);
_FF_GMENUITEM_FUNC (FVMenuCorrectRefs);
_FF_GMENUITEM_FUNC (FVMenuCreateMM);
_FF_GMENUITEM_FUNC (FVMenuCut);
_FF_GMENUITEM_FUNC (FVMenuDefineGroups);
_FF_GMENUITEM_FUNC (FVMenuDeltas);
_FF_GMENUITEM_FUNC (FVMenuDeselectAll);
_FF_GMENUITEM_FUNC (FVMenuDetachAndRemoveGlyphs);
_FF_GMENUITEM_FUNC (FVMenuDetachGlyphs);
_FF_GMENUITEM_FUNC (FVMenuDisplayByGroups);
_FF_GMENUITEM_FUNC (FVMenuDisplaySubs);
_FF_GMENUITEM_FUNC (FVMenuDontAutoHint);
_FF_GMENUITEM_FUNC (FVMenuEditInstrs);
_FF_GMENUITEM_FUNC (FVMenuEditTable);
_FF_GMENUITEM_FUNC (FVMenuEmbolden);
_FF_GMENUITEM_FUNC (FVMenuExecute);
_FF_GMENUITEM_FUNC (FVMenuExit);
_FF_GMENUITEM_FUNC (FVMenuFindProblems);
_FF_GMENUITEM_FUNC (FVMenuFindRpl);
_FF_GMENUITEM_FUNC (FVMenuFlatten);
_FF_GMENUITEM_FUNC (FVMenuFlattenByCMap);
_FF_GMENUITEM_FUNC (FVMenuFontInfo);
_FF_GMENUITEM_FUNC (FVMenuForceEncode);
_FF_GMENUITEM_FUNC (FVMenuGenerate);
_FF_GMENUITEM_FUNC (FVMenuGenerateFamily);
_FF_GMENUITEM_FUNC (FVMenuGenerateTTC);
_FF_GMENUITEM_FUNC (FVMenuGlyphLabel);
_FF_GMENUITEM_FUNC (FVMenuGlyphsBoth);
_FF_GMENUITEM_FUNC (FVMenuGlyphsRefs);
_FF_GMENUITEM_FUNC (FVMenuGlyphsSplines);
_FF_GMENUITEM_FUNC (FVMenuGlyphsWhite);
_FF_GMENUITEM_FUNC (FVMenuGotoChar);
_FF_GMENUITEM_FUNC (FVMenuHistograms);
_FF_GMENUITEM_FUNC (FVMenuImport);
_FF_GMENUITEM_FUNC (FVMenuInline);
_FF_GMENUITEM_FUNC (FVMenuInsertBlank);
_FF_GMENUITEM_FUNC (FVMenuInsertFont);
_FF_GMENUITEM_FUNC (FVMenuInterpFonts);
_FF_GMENUITEM_FUNC (FVMenuInvertSelection);
_FF_GMENUITEM_FUNC (FVMenuItalic);
_FF_GMENUITEM_FUNC (FVMenuJoin);
_FF_GMENUITEM_FUNC (FVMenuJustify);
_FF_GMENUITEM_FUNC (FVMenuKernByClasses);
_FF_GMENUITEM_FUNC (FVMenuKernPairs);
_FF_GMENUITEM_FUNC (FVMenuKPCloseup);
_FF_GMENUITEM_FUNC (FVMenuLigatures);
_FF_GMENUITEM_FUNC (FVMenuLoadEncoding);
_FF_GMENUITEM_FUNC (FVMenuLoadNamelist);
_FF_GMENUITEM_FUNC (FVMenuMagnify);
_FF_GMENUITEM_FUNC (FVMenuMakeFromFont);
_FF_GMENUITEM_FUNC (FVMenuMakeNamelist);
_FF_GMENUITEM_FUNC (FVMenuMassRename);
_FF_GMENUITEM_FUNC (FVMenuMATHInfo);
_FF_GMENUITEM_FUNC (FVMenuMergeFonts);
_FF_GMENUITEM_FUNC (FVMenuMergeKern);
_FF_GMENUITEM_FUNC (FVMenuMMInfo);
_FF_GMENUITEM_FUNC (FVMenuMMValid);
_FF_GMENUITEM_FUNC (FVMenuNameGlyphs);
_FF_GMENUITEM_FUNC (FVMenuNLTransform);
_FF_GMENUITEM_FUNC (FVMenuOblique);
_FF_GMENUITEM_FUNC (FVMenuOpenBitmap);
_FF_GMENUITEM_FUNC (FVMenuOpenMetrics);
_FF_GMENUITEM_FUNC (FVMenuOpenOutline);
_FF_GMENUITEM_FUNC (FVMenuOutline);
_FF_GMENUITEM_FUNC (FVMenuOverlap);
_FF_GMENUITEM_FUNC (FVMenuPaste);
_FF_GMENUITEM_FUNC (FVMenuPasteAfter);
_FF_GMENUITEM_FUNC (FVMenuPasteInto);
_FF_GMENUITEM_FUNC (FVMenuPatternTile);
_FF_GMENUITEM_FUNC (FVMenuPOV);
_FF_GMENUITEM_FUNC (FVMenuPrint);
_FF_GMENUITEM_FUNC (FVMenuRedo);
_FF_GMENUITEM_FUNC (FVMenuReencode);
_FF_GMENUITEM_FUNC (FVMenuRemoveEncoding);
_FF_GMENUITEM_FUNC (FVMenuRemoveFontFromCID);
_FF_GMENUITEM_FUNC (FVMenuRemoveKern);
_FF_GMENUITEM_FUNC (FVMenuRemoveUndoes);
_FF_GMENUITEM_FUNC (FVMenuRemoveUnused);
_FF_GMENUITEM_FUNC (FVMenuRemoveVKern);
_FF_GMENUITEM_FUNC (FVMenuRenameByNamelist);
_FF_GMENUITEM_FUNC (FVMenuReplaceWithRef);
_FF_GMENUITEM_FUNC (FVMenuRevert);
_FF_GMENUITEM_FUNC (FVMenuRevertBackup);
_FF_GMENUITEM_FUNC (FVMenuRevertGlyph);
_FF_GMENUITEM_FUNC (FVMenuRmInstrTables);
_FF_GMENUITEM_FUNC (FVMenuRound2Hundredths);
_FF_GMENUITEM_FUNC (FVMenuRound2Int);
_FF_GMENUITEM_FUNC (FVMenuSameGlyphAs);
_FF_GMENUITEM_FUNC (FVMenuSave);
_FF_GMENUITEM_FUNC (FVMenuSaveAs);
_FF_GMENUITEM_FUNC (FVMenuSelectAll);
_FF_GMENUITEM_FUNC (FVMenuSelectAutohintable);
_FF_GMENUITEM_FUNC (FVMenuSelectByName);
_FF_GMENUITEM_FUNC (FVMenuSelectByPST);
_FF_GMENUITEM_FUNC (FVMenuSelectByScript);
_FF_GMENUITEM_FUNC (FVMenuSelectChanged);
_FF_GMENUITEM_FUNC (FVMenuSelectColor);
_FF_GMENUITEM_FUNC (FVMenuSelectHintingNeeded);
_FF_GMENUITEM_FUNC (FVMenuSelectWorthOutputting);
_FF_GMENUITEM_FUNC (FVMenuSetColor);
_FF_GMENUITEM_FUNC (FVMenuSetExtremumBound);
_FF_GMENUITEM_FUNC (FVMenuSetWidth);
_FF_GMENUITEM_FUNC (FVMenuShadow);
_FF_GMENUITEM_FUNC (FVMenuShowAtt);
_FF_GMENUITEM_FUNC (FVMenuShowBitmap);
_FF_GMENUITEM_FUNC (FVMenuShowDependentRefs);
_FF_GMENUITEM_FUNC (FVMenuShowDependentSubs);
_FF_GMENUITEM_FUNC (FVMenuShowGroup);
_FF_GMENUITEM_FUNC (FVMenuShowMetrics);
_FF_GMENUITEM_FUNC (FVMenuShowSubFont);
_FF_GMENUITEM_FUNC (FVMenuSimplify);
_FF_GMENUITEM_FUNC (FVMenuSimplifyMore);
_FF_GMENUITEM_FUNC (FVMenuSize);
_FF_GMENUITEM_FUNC (FVMenuSmallCaps);
_FF_GMENUITEM_FUNC (FVMenuStroke);
_FF_GMENUITEM_FUNC (FVMenuSubSup);
_FF_GMENUITEM_FUNC (FVMenuTilePath);
_FF_GMENUITEM_FUNC (FVMenuTransform);
_FF_GMENUITEM_FUNC (FVMenuUndo);
_FF_GMENUITEM_FUNC (FVMenuUnlinkRef);
_FF_GMENUITEM_FUNC (FVMenuValidate);
_FF_GMENUITEM_FUNC (FVMenuVKernByClasses);
_FF_GMENUITEM_FUNC (FVMenuVKernFromHKern);
_FF_GMENUITEM_FUNC (FVMenuWireframe);
_FF_GMENUITEM_FUNC (FVMenuWSize);

// gfilechooser.c
_FF_GMENUITEM_FUNC (GFCAddCur);
_FF_GMENUITEM_FUNC (GFCBack);
_FF_GMENUITEM_FUNC (GFCDirsAmidToggle);
_FF_GMENUITEM_FUNC (GFCDirsFirstToggle);
_FF_GMENUITEM_FUNC (GFCDirsSeparateToggle);
_FF_GMENUITEM_FUNC (GFCForward);
_FF_GMENUITEM_FUNC (GFCHideToggle);
_FF_GMENUITEM_FUNC (GFCRefresh);
_FF_GMENUITEM_FUNC (GFCRemoveBook);

// gtextfield.c
_FF_GMENUITEM_FUNC (GTFPopupInvoked);

// kernclass.c
_FF_GMENUITEM_FUNC (kernmenu_dispatch);

// metricsview.c
_FF_GMENUITEM_FUNC (MVClear);
_FF_GMENUITEM_FUNC (MVClearSelection);
_FF_GMENUITEM_FUNC (MVCopy);
_FF_GMENUITEM_FUNC (MVCut);
_FF_GMENUITEM_FUNC (MVMenuAA);
_FF_GMENUITEM_FUNC (MVMenuAddExtrema);
_FF_GMENUITEM_FUNC (MVMenuAnchorPairs);
_FF_GMENUITEM_FUNC (MVMenuAutotrace);
_FF_GMENUITEM_FUNC (MVMenuBitmaps);
_FF_GMENUITEM_FUNC (MVMenuBuildAccent);
_FF_GMENUITEM_FUNC (MVMenuBuildComposite);
_FF_GMENUITEM_FUNC (MVMenuCenter);
_FF_GMENUITEM_FUNC (MVMenuChangeChar);
_FF_GMENUITEM_FUNC (MVMenuChangeLayer);
_FF_GMENUITEM_FUNC (MVMenuChangePointSize);
_FF_GMENUITEM_FUNC (MVMenuCharInfo);
_FF_GMENUITEM_FUNC (MVMenuCleanup);
_FF_GMENUITEM_FUNC (MVMenuClose);
_FF_GMENUITEM_FUNC (MVMenuCopyRef);
_FF_GMENUITEM_FUNC (MVMenuCopyWidth);
_FF_GMENUITEM_FUNC (MVMenuCorrectDir);
_FF_GMENUITEM_FUNC (MVMenuFindInFontView);
_FF_GMENUITEM_FUNC (MVMenuFindProblems);
_FF_GMENUITEM_FUNC (MVMenuFontInfo);
_FF_GMENUITEM_FUNC (MVMenuGenerate);
_FF_GMENUITEM_FUNC (MVMenuGenerateFamily);
_FF_GMENUITEM_FUNC (MVMenuGenerateTTC);
_FF_GMENUITEM_FUNC (MVMenuInline);
_FF_GMENUITEM_FUNC (MVMenuInsertChar);
_FF_GMENUITEM_FUNC (MVMenuJoin);
_FF_GMENUITEM_FUNC (MVMenuKPCloseup);
_FF_GMENUITEM_FUNC (MVMenuKernByClasses);
_FF_GMENUITEM_FUNC (MVMenuKernPairs);
_FF_GMENUITEM_FUNC (MVMenuLigatures);
_FF_GMENUITEM_FUNC (MVMenuMergeKern);
_FF_GMENUITEM_FUNC (MVMenuOpenBitmap);
_FF_GMENUITEM_FUNC (MVMenuOpenOutline);
_FF_GMENUITEM_FUNC (MVMenuOutline);
_FF_GMENUITEM_FUNC (MVMenuOverlap);
_FF_GMENUITEM_FUNC (MVMenuPointSize);
_FF_GMENUITEM_FUNC (MVMenuPrint);
_FF_GMENUITEM_FUNC (MVMenuRound2Int);
_FF_GMENUITEM_FUNC (MVMenuSave);
_FF_GMENUITEM_FUNC (MVMenuSaveAs);
_FF_GMENUITEM_FUNC (MVMenuScale);
_FF_GMENUITEM_FUNC (MVMenuShadow);
_FF_GMENUITEM_FUNC (MVMenuShowBitmap);
_FF_GMENUITEM_FUNC (MVMenuShowDependents);
_FF_GMENUITEM_FUNC (MVMenuShowGrid);
_FF_GMENUITEM_FUNC (MVMenuSimplify);
_FF_GMENUITEM_FUNC (MVMenuSimplifyMore);
_FF_GMENUITEM_FUNC (MVMenuSizeWindow);
_FF_GMENUITEM_FUNC (MVMenuTilePath);
_FF_GMENUITEM_FUNC (MVMenuTransform);
_FF_GMENUITEM_FUNC (MVMenuVKernByClasses);
_FF_GMENUITEM_FUNC (MVMenuVKernFromHKern);
_FF_GMENUITEM_FUNC (MVMenuVertical);
_FF_GMENUITEM_FUNC (MVMenuWindowType);
_FF_GMENUITEM_FUNC (MVMenuWireframe);
_FF_GMENUITEM_FUNC (MVPaste);
_FF_GMENUITEM_FUNC (MVRedo);
_FF_GMENUITEM_FUNC (MVSelectAll);
_FF_GMENUITEM_FUNC (MVUndo);
_FF_GMENUITEM_FUNC (MVUnlinkRef);

// problems.c
_FF_GMENUITEM_FUNC (VWMenuAllExtrema);
_FF_GMENUITEM_FUNC (VWMenuConnect);
_FF_GMENUITEM_FUNC (VWMenuCorrectDir);
_FF_GMENUITEM_FUNC (VWMenuGoodExtrema);
_FF_GMENUITEM_FUNC (VWMenuGotoGlyph);
_FF_GMENUITEM_FUNC (VWMenuInlineFlippedRefs);
_FF_GMENUITEM_FUNC (VWMenuInlineRefs);
_FF_GMENUITEM_FUNC (VWMenuManyAllExtrema);
_FF_GMENUITEM_FUNC (VWMenuManyConnect);
_FF_GMENUITEM_FUNC (VWMenuManyCorrectDir);
_FF_GMENUITEM_FUNC (VWMenuManyGoodExtrema);
_FF_GMENUITEM_FUNC (VWMenuManyMark);
_FF_GMENUITEM_FUNC (VWMenuManyOverlap);
_FF_GMENUITEM_FUNC (VWMenuManySimplify);
_FF_GMENUITEM_FUNC (VWMenuMark);
_FF_GMENUITEM_FUNC (VWMenuOpenGlyph);
_FF_GMENUITEM_FUNC (VWMenuOverlap);
_FF_GMENUITEM_FUNC (VWMenuRevalidate);
_FF_GMENUITEM_FUNC (VWMenuRevalidateAll);
_FF_GMENUITEM_FUNC (VWMenuSelect);
_FF_GMENUITEM_FUNC (VWMenuSimplify);

// sftextfield.c
_FF_GMENUITEM_FUNC (SFTFPopupInvoked);

// showatt.c
_FF_GMENUITEM_FUNC (AttSaveM);

// uiutil.c
_FF_GMENUITEM_FUNC (WarnMenuClear);
_FF_GMENUITEM_FUNC (WarnMenuCopy);


#endif	/* FF_INTERNAL_INVOKE_FUNCS_H */