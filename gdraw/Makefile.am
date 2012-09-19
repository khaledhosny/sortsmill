# Copyright (C) 2000-2012 by George Williams
# Copyright (C) 2012 by Barry Schwartz
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# The name of the author may not be used to endorse or promote products
# derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

include $(top_srcdir)/mk/flags.am
include $(top_srcdir)/mk/prep_symbols.mk

lib_LTLIBRARIES = libgdraw.la

libgdraw_la_SOURCES = choosericons.c ctlvalues.c drawboxborder.c		\
	gaskdlg.c gbuttons.c gcolor.c gcontainer.c gdraw.c					\
	gdrawbuildchars.c gdrawerror.c gdrawtxt.c gdrawtxtinit.c			\
	gfilechooser.c gfiledlg.c ggadgets.c ggroupbox.c gimageclut.c		\
	gimagecvt.c gimagepsdraw.c gimagewriteeps.c gdrawgimage.c			\
	gimagexdraw.c gkeysym.c glist.c gmenu.c gprogress.c gpsdraw.c		\
	gpstxtinit.c gradio.c gresource.c gresourceimage.c gresedit.c		\
	gsavefiledlg.c gscrollbar.c gtabset.c gtextfield.c gtextinfo.c		\
	gwidgets.c gxdraw.c gxcdraw.c ghvbox.c gmatrixedit.c gdrawable.c	\
	gspacer.c xkeysyms_unicode.c colorP.h gdrawP.h gimagebmpP.h			\
	gresourceP.h gxcdrawP.h fontP.h ggadgetP.h gpsdrawP.h gwidgetP.h	\
	gxdrawP.h

EXPORTED_LIBGDRAW_SYMBOLS = GBoxBorderWidth GBoxDrawBackground			\
	GBoxDrawBorder GButtonCreate GCheckBoxCreate						\
	GCompletionFieldSetCompletion GCompletionFieldSetCompletionMode		\
	GDrawAddSelectionType GDrawBeep GDrawBindSelection					\
	GDrawCancelTimer GDrawCreateBitmap GDrawCreateCursor				\
	GDrawCreateDisplays GDrawCreateInputContext GDrawCreatePixmap		\
	GDrawCreateTopWindow GDrawDecomposeFont GDrawDestroyWindow			\
	GDrawDrawArrow GDrawDrawElipse GDrawDrawGlyph GDrawDrawImage		\
	GDrawDrawImageMagnified GDrawDrawLine GDrawDrawPixmap				\
	GDrawDrawPoly GDrawDrawRect GDrawDrawText GDrawDrawText8			\
	GDrawEventLoop GDrawFillElipse GDrawFillPoly GDrawFillRect			\
	GDrawFillRuleSetWinding GDrawGetCursor GDrawGetDefaultBackground	\
	GDrawGetDefaultForeground GDrawGetDisplayOfWindow					\
	GDrawGetParentWindow GDrawGetPointerPosition						\
	GDrawGetPointerWindow GDrawGetRoot GDrawGetSize						\
	GDrawGetText8Bounds GDrawGetText8Width GDrawGetTextBounds			\
	GDrawGetTextWidth GDrawGetUserData GDrawGetWindowTitle				\
	GDrawGrabSelection GDrawIError GDrawInstanciateFont					\
	GDrawIsVisible GDrawKeyState GDrawLayoutDraw GDrawLayoutExtents		\
	GDrawLayoutIndexToPos GDrawLayoutInit GDrawLayoutXYToIndex			\
	GDrawMove GDrawMoveResize GDrawPathClose GDrawPathCurveTo			\
	GDrawPathFill GDrawPathLineTo GDrawPathMoveTo						\
	GDrawPathStartSubNew GDrawPathStroke GDrawPixelsToPoints			\
	GDrawPointsToPixels GDrawPopClip GDrawPostDragEvent					\
	GDrawPostEvent GDrawProcessOneEvent GDrawProcessPendingEvents		\
	GDrawPushClip GDrawRaise GDrawReparentWindow						\
	GDrawRequestDeviceEvents GDrawRequestExpose GDrawRequestSelection	\
	GDrawRequestTimer GDrawResize GDrawScroll GDrawSelectionHasType		\
	GDrawSelectionOwned GDrawSetBuildCharHooks GDrawSetCopyMode			\
	GDrawSetCursor GDrawSetDashedLine GDrawSetDefaultIcon				\
	GDrawSetDither GDrawSetEH GDrawSetFont GDrawSetGIC					\
	GDrawSetLineWidth GDrawSetStippled GDrawSetTransientFor				\
	GDrawSetUserData GDrawSetVisible GDrawSetWindowBorder				\
	GDrawSetWindowTitles GDrawSetWindowTitles8 GDrawSetXORBase			\
	GDrawSetXORMode GDrawSetZoom GDrawSkipMouseMoveEvents GDrawSync		\
	GDrawTranslateCoordinates GDrawTrueMove GDrawWindowFontMetrics		\
	GDrawableCreate GDrawableGetWindow GFileChooserConnectButtons		\
	GFileChooserCreate GFileChooserDefFilter							\
	GFileChooserFileNameOfPos GFileChooserFilterEh						\
	GFileChooserGetBookmarks GFileChooserGetChildren					\
	GFileChooserGetDir GFileChooserGetDirectoryPlacement				\
	GFileChooserGetShowHidden GFileChooserPopupCheck					\
	GFileChooserPosIsDir GFileChooserRefreshList						\
	GFileChooserReplaceIO GFileChooserSetBookmarks GFileChooserSetDir	\
	GFileChooserSetDirectoryPlacement GFileChooserSetFilterText			\
	GFileChooserSetPrefsChangedCallback GFileChooserSetShowHidden		\
	GGadgetActiveGadgetEditCmd GGadgetClearList GGadgetDispatchEvent	\
	GGadgetEndPopup GGadgetGetCid GGadgetGetDesiredSize					\
	GGadgetGetFirstListSelectedItem GGadgetGetList GGadgetGetListItem	\
	GGadgetGetListItemSelected GGadgetGetSize GGadgetGetTitle			\
	GGadgetGetTitle8 GGadgetGetUserData GGadgetGetWindow				\
	GGadgetImageCache GGadgetInit GGadgetInnerWithin GGadgetIsChecked	\
	GGadgetIsEnabled GGadgetIsListItemSelected GGadgetIsVisible			\
	GGadgetMove GGadgetPopupExternalEvent GGadgetPreparePopup			\
	GGadgetPreparePopup8 GGadgetPreparePopupImage GGadgetProtest8		\
	GGadgetRedraw GGadgetResize GGadgetScale GGadgetScrollListToPos		\
	GGadgetSelectListItem GGadgetSelectOneListItem GGadgetSetChecked	\
	GGadgetSetDesiredSize GGadgetSetEnabled GGadgetSetFont				\
	GGadgetSetImageDir GGadgetSetList GGadgetSetTitle					\
	GGadgetSetTitle8 GGadgetSetTitle8WithMn GGadgetSetUserData			\
	GGadgetSetVisible GGadgetTakesKeyboard GGadgetWildMatch				\
	GGadgetWithin GGadgetsCreate GGroupCreate GHBoxCreate				\
	GHVBoxCreate GHVBoxFitWindow GHVBoxReflow GHVBoxSetExpandableCol	\
	GHVBoxSetExpandableRow GHVBoxSetPadding GHVGroupCreate				\
	GInsCharSetChar GIntGetResource GLabelCreate GLineCreate			\
	GListButtonCreate GListCreate GListFieldCreate GListIndexFromY		\
	GListSetPopupCallback GListSetSBAlwaysVisible						\
	GMatrixEditActivateRowCol GMatrixEditAddButtons GMatrixEditCreate	\
	GMatrixEditDeleteRow GMatrixEditEnableColumn GMatrixEditGet			\
	GMatrixEditGetActiveCol GMatrixEditGetActiveRow						\
	GMatrixEditGetColCnt GMatrixEditGetColumnChoices					\
	GMatrixEditScrollToRowCol GMatrixEditSet							\
	GMatrixEditSetBeforeDelete GMatrixEditSetCanUpDown					\
	GMatrixEditSetColumnChoices GMatrixEditSetColumnCompletion			\
	GMatrixEditSetEditable GMatrixEditSetMouseMoveReporter				\
	GMatrixEditSetNewText GMatrixEditSetOtherButtonEnable				\
	GMatrixEditSetRowMotionCallback GMatrixEditSetTextChangeReporter	\
	GMatrixEditSetUpDownVisible GMatrixEditShowColumn					\
	GMatrixEditStringDlg GMenu2BarCreate GMenuCreatePopupMenu			\
	GMenuIsCommand GMenuItem2ArrayCopy GMenuItem2ArrayFree				\
	GMenuItemArrayFree GMenuItemParseMask GMenuMask						\
	GMenuSetShortcutDomain GNumericFieldCreate GPasswordCreate			\
	GProgressChangeLine1_8 GProgressChangeLine2_8						\
	GProgressChangeStages GProgressChangeTotal GProgressEnableStop		\
	GProgressEndIndicator GProgressIncrementBy GProgressNext			\
	GProgressNextStage GProgressPauseTimer GProgressReset				\
	GProgressResumeTimer GProgressShow GProgressStartIndicator8			\
	GRadioCreate GResEdit GResEditFind GResourceAddResourceFile			\
	GResourceAddResourceString GResourceFind GResourceFindColor			\
	GResourceFindFont GResourceFindInt GResourceFindString				\
	GResourceProgramDir GResourceProgramName GResourceSetProg			\
	GResourceUseGetText GScrollBarCreate GScrollBarGetBounds			\
	GScrollBarSetBounds GScrollBarSetPos GSpacerCreate					\
	GStringGetResource GTabSetChangeTabName GTabSetCreate				\
	GTabSetGetSel GTabSetGetSubwindow GTabSetRemetric					\
	GTabSetRemoveTabByPos GTabSetSetEnabled GTabSetSetSel				\
	GTextAreaCreate GTextCompletionCreate GTextFieldCreate				\
	GTextFieldReplace GTextFieldSelect GTextFieldShow					\
	GTextInfoArrayFree GTextInfoArrayFromList GTextInfoFromChars		\
	GTextInfoListFree GVBoxCreate GVisibilityBoxCreate GWidgetAsk8		\
	GWidgetAskCentered8 GWidgetAskPassword8 GWidgetAskString8			\
	GWidgetChoices8 GWidgetChoicesB8 GWidgetChoicesBM8 GWidgetColor		\
	GWidgetCreateInsChar GWidgetCreateSubWindow							\
	GWidgetCreateTopWindow GWidgetError8 GWidgetFlowGadgets				\
	GWidgetGetControl GWidgetHidePalettes GWidgetIndicateFocusGadget	\
	GWidgetOpenFile8 GWidgetPostNotice8 GWidgetPostNoticeActive8		\
	GWidgetPostNoticeTimeout8 GWidgetReparentWindow						\
	GWidgetSaveAsFile8 GWidgetSaveAsFileWithGadget8						\
	GWidgetToDesiredSize GWindowClearFocusGadgetOfWindow				\
	GWindowGetFocusGadgetOfWindow GetCalmReal8 GetInt8 GetReal8			\
	GetUnicodeChar8 TryGGadgetImageCache _GButtonInit					\
	_GGadgetCloseGroup _GGadgetCopyDefaultBox _GGadgetFileToUString		\
	_GGadgetGetTitle _GGadgetInitDefaultBox _GGadget_Create				\
	_GGadget_FinalPosition _GGadget_button_box							\
	_GGadget_gtextfield_box _GIcon_dir _GMatrixEditGet					\
	_GMatrixEditGetActiveTextField _GScrollBar_Width					\
	_GWidgetGetGadgets _GWidgetPostNotice8 _GWidget_ClearGrabGadget		\
	_GWidget_SetGrabGadget _ggadgetFigureSize _ggadget_Default_Box		\
	_ggadget_default_font _ggadget_destroy _ggadget_getinnersize		\
	_ggadget_getsize _ggadget_move _ggadget_redraw _ggadget_resize		\
	_ggadget_setenabled _ggadget_setvisible screen_display				\
	utf82u_mncopy

libgdraw_la_CPPFLAGS = $(AM_CPPFLAGS)

libgdraw_la_LIBADD = $(top_builddir)/Unicode/libgunicode.la				\
	$(top_builddir)/gutils/libgutils.la $(top_builddir)/lib/libgnu.la

# Using -release instead of -version-info seems appropriate in the
# absence of a well defined ABI.
libgdraw_la_LDFLAGS =						\
	-release $(VERSION)						\
	$(MY_CFLAGS) $(MY_LIBS)	$(LTLIBINTL)	\
	-export-symbols-regex					\
	'$(call prep_symbols_regex,EXPORTED_LIBGDRAW_SYMBOLS)'

#--------------------------------------------------------------------------
