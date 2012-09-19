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

include $(top_srcdir)/mk/layout.am
include $(top_srcdir)/mk/flags.am

#--------------------------------------------------------------------------

if THE_PROGRAMS

bin_PROGRAMS = fontforge
dist_man_MANS = fontforge.1

if NATIVE_SCRIPTING
nodist_bin_SCRIPTS = fontimage fontlint sfddiff
dist_man_MANS += fontimage.1 fontlint.1 sfddiff.1
endif NATIVE_SCRIPTING

endif THE_PROGRAMS

lib_LTLIBRARIES = libfontforge.la libfontforgeexe.la

#--------------------------------------------------------------------------

SUBDIRS = pixmaps

FFSCRIPT_HEADER = "\#!$(bindir)/fontforge -lang=ff"

%: %.pe
	echo "${FFSCRIPT_HEADER}" | cat - $< > $@
	chmod +x $@

%.c: %.pre
	$(SED) -e '1,$$ s/REPLACE_ME_WITH_MAJOR_VERSION/$(VERSION_MAJOR)/'	\
	       -e '1,$$ s/REPLACE_ME_WITH_MINOR_VERSION/$(VERSION_MINOR)/'	\
	    < $< > $@

#--------------------------------------------------------------------------

noinst_HEADERS = fontforgeui.h sftextfieldP.h configure-fontforge.h	\
	views.h is_LIGATURE.h flaglist.h namelist_data.h
pkginclude_HEADERS = autowidth2.h configure-fontforge.h fontforge.h		\
	libffstamp.h psfont.h stemdb.h autowidth.h delta.h fontforgevw.h	\
	lookups.h savefont.h ttf.h baseviews.h fvmetrics.h mm.h				\
	scriptfuncs.h ttfinstrs.h edgelist2.h namehash.h scripting.h		\
	uiinterface.h bezctx_ff.h edgelist.h groups.h nonlineartrans.h		\
	sd.h unicoderange.h bitmapcontrol.h encoding.h ofl.h search.h		\
	usermenu.h fffreetype.h PfEd.h sfd1.h ffpython.h import.h			\
	plugins.h sflayoutP.h print.h splinefont.h annotations.h

#--------------------------------------------------------------------------

if THE_PROGRAMS

fontforge_SOURCES = main.c
fontforge_CPPFLAGS = $(AM_CPPFLAGS)
fontforge_LDADD = libfontforgeexe.la $(LDADD)
fontforge_LDFLAGS = $(MY_CFLAGS) $(MY_LIBS) -export-dynamic

endif THE_PROGRAMS

#--------------------------------------------------------------------------

libfontforge_la_SOURCES = asmfpst.c autohint.c autosave.c autotrace.c	\
	autowidth.c bezctx_ff.c bitmapchar.c bitmapcontrol.c bvedit.c		\
	clipnoui.c crctab.c cvexport.c cvimages.c cvundoes.c dumpbdf.c		\
	dumppfa.c effects.c encoding.c featurefile.c fontviewbase.c			\
	freetype.c fvcomposite.c fvfonts.c fvimportbdf.c fvmetrics.c		\
	glyphcomp.c http.c ikarus.c lookups.c macbinary.c macenc.c			\
	mathconstants.c mm.c namelist.c namelist_data.c nonlineartrans.c	\
	noprefs.c nouiutil.c nowakowskittfinstr.c ofl.c othersubrs.c		\
	palmfonts.c parsepdf.c parsepfa.c parsettfatt.c parsettfbmf.c		\
	parsettf.c parsettfvar.c plugins.c print.c psread.c pua.c			\
	python.c savefont.c scripting.c scstyles.c search.c sfd1.c sfd.c	\
	sflayout.c spiro.c splinechar.c splinefill.c splinefont.c			\
	splineorder2.c splineoverlap.c splinesaveafm.c splinesave.c			\
	splinestroke.c splineutil2.c splineutil.c start.c stemdb.c svg.c	\
	tottfaat.c tottfgpos.c tottf.c tottfvar.c ttfinstrs.c				\
	ttfspecial.c ufo.c unicoderange.c utils.c winfonts.c zapfnomen.c	\
	groups.c langfreq.c ftdelta.c autowidth2.c woff.c stamp.c			\
	activeinui.c pluginloading.c is_LIGATURE.c flaglist.c
nodist_libfontforge_la_SOURCES = libstamp.c
EXTRA_libfontforge_la_SOURCES = splinerefigure.c

PLUGIN_HOOKS = AddEncoding AddScriptingCommand

SYMBOLS_ASSOCIATED_WITH_OBSCURE_FEATURES = ClipBoardToSplineSet	\
	RealApprox RealNearish RefCharsCopy SFUntickAll				\
	SPLCatagorizePoints SplineSplit

SYMBOLS_FOR_FONTFORGE_WITHOUT_GUI = PyFF_Stdin

EXPORTED_LIBFONTFORGE_SYMBOLS = $(PLUGIN_HOOKS)							\
	$(SYMBOLS_ASSOCIATED_WITH_OBSCURE_FEATURES)							\
	$(SYMBOLS_FOR_FONTFORGE_WITHOUT_GUI) AddOTLToSllks AllGlyphNames	\
	AllNamelistNames AlreadyMSSymbolArea AltUniCopy AltUniFree			\
	AnchorClassDecompose AnchorClassMatch AnchorPointsCopy				\
	AnchorPointsFree ApTransform ApplyTickedFeatures					\
	ApproximateSplineFromPoints ApproximateSplineFromPointsSlopes		\
	AutoKern2 AutoKern2BuildClasses AutoKern2NewClass					\
	AutoSaveFrequency AutoWidth2 BCClearAll BCCompressBitmap			\
	BCCopyReference BCCopySelected BCDoRedo BCDoUndo BCExportXBM		\
	BCFlattenFloat BCMergeReferences BCPasteInto BCPreserveState		\
	BCRemoveDependent BCSetPoint BCTransFunc BDFCharFindBounds			\
	BDFCharFree BDFCharQuickBounds BDFDefaultProps BDFDepth				\
	BDFFloatCreate BDFFloatFree BDFFontFree BDFFoundry					\
	BDFGetMergedChar BDFMakeChar BDFMakeGID BDFOrigFixup BDFPieceMeal	\
	BDFPieceMealCheck BDFPropsFree BPTooFar BP_HVForce					\
	BackgroundImageTransform BaseFree BaseLangCopy BaseLangFree			\
	BdfPropHasInt BdfPropHasString BdfPropsCopy BitmapsDoIt				\
	BpColinear CID2Uni CIDFromName CIDLayerFindBounds CIDMasterAsDes	\
	CIDSetEncMap CI_Init CVCopyGridFit CVDoRedo CVDoUndo CVDoit			\
	CVEmbolden CVGenericChange CVLayer CVNLTrans CVPreserveState		\
	CVPreserveStateHints CVRemoveTopUndo CVYPerspective CanWoff			\
	CanonicalContours ChangeXHeight CharCenterHighest CheckIsScript		\
	CleanAutoRecovery ClipboardClear CompactEncMap CompareFonts			\
	CopyContainsBitmap CopyContainsRef CopyContainsSomething			\
	CopyReference CopySelected CopyUndoType CopyWidth CubicSolve		\
	CvtFindDeltas DStemInfoFree DStemInfosFree							\
	DefaultNameListForNewFonts Default_Properties Default_XLFD			\
	DeleteEncoding DeviceTableCopy DeviceTableFree DeviceTableSet		\
	DictionaryFree DoAutoRecovery DoAutoSaves DoPrinting DoRpl			\
	DumpPfaEditEncodings EncFromUni EncMap1to1 EncMapCopy EncMapFree	\
	EncMapFromEncoding EncMapNew EntryExitDecompose EvaluatePS			\
	ExecuteScriptFile ExportEPS ExportFig ExportGlif ExportImage		\
	ExportPDF ExportPlate ExportSVG FF_SetBCInterface					\
	FF_SetCVInterface FF_SetClipInterface FF_SetFIInterface				\
	FF_SetFVInterface FF_SetMVInterface FF_SetPrefsInterface			\
	FF_SetSCInterface FF_SetUiInterface FLMerge FLOrder					\
	FPSTClassesFree FPSTFree FPSTGlyphToClass FPSTRuleContentsFree		\
	FPSTRule_From_Str FPSTRule_To_Str FPSTRulesFree FVAddExtrema		\
	FVAddSmallCaps FVAddUnencoded FVAutoCounter FVAutoHint				\
	FVAutoHintSubs FVAutoInstr FVAutoTrace FVBParseSelectByPST			\
	FVBReplaceOutlineWithReference FVB_MakeNamelist FVBuildAccent		\
	FVBuildDuplicate FVCanonicalContours FVCanonicalStart FVClear		\
	FVClearBackground FVClearHints FVClearInstrs FVCluster FVCompact	\
	FVCondenseExtend FVCopy FVCopyFgtoBg FVCopyWidth FVCorrectDir		\
	FVCorrectReferences FVDetachAndRemoveGlyphs FVDetachGlyphs FVDoit	\
	FVDontAutoHint FVEmbolden FVGenericChange FVImportBDF				\
	FVImportImageTemplate FVImportImages FVImportMult FVInline			\
	FVInsertInCID FVJoin FVMetricsCenter FVOutline FVOverlap			\
	FVPointOfView FVRedo FVReencode FVRemoveKerns FVRemoveUnused		\
	FVRemoveVKerns FVRevert FVRevertBackup FVRevertGlyph FVRound2Int	\
	FVSameGlyphAs FVShadow FVStrokeItScript FVTrans FVTransFunc			\
	FVUndo FVUnlinkRef FVVKernFromHKern FeatDumpFontLookups				\
	FeatDumpOneLookup FeatureOrderId FeatureScriptLangListFree			\
	FeatureScriptTagInFeatureScriptList FeatureTagInFeatureScriptList	\
	FfPy_Replace_MenuItemStub FigureBaseOffsets FindAutoTraceName		\
	FindCidMap FindOrMakeEncoding FindUnicharName FlagsFromTuple		\
	FreeGlobalInstrCt FreeTypeAtLeast FreeTypeFontContext				\
	FreeTypeFreeContext FreeTypeStringVersion FreeType_FreeRaster		\
	FreeType_GetRaster FreeType_GridFitChar GetAuthor					\
	GetAutoTraceArgs GetFontNames GetNextUntitledName GlyphHashFree		\
	GlyphNameCnt GlyphVariantsCopy GlyphVariantsFree GradientCopy		\
	GradientFree GraveAcuteCenterBottom GroupCopy GroupFree				\
	GrowBufferAddStr GuessOpticalOffset HasLicense HasUseMyMetrics		\
	HttpGetBuf ImageListTransform ImageListsFree InitGlobalInstrCt		\
	InitSimpleStuff InitXHeightInfo InterpolateFont IsAnchorClassUsed	\
	JstfLangFree JstfLangsCopy JustifyFree KCFindName					\
	KernClassListFree KernPairsFree LIConvertToPrint					\
	LIConvertToSplines LI_BreakFontList LI_FDDrawChar LI_FindFontData	\
	LI_RegenFontData LI_SetFontData LI_TagsCopy LI_fontlistmergecheck	\
	LayerAllSplines LayerDefault LayerUnAllSplines LayersSimilar		\
	LayoutInfoInitLangSys LayoutInfoRefigureLines LayoutInfoReplace		\
	LayoutInfo_Destroy LoadGroupList LoadKerningDataFromMetricsFile		\
	LoadMapFromFile LoadNamelist LoadNamelistDir LoadPfaEditEncodings	\
	LoadPluginDir LoadSplineFont LookupInit LookupUsedNested MATHFree	\
	MMAxisUnmap MMBlendChar MMCreateBlendedFont MMKern MMMatchGlyphs	\
	MMNewFont MMReblend MMSetFreeContents MMValid MMWeightsUnMap		\
	MSLanguageFromLocale MVCopyChar MacEncFromMacLang MacFeatListFree	\
	MacNameCopy MacNameListFree MacSettingListFree MacStrToUtf8			\
	MacStyleCode MakeCIDMaster MakeItalic MarkClassFree MarkSetFree		\
	MathKernCopy MathKernFree MathTableNew MaxCID MergeDStemInfo		\
	MergeFont MfArgsInit NameListByName NameOTLookup NameToEncoding		\
	NearSpline NewAALTLookup NowakowskiSCAutoInstr OFLibUploadFont		\
	OS2FigureCodePages OS2FigureUnicodeRanges OTLookupsCopyInto			\
	OTTagToMacFeature OtfFeatNameListFree OtfNameListFree PI_Init		\
	PSDictChangeEntry PSDictFindEntry PSDictFree PSDictHasEntry			\
	PSTContains PSTFree ParseEncodingFile PasteIntoFV PasteIntoMV		\
	PasteToBC PasteToCV PathLength PatternCopy PatternFree				\
	PatternSCBounds PerGlyphDefBaseline PickNameFromMacName				\
	PointListIsSelected PointsDiagonalable PolygonIsConvex				\
	PreferSpacingAccents PrepareUnlinkRmOvrlp ProgramExists				\
	PrtBuildDef PyFF_FreeFV PyFF_Glyph_Set_Layer PyFF_InitFontHook		\
	PyFF_ProcessInitFiles PyFF_SCExport PyFF_SCImport					\
	PyFF_ScriptString PyFV_From_FV PySC_From_SC QuickBlues				\
	RandomParaFromScriptLang ReadOtherSubrsFile RealNear RealWithin		\
	RefCharCreate RefCharFindBounds RefCharFree RefCharsFree			\
	RemoveMultiples RestoreUnlinkRmOvrlp RevertedGlyphReferenceFixup	\
	SCAddScaleImage SCAutoTrace SCBuildComposit SCBuildDummy			\
	SCClearAll SCClearBackground SCClearContents SCClearHintMasks		\
	SCCondenseExtend SCCopyLayerToLayer SCCopyLookupData SCCopyWidth	\
	SCDependsOnSC SCDoRedo SCDoUndo SCDrawsSomething					\
	SCFigureCounterMasks SCFigureHintMasks SCFindDeltas					\
	SCGuessDHintInstances SCGuessHHintInstancesAndAdd					\
	SCGuessVHintInstancesAndAdd SCHintOverlapInMask SCImportFig			\
	SCImportGlif SCImportPDFFile SCImportPSFile SCImportPlateFile		\
	SCImportSVG SCMakeDependent SCModifyHintMasksAdd SCNumberPoints		\
	SCOrderAP SCPointsNumberedProperly SCPreserveBackground				\
	SCPreserveHints SCPreserveLayer SCPreserveState SCPreserveVWidth	\
	SCPreserveWidth SCRefToSplines SCReinstanciateRef					\
	SCReinstanciateRefChar SCRemoveDependent SCRightToLeft				\
	SCRound2Int SCRoundToCluster SCScriptFromUnicode					\
	SCSplinePointsUntick SCSynchronizeLBearing SCSynchronizeWidth		\
	SCTickValidationState SCUndoSetLBearingChange SCValidate			\
	SCValidateAnchors SCWorthOutputting SDDestroy SDFillup				\
	SFAddGlyphAndEncode SFAddLayer SFBaseSort SFClearAutoSave			\
	SFConvertGridToOrder2 SFConvertGridToOrder3							\
	SFConvertLayerToOrder2 SFConvertLayerToOrder3 SFDDumpMacFeat		\
	SFDParseMacFeatures SFDReadOneChar SFDWrite SFDWriteBak				\
	SFDefaultImage SFDefaultOS2Info SFDefaultOS2SubSuper				\
	SFEncodeToMap SFEncodingName SFFeaturesInScriptLang					\
	SFFindClearUnusedLookupBits SFFindExistingSlot SFFindGID			\
	SFFindLookup SFFindLookupSubtable SFFindNotdef SFFindSlot			\
	SFFindTable SFFindUnusedLookups SFFlatten SFFlattenByCMap			\
	SFForceEncoding SFGetAlternate SFGetChar SFGlyphRenameFixup			\
	SFGuessItalicAngle SFHasInstructions SFHashName						\
	SFIsCompositBuildable SFIsDuplicatable SFIsRotatable				\
	SFIsSomethingBuildable SFKernClassTempDecompose SFKernCleanup		\
	SFLangsInScript SFLayerSetBackground SFMakeChar SFMapFill			\
	SFPrivateGuess SFRandomChangeXUID SFReinstanciateRefs				\
	SFRemoveAnchorClass SFRemoveLayer SFRemoveLookup					\
	SFRemoveLookupSubTable SFRemoveUndoes SFRenameGlyphsToNamelist		\
	SFReplaceEncodingBDFProps SFReplaceFontnameBDFProps SFScaleToEm		\
	SFScriptLangs SFScriptsInLookups SFSerifHeight						\
	SFSetLayerWidthsStroked SFSetModTime SFSplineCharCreate				\
	SFSplinesFromLayers SFSubTablesMerge								\
	SFTemporaryRenameGlyphsToNamelist SFTemporaryRestoreGlyphNames		\
	SFUnicodeRanges SFValidate SPAdjustControl SPChangePointType		\
	SPHVCurveForce SPInterpolate SPLPoV SPLStartToLeftmost				\
	SSExistsInLayer SSRegenerateFromSpiros SSShadow SSttfApprox			\
	SVResetPaths SaveGroupList SaveTablesPref ScriptError				\
	ScriptFromUnicode ScriptInFeatureScriptList ScriptIsRightToLeft		\
	SearchChar SetAutoTraceArgs SfListFree SllkFree						\
	SmallCapsFindConstants SortInsertLookup SpIsExtremum				\
	Spline2DFindExtrema Spline2DFindPointsOfInflection SplineBisect		\
	SplineCharAddExtrema SplineCharAntiAlias SplineCharAutoHint			\
	SplineCharCopy SplineCharDefaultNextCP SplineCharDefaultPrevCP		\
	SplineCharFindBounds SplineCharFree SplineCharFreeContents			\
	SplineCharFreeTypeRasterize SplineCharFreeTypeRasterizeNoHints		\
	SplineCharIsFlexible SplineCharLayerFindBounds						\
	SplineCharLayerQuickBounds SplineCharMerge SplineCharQuickBounds	\
	SplineCharRasterize SplineCharSimplify SplineCharTangentNextCP		\
	SplineCharTangentPrevCP SplineCurvature SplineExistsInSS			\
	SplineFontBlank SplineFontFree SplineFontNew SplineFontPieceMeal	\
	SplineFontSetUnChanged SplineFree SplineLength SplineLengthRange	\
	SplineMake SplineMake2 SplineMake3 SplinePointCatagorize			\
	SplinePointCreate SplinePointFree SplinePointListCopy				\
	SplinePointListFree SplinePointListIsClockwise						\
	SplinePointListRemoveSelected SplinePointListSpiroTransform			\
	SplinePointListTransform SplinePointListsFree						\
	SplinePointNextCPChanged2 SplinePointPrevCPChanged2					\
	SplinePointRound SplineRefigure SplineRefigureFixup					\
	SplineSet2SpiroCP SplineSetAddExtrema SplineSetBindToPath			\
	SplineSetFindBounds SplineSetIntersect SplineSetJoin				\
	SplineSetJoinCpFixup SplineSetQuickBounds SplineSetRemoveOverlap	\
	SplineSetReverse SplineSetSpirosClear SplineSetStroke				\
	SplineSetsCorrect SplineSetsDetectDir SplineSetsRound2Int			\
	SplineSetsTTFApprox SplineT2SpiroIndex SplinesIntersect				\
	StandardProps StdFeaturesOfScript StdGlyphName StemInfoCopy			\
	StemInfoFree StemInfosFree StemListAnyConflicts SuffixFromTags		\
	TTFFoundry TTFLangNamesFree TTFPointMatches TTF_PSDupsDefault		\
	TagFullName TeXDefaultParams TopFindQuestionablePoints				\
	TransDStemHints TransHints TtfTablesFree URLToTempFile UndoesFree	\
	UniFromEnc UniFromName UnicodeRange UnitShape UnlinkThisReference	\
	UserFeaturesDiffer Utf8ToMacStr VSMaskFromFormat ValDevFree			\
	ValDevTabCopy ValidatePrivate VerticalKernFeature					\
	ViewPostScriptFont VrTrans WriteMacFamily WriteTTC					\
	XLFD_CreateComponents XLFD_GetComponents _BDFClut _CVMenuMakeLine	\
	_CVPreserveTState _CVRestoreTOriginalState _CVUndoCleanup			\
	_DoFindAll _DoSave _FVSimplify _FreeTypeFontContext _IVParse		\
	_MMNewFont _ReadSplineFont _SFNLTrans __FreeTypeFontContext			\
	__IVUnParseInstrs _uGetModifiers accent_offset adjustlbearing		\
	adjustwidth adobes_pua_alts allow_utf8_glyphnames amspua			\
	ask_user_for_cmap autohint_before_generate autotrace_ask			\
	bc_interface bdfcontrol_lastwhich bitmapextensions					\
	builtin_mac_feature_map check_library_version cidmaps				\
	clear_tt_instructions_when_needed compressors control_counters		\
	coord_sep copymetadata copyttfinstr coverageformatsallowed custom	\
	cv_interface default_background default_encoding					\
	default_fv_antialias default_fv_bbsized default_fv_col_count		\
	default_fv_font_size default_fv_row_count default_mac_feature_map	\
	detect_diagonal_stems doversion enclist export_clipboard			\
	ff_NextToken ff_VerboseCheck ff_backuptok ff_ft_context				\
	ff_statement ff_ttf_instrnames force_names_when_opening				\
	force_names_when_saving friendlies fv_active_in_ui fv_interface		\
	gdefclass getPfaEditDir group_root hasFreeType						\
	hasFreeTypeByteCode hasFreeTypeDebugger hascomposing haslrbounds	\
	hasspiro hint_bounding_boxes hint_diagonal_ends						\
	hint_diagonal_intersections instr_typify instrcheck					\
	instruct_ball_terminals instruct_diagonal_stems						\
	instruct_serif_stems interpolate_strong								\
	is_LIGATURE_or_VULGAR_FRACTION joinsnap knownweights				\
	layer_active_in_ui library_version_configuration					\
	loaded_fonts_same_as_new lookup_type_names macfeat_otftag			\
	math_constants_descriptor maxundoes memputshort memushort mf_args	\
	mf_ask mf_clearbackgrounds mf_showerrors mv_interface				\
	namelist_for_new_fonts names_db new_em_size new_fonts_are_order2	\
	nlt_exprfree nlt_parseexpr no_windowing_ui noticeweights			\
	ofl_str_lang_data old_ps_flags old_psotb_flags old_sfnt_flags		\
	oldbitmapstate oldformatstate onlycopydisplayed pageheight			\
	pagewidth pdefs prefer_cjk_encodings preferpotrace					\
	prefs_interface printcommand printlazyprinter printtype				\
	props_array py_ie realweights recognizePUA running_script			\
	savefont_extensions sc_active_in_ui sc_interface					\
	scriptsHaveDefault skewselect snaptoint source_modtime_str			\
	source_version_str stem_slope_error strconcat strconcat3			\
	stub_slope_error ttfFindPointInSC ttfFixupRef ui_interface			\
	unicoderange unicoderange_cnt use_freetype_to_rasterize_fv use_gv	\
	use_second_indic_scripts user_mac_feature_map user_macfeat_otftag	\
	xuid

libfontforge_la_CPPFLAGS = $(AM_CPPFLAGS)
libfontforge_la_LIBADD = splinerefigure.lo $(LIBADD)	\
	-dlopen $(top_builddir)/plugins/gb12345.la			\
	$(top_builddir)/lib/libgnu.la
libfontforge_la_LDFLAGS = $(call ldflags,EXPORTED_LIBFONTFORGE_SYMBOLS)

EXTRA_SCRIPTS = generate_codepoint_selector.py

# "make is_LIGATURE" or "make -C fontforge is_LIGATURE" to regenerate
# is_LIGATURE.[ch]. You need LibUnicodeNames with Python bindings, and
# you need cproto. (Using cproto is a bit safer than other methods,
# because it ensures the .h file is in sync with the .c file.)
is_LIGATURE:
	$(PYTHON) $(srcdir)/generate_codepoint_selector.py \
		'LIGATURE|VULGAR FRACTION' \
		is_LIGATURE_or_VULGAR_FRACTION > $(srcdir)/is_LIGATURE.c
	$(CPROTO) $(srcdir)/is_LIGATURE.c > $(srcdir)/is_LIGATURE.h
#	Add the '#include <config.h>' after the call to cproto, so we do
#	not need to pass preprocessor flags to cproto.
	$(SED) -i -e '1i\
\#include <config.h>\
' $(srcdir)/is_LIGATURE.c

#--------------------------------------------------------------------------

if GRAPHICAL_USER_INTERFACE

libfontforgeexe_la_SOURCES = alignment.c anchorsaway.c				\
	autowidth2dlg.c basedlg.c bdfinfo.c bitmapdlg.c bitmapview.c	\
	charinfo.c charview.c clipui.c combinations.c contextchain.c	\
	cursors.c cvaddpoints.c cvdebug.c cvdgloss.c cvexportdlg.c		\
	cvfreehand.c cvgetinfo.c cvgridfit.c cvhand.c cvhints.c			\
	cvimportdlg.c cvknife.c cvpalettes.c cvpointer.c cvruler.c		\
	cvshapes.c cvstroke.c cvtranstools.c displayfonts.c effectsui.c	\
	encodingui.c fontinfo.c fontview.c freetypeui.c fvfontsdlg.c	\
	fvmetricsdlg.c gotodlg.c groupsdlg.c histograms.c images.c		\
	kernclass.c layer2layer.c lookupui.c macencui.c math.c			\
	metricsview.c mmdlg.c nonlineartransui.c oflib.c openfontdlg.c	\
	prefs.c problems.c pythonui.c savefontdlg.c scriptingdlg.c		\
	scstylesui.c searchview.c sftextfield.c showatt.c simplifydlg.c	\
	splashimage.c startui.c statemachine.c tilepath.c transform.c	\
	ttfinstrsui.c uiutil.c windowmenu.c justifydlg.c deltaui.c		\
	usermenu.c

else !GRAPHICAL_USER_INTERFACE

libfontforgeexe_la_SOURCES = startnoui.c

endif !GRAPHICAL_USER_INTERFACE

nodist_libfontforgeexe_la_SOURCES = exelibstamp.c

EXPORTED_LIBFONTFORGEEXE_SYMBOLS = fontforge_main

libfontforgeexe_la_CPPFLAGS = $(AM_CPPFLAGS)
libfontforgeexe_la_LIBADD = libfontforge.la $(LTDLDEPS) $(LIBADD)	\
	$(GUILIBADD) $(top_builddir)/lib/libgnu.la
libfontforgeexe_la_LDFLAGS = $(call ldflags,EXPORTED_LIBFONTFORGEEXE_SYMBOLS)

#--------------------------------------------------------------------------

LIBADD = $(top_builddir)/Unicode/libgunicode.la	\
	$(top_builddir)/gutils/libgutils.la			\
	$(top_builddir)/gutils/libgioftp.la

GUILIBADD =
if GRAPHICAL_USER_INTERFACE
GUILIBADD += $(top_builddir)/gdraw/libgdraw.la $(XINPUT_LIBS)	\
	$(XKB_LIBS)
endif GRAPHICAL_USER_INTERFACE

LDADD = $(LIBADD) $(GUILIBADD)

#--------------------------------------------------------------------------

# In splinerefigure.c, the slight error introduced occasionally by
# optimization turns out to have disastrous effects. Compile it
# without optimization.
splinerefigure.lo: $(srcdir)/splinerefigure.c $(srcdir)/splinefont.h
	$(LIBTOOL) --mode=compile $(CC) -g -DHAVE_CONFIG_H -c -o		\
		splinerefigure.lo $(AM_CPPFLAGS) $(srcdir)/splinerefigure.c

#--------------------------------------------------------------------------

EXTRA_DIST = fontimage.pe fontlint.pe sfddiff.pe libstamp.pre	\
	exelibstamp.pre fontimage.1 fontlint.1 sfddiff.1			\
	MacFontForgeApp.zip darwinsetup.in
MOSTLYCLEANFILES = fontimage fontlint sfddiff libstamp.c	\
	exelibstamp.c

#--------------------------------------------------------------------------

if MACINTOSH

# FIXME: All of this needs testing on Mac OS X.

install-exec-local: FontForge.app/Contents/Info.plist
	$(SHELL) -x darwinsetup

FontForge.app/Contents/Info.plist: $(srcdir)/MacFontForgeApp.zip $(srcdir)/stamp.c clean-darwin-stuff
	$(UNZIP) $(srcdir)/MacFontForgeApp.zip
	mkdir -p darwinsetup_tmpdir
	cp FontForge.app/Contents/Info.plist darwinsetup_tmpdir
	( VERSION=`$(GREP) source_version_str $(srcdir)/stamp.c | $(SED) 's/const char \*source_version_str = "//' | $(SED) 's/";//'` ; $(SED) "s/20080810/$$VERSION/" < darwinsetup_tmpdir/Info.plist > FontForge.app/Contents/Info.plist )

mostlyclean-local: clean-darwin-stuff

clean-darwin-stuff:
	@if test -d darwinsetup_tmpdir; then rm -r -f darwinsetup_tmpdir;	\
		echo rm -r -f darwinsetup_tmpdir; else :; fi
	@if test -d FontForge.app; then rm -r -f FontForge.app;	\
		echo rm -r -f FontForge.app; else :; fi
	@if test -d __MACOSX; then rm -r -f __MACOSX;	\
		echo rm -r -f __MACOSX; else :; fi

endif MACINTOSH

#--------------------------------------------------------------------------
#
# FIXME: Get rid of the stamper.

#######################################################################
# The maintainer tools such as "stamper" are built if you configure   #
# with --enable-maintainer-tools                                      #
#######################################################################
if MAINTAINER_TOOLS

# You need to build the "fontforge" program to do the stamps for
# release.
if THE_PROGRAMS

#######################################################################
# Run "make stamps-for-release" with GNU Make before doing a release. #
#######################################################################
stamps-for-release: stamper $(LIBFFSTAMP_H_DEPENDS) $(STAMP_C_DEPENDS)
	$(builddir)/stamper$(EXEEXT) > $(srcdir)/stamp.c
	$(builddir)/stamper$(EXEEXT) -l > $(srcdir)/libffstamp.h

EXTRA_PROGRAMS = stamper
stamper_SOURCES = stamper.c
stamper_LDADD = $(top_builddir)/Unicode/libgunicode.la

LIBFFSTAMP_H_DEPENDS = \
	$(filter-out libfontforge_la-stamp.lo, \
		$(filter %.$(OBJEXT) %.lo, \
			$(addprefix libfontforge_la-, $(LIBFF_C_SRCFILES:.c=.lo))))
STAMP_C_DEPENDS = \
	$(LIBFFSTAMP_H_DEPENDS) \
	$(filter %.$(OBJEXT) %.lo, \
		$(addprefix fontforge-, $(FF_C_SRCFILES:.c=.$(OBJEXT))) \
		$(addprefix libfontforgeexe_la-, $(LIBFFEXE_C_SRCFILES:.c=.lo)) \
		$(EXTRA_LIBFF_C_SRCFILES:.c=.lo))

$(srcdir)/stamp.c: stamper $(STAMP_C_DEPENDS) 
$(srcdir)/libffstamp.h: stamper $(LIBFFSTAMP_H_DEPENDS) 

endif # THE_PROGRAMS
endif # MAINTAINER_TOOLS

#--------------------------------------------------------------------------
