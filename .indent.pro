// -*- fundamental -*-
//
// .indent.pro file for GNU Indent.

--gnu-style
--no-tabs
--line-length80

-T FILE

-T _Bool
-T bool

-T int8_t
-T int16_t
-T int32_t
-T int64_t
-T int128_t
-T intptr_t
-T intmax_t
-T ssize_t

-T uint8_t
-T uint16_t
-T uint32_t
-T uint64_t
-T uint128_t
-T uintptr_t
-T uintmax_t
-T size_t

-T SCM
-T scm_t_array_handle
-T scm_t_array_dim
-T pthread_mutex_t
-T cairo_t
-T regoff_t
-T pcre
-T AO_t
-T Hash_table
-T mpz_t
-T mpq_t
-T mpf_t
-T __mpz_struct
-T __mpq_struct
-T __mpf_struct
-T gsl_vector
-T gsl_matrix
-T gsl_permutation

-T rexp_buffer_t
-T rexp_t
-T rexp_match_buffer_t
-T rexp_match_t
-T rexp_interval_t
-T precomputed_data_by_degree_t
-T mpqmat_t
-T mpqmat_struct_t
-T _precomputed_matrices_t

-T real
-T bigreal
-T extended
-T menu_info
-T spiro_cp
-T spiro_seg
-T bezctx
-T bandmat
-T ui_interface_t
-T ImageList
-T ASM
-T AnchorPoint
-T BasePoint
-T BDFChar
-T BDFFont
-T DebugView
-T BitmapView
-T CharView
-T CharViewBase
-T Encoding
-T FontView
-T FontViewBase
-T DeviceTable
-T DRect
-T BVTFunc
-T FontRequest
-T GFont
-T GTextInfo
-T GPoint
-T IPoint
-T GEvent
-T GGadget
-T GGadgetCreateData
-T GImage
-T GMenuItem
-T GMenuItem2
-T GResStruct
-T GTextBounds
-T GWindow
-T GWindowAttrs
-T GDisplay
-T GRect
-T GBox
-T MacFeat
-T MenuInfo
-T MetricsView
-T MMSet
-T DBounds
-T SplineChar
-T SplineFont
-T SplineSet
-T SplinePointList
-T SplinePoint
-T Spline
-T Spline1
-T Spline1D
-T KernPair
-T KernPair1
-T Val
-T Array
-T Context
-T RefChar
-T FeatureScriptLangList
-T PST
-T FPST
-T KernClass
-T OTLookup
-T Layer
-T EncMap
-T Undoes
-T FindSel
-T SearchData
-T SearchView
-T MathKernDlg
-T GradientDlg
-T StrokeDlg
-T TilePathDlg
-T StrokeInfo
-T PressedOn

-T PyFF_AWContext
-T PyFF_Font
-T PyFF_Glyph
-T PyObject

-T GOptionContext

// Internal to sfd.c. FIXME: Get rid of this type.
-T __longlong
