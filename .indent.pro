// -*- fundamental -*-
//
// .indent.pro file for GNU Indent.

--gnu-style
--no-tabs

-T FILE
-T int8_t
-T int16_t
-T int32_t
-T uint8_t
-T uint16_t
-T uint32_t
-T intptr_t
-T uintptr_t
-T SCM
-T pthread_mutex_t
-T cairo_t
-T regoff_t
-T pcre
-T AO_t
-T Hash_table
-T mpz_t
-T mpq_t
-T mpf_t

-T rexp_buffer_t
-T rexp_t
-T rexp_match_buffer_t
-T rexp_match_t
-T rexp_interval_t
-T precomputed_data_by_degree_t

-T real
-T bigreal
-T extended
-T AnchorPoint
-T BDFFont
-T Encoding
-T GEvent
-T GGadget
-T GImage
-T GResStruct
-T MMSet
-T DBounds
-T SplineChar
-T SplineFont
-T Spline1
-T Spline1D
-T KernPair
-T KernPair1

-T PyFF_Font
-T PyFF_Glyph
-T PyObject

// Internal to sfd.c. FIXME: Get rid of this type.
-T __longlong
