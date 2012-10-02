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
-T unichar_t

-T real
-T AnchorPoint
-T BDFFont
-T GImage
-T GResStruct
-T MMSet
-T SplineChar
-T SplineFont
-T KernPair
-T KernPair1

// Internal to sfd.c. FIXME: Get rid of this type.
-T __longlong
