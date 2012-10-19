/* ANSI-C code produced by gperf version 3.0.4 */
/* Command-line: /usr/bin/gperf ../../auxiliary/sfd_read_keywords.gperf  */
/* Computed positions: -k'3-4,6-8,12' */

#if !((' ' == 32) && ('!' == 33) && ('"' == 34) && ('#' == 35) \
      && ('%' == 37) && ('&' == 38) && ('\'' == 39) && ('(' == 40) \
      && (')' == 41) && ('*' == 42) && ('+' == 43) && (',' == 44) \
      && ('-' == 45) && ('.' == 46) && ('/' == 47) && ('0' == 48) \
      && ('1' == 49) && ('2' == 50) && ('3' == 51) && ('4' == 52) \
      && ('5' == 53) && ('6' == 54) && ('7' == 55) && ('8' == 56) \
      && ('9' == 57) && (':' == 58) && (';' == 59) && ('<' == 60) \
      && ('=' == 61) && ('>' == 62) && ('?' == 63) && ('A' == 65) \
      && ('B' == 66) && ('C' == 67) && ('D' == 68) && ('E' == 69) \
      && ('F' == 70) && ('G' == 71) && ('H' == 72) && ('I' == 73) \
      && ('J' == 74) && ('K' == 75) && ('L' == 76) && ('M' == 77) \
      && ('N' == 78) && ('O' == 79) && ('P' == 80) && ('Q' == 81) \
      && ('R' == 82) && ('S' == 83) && ('T' == 84) && ('U' == 85) \
      && ('V' == 86) && ('W' == 87) && ('X' == 88) && ('Y' == 89) \
      && ('Z' == 90) && ('[' == 91) && ('\\' == 92) && (']' == 93) \
      && ('^' == 94) && ('_' == 95) && ('a' == 97) && ('b' == 98) \
      && ('c' == 99) && ('d' == 100) && ('e' == 101) && ('f' == 102) \
      && ('g' == 103) && ('h' == 104) && ('i' == 105) && ('j' == 106) \
      && ('k' == 107) && ('l' == 108) && ('m' == 109) && ('n' == 110) \
      && ('o' == 111) && ('p' == 112) && ('q' == 113) && ('r' == 114) \
      && ('s' == 115) && ('t' == 116) && ('u' == 117) && ('v' == 118) \
      && ('w' == 119) && ('x' == 120) && ('y' == 121) && ('z' == 122) \
      && ('{' == 123) && ('|' == 124) && ('}' == 125) && ('~' == 126))
/* The character set is not based on ISO-646.  */
#error "gperf generated tables don't work with this execution character set. Please report a bug to <bug-gnu-gperf@gnu.org>."
#endif

#line 3 "../../auxiliary/sfd_read_keywords.gperf"


/* *INDENT-ON* */

#include <config.h>

/* Copyright (C) 2012 by Barry Schwartz

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

#include <sfd_read.h>
#include <string.h>

/* *INDENT-OFF* */


#define TOTAL_KEYWORDS 194
#define MIN_WORD_LENGTH 4
#define MAX_WORD_LENGTH 25
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 651
/* maximum key range = 648, duplicates = 0 */

#ifndef GPERF_DOWNCASE
#define GPERF_DOWNCASE 1
static unsigned char gperf_downcase[256] =
  {
      0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,
     15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
     30,  31,  32,  33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,
     45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,
     60,  61,  62,  63,  64,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106,
    107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
    122,  91,  92,  93,  94,  95,  96,  97,  98,  99, 100, 101, 102, 103, 104,
    105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
    120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134,
    135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149,
    150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164,
    165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179,
    180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194,
    195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
    210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224,
    225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
    240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254,
    255
  };
#endif

#ifndef GPERF_CASE_STRNCMP
#define GPERF_CASE_STRNCMP 1
static int
gperf_case_strncmp (register const char *s1, register const char *s2, register unsigned int n)
{
  for (; n > 0;)
    {
      unsigned char c1 = gperf_downcase[(unsigned char)*s1++];
      unsigned char c2 = gperf_downcase[(unsigned char)*s2++];
      if (c1 != 0 && c1 == c2)
        {
          n--;
          continue;
        }
      return (int)c1 - (int)c2;
    }
  return 0;
}
#endif

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash (register const char *str, register unsigned int len)
{
  static const unsigned short asso_values[] =
    {
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      130,   5, 652, 652, 652, 652, 652, 652,   0, 225,
      652, 652, 652, 652, 652,   0,  65,   5,   5,  30,
        5,  95, 175,   0, 150, 185,  20,   0,  20,  15,
       40, 145, 120,  75,   0,  45, 165,  55, 150, 190,
        5, 652, 652, 652, 652,  20, 652,   0,  65,   5,
        5,  30,   5,  95, 175,   0, 150, 185,  20,   0,
       20,  15,  40, 145, 120,  75,   0,  45, 165,  55,
      150, 190,   5, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652, 652, 652, 652,
      652, 652, 652, 652, 652, 652, 652
    };
  register int hval = len;

  switch (hval)
    {
      default:
        hval += asso_values[(unsigned char)str[11]];
      /*FALLTHROUGH*/
      case 11:
      case 10:
      case 9:
      case 8:
        hval += asso_values[(unsigned char)str[7]+1];
      /*FALLTHROUGH*/
      case 7:
        hval += asso_values[(unsigned char)str[6]];
      /*FALLTHROUGH*/
      case 6:
        hval += asso_values[(unsigned char)str[5]];
      /*FALLTHROUGH*/
      case 5:
      case 4:
        hval += asso_values[(unsigned char)str[3]];
      /*FALLTHROUGH*/
      case 3:
        hval += asso_values[(unsigned char)str[2]];
        break;
    }
  return hval;
}

struct stringpool_t
  {
    char stringpool_str4[sizeof("DEI:")];
    char stringpool_str6[sizeof("Remap:")];
    char stringpool_str9[sizeof("Grid")];
    char stringpool_str10[sizeof("XUID:")];
    char stringpool_str11[sizeof("EndASM")];
    char stringpool_str15[sizeof("PfmFamily:")];
    char stringpool_str16[sizeof("MMCDV:")];
    char stringpool_str24[sizeof("MacIndic:")];
    char stringpool_str25[sizeof("MacIndic2:")];
    char stringpool_str27[sizeof("RemapN:")];
    char stringpool_str29[sizeof("Comments:")];
    char stringpool_str31[sizeof("MMNDV:")];
    char stringpool_str34[sizeof("FontName:")];
    char stringpool_str36[sizeof("EndTTInstrs")];
    char stringpool_str37[sizeof("OtfFeatName:")];
    char stringpool_str39[sizeof("FONDName:")];
    char stringpool_str42[sizeof("Ascent:")];
    char stringpool_str46[sizeof("MacContext:")];
    char stringpool_str47[sizeof("MacContext2:")];
    char stringpool_str49[sizeof("MMCounts:")];
    char stringpool_str50[sizeof("EndMMFonts")];
    char stringpool_str52[sizeof("ItalicAngle:")];
    char stringpool_str54[sizeof("FullName:")];
    char stringpool_str55[sizeof("DefaultBaseFilename:")];
    char stringpool_str56[sizeof("MultiLayer:")];
    char stringpool_str60[sizeof("Compacted:")];
    char stringpool_str64[sizeof("ChainPos:")];
    char stringpool_str65[sizeof("ChainPos2:")];
    char stringpool_str72[sizeof("Panose:")];
    char stringpool_str74[sizeof("TTFWidth:")];
    char stringpool_str75[sizeof("UFOAscent:")];
    char stringpool_str78[sizeof("HHeadDescent:")];
    char stringpool_str80[sizeof("woffMinor:")];
    char stringpool_str82[sizeof("DupEnc:")];
    char stringpool_str90[sizeof("UnderlineWidth:")];
    char stringpool_str92[sizeof("EndFPST")];
    char stringpool_str95[sizeof("AntiAlias:")];
    char stringpool_str96[sizeof("BitmapFont:")];
    char stringpool_str97[sizeof("OldEncoding:")];
    char stringpool_str102[sizeof("Weight:")];
    char stringpool_str103[sizeof("CreationTime:")];
    char stringpool_str104[sizeof("TtfTable:")];
    char stringpool_str110[sizeof("Base:")];
    char stringpool_str112[sizeof("ModificationTime:")];
    char stringpool_str116[sizeof("UFODescent:")];
    char stringpool_str118[sizeof("woffMetadata:")];
    char stringpool_str120[sizeof("UComments:")];
    char stringpool_str122[sizeof("HHeadAscent:")];
    char stringpool_str126[sizeof("Supplement:")];
    char stringpool_str127[sizeof("EndSplineSet")];
    char stringpool_str129[sizeof("LangName:")];
    char stringpool_str130[sizeof("MacSimple:")];
    char stringpool_str131[sizeof("MacSimple2:")];
    char stringpool_str132[sizeof("TopEncoding:")];
    char stringpool_str134[sizeof("ChainSub:")];
    char stringpool_str135[sizeof("ChainSub2:")];
    char stringpool_str136[sizeof("TileBounds:")];
    char stringpool_str137[sizeof("EndChar")];
    char stringpool_str138[sizeof("EndChars")];
    char stringpool_str139[sizeof("SplineSet")];
    char stringpool_str143[sizeof("ExtremaBound:")];
    char stringpool_str145[sizeof("EndMMSubroutine")];
    char stringpool_str146[sizeof("GridOrder2:")];
    char stringpool_str148[sizeof("EndSplineFont")];
    char stringpool_str150[sizeof("OS2FamilyClass:")];
    char stringpool_str151[sizeof("ShortTable:")];
    char stringpool_str153[sizeof("HHeadAOffset:")];
    char stringpool_str154[sizeof("EndMacFeatures")];
    char stringpool_str158[sizeof("HHeadDOffset:")];
    char stringpool_str159[sizeof("NameList:")];
    char stringpool_str160[sizeof("PfmWeight:")];
    char stringpool_str162[sizeof("Colour:")];
    char stringpool_str163[sizeof("UnderlinePosition:")];
    char stringpool_str165[sizeof("TTFWeight:")];
    char stringpool_str168[sizeof("EndBitmapFont")];
    char stringpool_str170[sizeof("KernClass:")];
    char stringpool_str171[sizeof("KernClass2:")];
    char stringpool_str172[sizeof("Order2:")];
    char stringpool_str173[sizeof("SplineFontDB:")];
    char stringpool_str174[sizeof("JstfPrio:")];
    char stringpool_str177[sizeof("MacLigature:")];
    char stringpool_str180[sizeof("MATH:")];
    char stringpool_str181[sizeof("ContextSub:")];
    char stringpool_str182[sizeof("ContextSub2:")];
    char stringpool_str189[sizeof("JstfMaxShrink:")];
    char stringpool_str190[sizeof("GaspTable:")];
    char stringpool_str193[sizeof("JstfDisableExtend:")];
    char stringpool_str194[sizeof("JstfMaxExtend:")];
    char stringpool_str195[sizeof("MacInsert:")];
    char stringpool_str196[sizeof("MacInsert2:")];
    char stringpool_str197[sizeof("JstfEnableExtend:")];
    char stringpool_str201[sizeof("EndSubFonts")];
    char stringpool_str209[sizeof("JstfLang:")];
    char stringpool_str210[sizeof("woffMajor:")];
    char stringpool_str214[sizeof("UniqueID:")];
    char stringpool_str216[sizeof("ScriptLang:")];
    char stringpool_str217[sizeof("MMPositions:")];
    char stringpool_str218[sizeof("OS2WinAscent:")];
    char stringpool_str221[sizeof("MMNamedInstance:")];
    char stringpool_str222[sizeof("JstfEnableShrink:")];
    char stringpool_str224[sizeof("Encoding:")];
    char stringpool_str226[sizeof("Layer:")];
    char stringpool_str227[sizeof("FSType:")];
    char stringpool_str229[sizeof("Registry:")];
    char stringpool_str231[sizeof("BeginSubrs:")];
    char stringpool_str232[sizeof("MMAxis:")];
    char stringpool_str234[sizeof("BeginSubFonts:")];
    char stringpool_str235[sizeof("VerticalOrigin:")];
    char stringpool_str237[sizeof("OS2UnicodeRanges:")];
    char stringpool_str238[sizeof("JstfDisableShrink:")];
    char stringpool_str239[sizeof("Ordering:")];
    char stringpool_str242[sizeof("OnlyBitmaps:")];
    char stringpool_str243[sizeof("MacFeat:")];
    char stringpool_str247[sizeof("Lookup:")];
    char stringpool_str249[sizeof("OS2WinDescent:")];
    char stringpool_str251[sizeof("VKernClass:")];
    char stringpool_str252[sizeof("VKernClass2:")];
    char stringpool_str253[sizeof("Comment:")];
    char stringpool_str260[sizeof("OS2TypoDescent:")];
    char stringpool_str261[sizeof("TableOrder:")];
    char stringpool_str263[sizeof("FitToEm:")];
    char stringpool_str267[sizeof("PickledData:")];
    char stringpool_str268[sizeof("EndShort")];
    char stringpool_str269[sizeof("VLineGap:")];
    char stringpool_str271[sizeof("WidthSeparation:")];
    char stringpool_str273[sizeof("WinInfo:")];
    char stringpool_str277[sizeof("StrokedFont:")];
    char stringpool_str278[sizeof("BeginMMFonts:")];
    char stringpool_str283[sizeof("TtTable:")];
    char stringpool_str284[sizeof("OS2TypoAscent:")];
    char stringpool_str285[sizeof("EndPrivate")];
    char stringpool_str286[sizeof("FamilyName:")];
    char stringpool_str288[sizeof("MacName:")];
    char stringpool_str289[sizeof("OS2WinAOffset:")];
    char stringpool_str293[sizeof("JstfExtender:")];
    char stringpool_str294[sizeof("OS2WinDOffset:")];
    char stringpool_str295[sizeof("OS2TypoLinegap:")];
    char stringpool_str300[sizeof("MMAxisMap:")];
    char stringpool_str304[sizeof("MacStyle:")];
    char stringpool_str305[sizeof("OS2TypoDOffset:")];
    char stringpool_str308[sizeof("MacLigature2:")];
    char stringpool_str309[sizeof("BaseVert:")];
    char stringpool_str317[sizeof("DisplaySize:")];
    char stringpool_str319[sizeof("OS2_UseTypoMetrics:")];
    char stringpool_str323[sizeof("LineGap:")];
    char stringpool_str324[sizeof("UnicodeInterp:")];
    char stringpool_str326[sizeof("ContextPos:")];
    char stringpool_str327[sizeof("ContextPos2:")];
    char stringpool_str331[sizeof("DesignSize:")];
    char stringpool_str333[sizeof("Descent:")];
    char stringpool_str335[sizeof("Copyright:")];
    char stringpool_str338[sizeof("ReverseChain:")];
    char stringpool_str339[sizeof("ReverseChain2:")];
    char stringpool_str340[sizeof("OS2TypoAOffset:")];
    char stringpool_str344[sizeof("MacKern2:")];
    char stringpool_str351[sizeof("BeginChars:")];
    char stringpool_str356[sizeof("TileMargin:")];
    char stringpool_str357[sizeof("HasVMetrics:")];
    char stringpool_str360[sizeof("OS2StrikeYSize:")];
    char stringpool_str363[sizeof("FontLog:")];
    char stringpool_str370[sizeof("OS2Vendor:")];
    char stringpool_str374[sizeof("OS2StrikeYPos:")];
    char stringpool_str375[sizeof("StartChar:")];
    char stringpool_str380[sizeof("OS2_WeightWidthSlopeOnly:")];
    char stringpool_str382[sizeof("StrokeWidth:")];
    char stringpool_str388[sizeof("TeXData:")];
    char stringpool_str390[sizeof("EndJustify")];
    char stringpool_str391[sizeof("BaseScript:")];
    char stringpool_str396[sizeof("NeedsXUIDChange:")];
    char stringpool_str397[sizeof("UseUniqueID:")];
    char stringpool_str398[sizeof("sfntRevision:")];
    char stringpool_str400[sizeof("BaseHoriz:")];
    char stringpool_str403[sizeof("OS2CodePages:")];
    char stringpool_str407[sizeof("OS2SupXSize:")];
    char stringpool_str408[sizeof("MarkAttachClasses:")];
    char stringpool_str410[sizeof("MMWeights:")];
    char stringpool_str411[sizeof("EndSubSplineFont")];
    char stringpool_str415[sizeof("MarkAttachSets:")];
    char stringpool_str416[sizeof("LayerCount:")];
    char stringpool_str418[sizeof("UseXUID:")];
    char stringpool_str432[sizeof("OS2SubXSize:")];
    char stringpool_str438[sizeof("DisplayLayer:")];
    char stringpool_str446[sizeof("OS2SupXOff:")];
    char stringpool_str447[sizeof("OS2SupYSize:")];
    char stringpool_str448[sizeof("BeginPrivate:")];
    char stringpool_str463[sizeof("Version:")];
    char stringpool_str471[sizeof("OS2SubXOff:")];
    char stringpool_str472[sizeof("OS2SubYSize:")];
    char stringpool_str486[sizeof("OS2SupYOff:")];
    char stringpool_str503[sizeof("Justify:")];
    char stringpool_str511[sizeof("OS2SubYOff:")];
    char stringpool_str526[sizeof("CIDVersion:")];
    char stringpool_str563[sizeof("MacKern:")];
    char stringpool_str651[sizeof("OS2Version:")];
  };
static const struct stringpool_t stringpool_contents =
  {
    "DEI:",
    "Remap:",
    "Grid",
    "XUID:",
    "EndASM",
    "PfmFamily:",
    "MMCDV:",
    "MacIndic:",
    "MacIndic2:",
    "RemapN:",
    "Comments:",
    "MMNDV:",
    "FontName:",
    "EndTTInstrs",
    "OtfFeatName:",
    "FONDName:",
    "Ascent:",
    "MacContext:",
    "MacContext2:",
    "MMCounts:",
    "EndMMFonts",
    "ItalicAngle:",
    "FullName:",
    "DefaultBaseFilename:",
    "MultiLayer:",
    "Compacted:",
    "ChainPos:",
    "ChainPos2:",
    "Panose:",
    "TTFWidth:",
    "UFOAscent:",
    "HHeadDescent:",
    "woffMinor:",
    "DupEnc:",
    "UnderlineWidth:",
    "EndFPST",
    "AntiAlias:",
    "BitmapFont:",
    "OldEncoding:",
    "Weight:",
    "CreationTime:",
    "TtfTable:",
    "Base:",
    "ModificationTime:",
    "UFODescent:",
    "woffMetadata:",
    "UComments:",
    "HHeadAscent:",
    "Supplement:",
    "EndSplineSet",
    "LangName:",
    "MacSimple:",
    "MacSimple2:",
    "TopEncoding:",
    "ChainSub:",
    "ChainSub2:",
    "TileBounds:",
    "EndChar",
    "EndChars",
    "SplineSet",
    "ExtremaBound:",
    "EndMMSubroutine",
    "GridOrder2:",
    "EndSplineFont",
    "OS2FamilyClass:",
    "ShortTable:",
    "HHeadAOffset:",
    "EndMacFeatures",
    "HHeadDOffset:",
    "NameList:",
    "PfmWeight:",
    "Colour:",
    "UnderlinePosition:",
    "TTFWeight:",
    "EndBitmapFont",
    "KernClass:",
    "KernClass2:",
    "Order2:",
    "SplineFontDB:",
    "JstfPrio:",
    "MacLigature:",
    "MATH:",
    "ContextSub:",
    "ContextSub2:",
    "JstfMaxShrink:",
    "GaspTable:",
    "JstfDisableExtend:",
    "JstfMaxExtend:",
    "MacInsert:",
    "MacInsert2:",
    "JstfEnableExtend:",
    "EndSubFonts",
    "JstfLang:",
    "woffMajor:",
    "UniqueID:",
    "ScriptLang:",
    "MMPositions:",
    "OS2WinAscent:",
    "MMNamedInstance:",
    "JstfEnableShrink:",
    "Encoding:",
    "Layer:",
    "FSType:",
    "Registry:",
    "BeginSubrs:",
    "MMAxis:",
    "BeginSubFonts:",
    "VerticalOrigin:",
    "OS2UnicodeRanges:",
    "JstfDisableShrink:",
    "Ordering:",
    "OnlyBitmaps:",
    "MacFeat:",
    "Lookup:",
    "OS2WinDescent:",
    "VKernClass:",
    "VKernClass2:",
    "Comment:",
    "OS2TypoDescent:",
    "TableOrder:",
    "FitToEm:",
    "PickledData:",
    "EndShort",
    "VLineGap:",
    "WidthSeparation:",
    "WinInfo:",
    "StrokedFont:",
    "BeginMMFonts:",
    "TtTable:",
    "OS2TypoAscent:",
    "EndPrivate",
    "FamilyName:",
    "MacName:",
    "OS2WinAOffset:",
    "JstfExtender:",
    "OS2WinDOffset:",
    "OS2TypoLinegap:",
    "MMAxisMap:",
    "MacStyle:",
    "OS2TypoDOffset:",
    "MacLigature2:",
    "BaseVert:",
    "DisplaySize:",
    "OS2_UseTypoMetrics:",
    "LineGap:",
    "UnicodeInterp:",
    "ContextPos:",
    "ContextPos2:",
    "DesignSize:",
    "Descent:",
    "Copyright:",
    "ReverseChain:",
    "ReverseChain2:",
    "OS2TypoAOffset:",
    "MacKern2:",
    "BeginChars:",
    "TileMargin:",
    "HasVMetrics:",
    "OS2StrikeYSize:",
    "FontLog:",
    "OS2Vendor:",
    "OS2StrikeYPos:",
    "StartChar:",
    "OS2_WeightWidthSlopeOnly:",
    "StrokeWidth:",
    "TeXData:",
    "EndJustify",
    "BaseScript:",
    "NeedsXUIDChange:",
    "UseUniqueID:",
    "sfntRevision:",
    "BaseHoriz:",
    "OS2CodePages:",
    "OS2SupXSize:",
    "MarkAttachClasses:",
    "MMWeights:",
    "EndSubSplineFont",
    "MarkAttachSets:",
    "LayerCount:",
    "UseXUID:",
    "OS2SubXSize:",
    "DisplayLayer:",
    "OS2SupXOff:",
    "OS2SupYSize:",
    "BeginPrivate:",
    "Version:",
    "OS2SubXOff:",
    "OS2SubYSize:",
    "OS2SupYOff:",
    "Justify:",
    "OS2SubYOff:",
    "CIDVersion:",
    "MacKern:",
    "OS2Version:"
  };
#define stringpool ((const char *) &stringpool_contents)
#ifdef __GNUC__
__inline
#if defined __GNUC_STDC_INLINE__ || defined __GNUC_GNU_INLINE__
__attribute__ ((__gnu_inline__))
#endif
#endif
const struct sfd_keyword *
sfd_keyword_lookup (register const char *str, register unsigned int len)
{
  static const struct sfd_keyword wordlist[] =
    {
      {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 98 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str4, "S"},
      {-1,""},
#line 221 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str6},
      {-1,""}, {-1,""},
#line 130 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str9},
#line 263 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str10, "S"},
#line 105 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str11},
      {-1,""}, {-1,""}, {-1,""},
#line 217 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str15},
#line 175 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str16},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""},
#line 158 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str24},
#line 159 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str25},
      {-1,""},
#line 222 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str27},
      {-1,""},
#line 89 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str29, "S"},
      {-1,""},
#line 178 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str31},
      {-1,""}, {-1,""},
#line 126 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str34, "S"},
      {-1,""},
#line 120 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str36},
#line 215 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str37},
      {-1,""},
#line 124 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str39},
      {-1,""}, {-1,""},
#line 71 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str42, "I"},
      {-1,""}, {-1,""}, {-1,""},
#line 155 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str46},
#line 156 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str47},
      {-1,""},
#line 176 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str49},
#line 112 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str50},
      {-1,""},
#line 137 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str52, "R"},
      {-1,""},
#line 128 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str54, "S"},
#line 97 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str55, "S"},
#line 182 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str56},
      {-1,""}, {-1,""}, {-1,""},
#line 90 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str60},
      {-1,""}, {-1,""}, {-1,""},
#line 82 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str64},
#line 83 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str65},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 216 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str72},
      {-1,""},
#line 241 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str74},
#line 244 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str75, "R"},
      {-1,""}, {-1,""},
#line 135 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str78, "I"},
      {-1,""},
#line 262 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str80},
      {-1,""},
#line 103 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str82},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""},
#line 247 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str90, "R"},
      {-1,""},
#line 109 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str92},
      {-1,""}, {-1,""},
#line 70 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str95, "I"},
#line 81 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str96},
#line 185 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str97},
      {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 257 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str102, "S"},
#line 96 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str103},
#line 239 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str104},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 72 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str110, "S"},
      {-1,""},
#line 181 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str112},
      {-1,""}, {-1,""}, {-1,""},
#line 245 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str116, "R"},
      {-1,""},
#line 261 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str118},
      {-1,""},
#line 243 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str120, "U"},
      {-1,""},
#line 134 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str122, "I"},
      {-1,""}, {-1,""}, {-1,""},
#line 233 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str126},
#line 117 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str127},
      {-1,""},
#line 150 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str129},
#line 167 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str130},
#line 168 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str131},
#line 238 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str132},
      {-1,""},
#line 84 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str134},
#line 85 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str135},
#line 236 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str136, "RRRR"},
#line 107 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str137},
#line 108 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str138},
#line 229 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str139},
      {-1,""}, {-1,""}, {-1,""},
#line 121 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str143},
      {-1,""},
#line 113 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str145},
#line 131 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str146},
      {-1,""},
#line 116 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str148},
      {-1,""},
#line 190 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str150},
#line 227 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str151},
      {-1,""},
#line 133 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str153, "I"},
#line 111 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str154},
      {-1,""}, {-1,""}, {-1,""},
#line 136 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str158, "I"},
#line 183 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str159},
#line 218 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str160},
      {-1,""},
#line 87 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str162, "X"},
#line 246 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str163, "R"},
      {-1,""},
#line 240 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str165},
      {-1,""}, {-1,""},
#line 106 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str168},
      {-1,""},
#line 148 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str170},
#line 149 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str171},
#line 187 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str172, "I"},
#line 228 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str173, "R"},
#line 146 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str174},
      {-1,""}, {-1,""},
#line 164 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str177},
      {-1,""}, {-1,""},
#line 172 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str180},
#line 93 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str181},
#line 94 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str182},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 145 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str189},
#line 129 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str190},
      {-1,""}, {-1,""},
#line 138 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str193},
#line 144 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str194},
#line 160 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str195},
#line 161 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str196},
#line 140 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str197},
      {-1,""}, {-1,""}, {-1,""},
#line 118 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str201},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""},
#line 143 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str209},
#line 260 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str210},
      {-1,""}, {-1,""}, {-1,""},
#line 249 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str214},
      {-1,""},
#line 225 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str216},
#line 179 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str217},
#line 212 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str218, "I"},
      {-1,""}, {-1,""},
#line 177 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str221},
#line 141 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str222},
      {-1,""},
#line 104 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str224, "S"},
      {-1,""},
#line 151 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str226, "="},
#line 127 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str227},
      {-1,""},
#line 220 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str229},
      {-1,""},
#line 80 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str231},
#line 173 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str232},
      {-1,""},
#line 79 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str234},
#line 253 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str235},
      {-1,""},
#line 206 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str237},
#line 139 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str238},
#line 188 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str239},
      {-1,""}, {-1,""},
#line 186 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str242, "I"},
#line 157 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str243},
      {-1,""}, {-1,""}, {-1,""},
#line 154 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str247},
      {-1,""},
#line 213 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str249, "I"},
      {-1,""},
#line 254 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str251},
#line 255 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str252},
#line 88 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str253, "U"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 203 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str260, "I"},
#line 234 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str261},
      {-1,""},
#line 123 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str263, "I"},
      {-1,""}, {-1,""}, {-1,""},
#line 219 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str267},
#line 115 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str268},
#line 256 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str269},
      {-1,""},
#line 258 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str271},
      {-1,""},
#line 259 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str273},
      {-1,""}, {-1,""}, {-1,""},
#line 231 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str277},
#line 77 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str278},
      {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 242 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str283},
#line 202 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str284, "I"},
#line 114 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str285},
#line 122 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str286, "S"},
      {-1,""},
#line 166 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str288},
#line 211 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str289, "I"},
      {-1,""}, {-1,""}, {-1,""},
#line 142 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str293},
#line 214 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str294, "I"},
#line 205 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str295, "I"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 174 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str300},
      {-1,""}, {-1,""}, {-1,""},
#line 169 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str304, "I"},
#line 204 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str305, "I"},
      {-1,""}, {-1,""},
#line 165 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str308},
#line 75 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str309, "#T"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""},
#line 102 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str317, "I"},
      {-1,""},
#line 207 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str319},
      {-1,""}, {-1,""}, {-1,""},
#line 153 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str323},
#line 248 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str324},
      {-1,""},
#line 91 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str326},
#line 92 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str327},
      {-1,""}, {-1,""}, {-1,""},
#line 100 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str331},
      {-1,""},
#line 99 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str333, "I"},
      {-1,""},
#line 95 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str335},
      {-1,""}, {-1,""},
#line 223 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str338},
#line 224 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str339},
#line 201 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str340, "I"},
      {-1,""}, {-1,""}, {-1,""},
#line 163 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str344},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 76 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str351},
      {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 237 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str356, "R"},
#line 132 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str357, "I"},
      {-1,""}, {-1,""},
#line 192 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str360, "I"},
      {-1,""}, {-1,""},
#line 125 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str363, "U"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 208 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str370},
      {-1,""}, {-1,""}, {-1,""},
#line 191 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str374, "I"},
#line 230 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str375},
      {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 210 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str380},
      {-1,""},
#line 232 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str382, "R"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 235 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str388},
      {-1,""},
#line 110 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str390},
#line 74 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str391, "="},
      {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 184 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str396},
#line 250 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str397, "I"},
#line 226 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str398},
      {-1,""},
#line 73 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str400, "#T"},
      {-1,""}, {-1,""},
#line 189 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str403},
      {-1,""}, {-1,""}, {-1,""},
#line 198 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str407, "I"},
#line 170 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str408},
      {-1,""},
#line 180 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str410},
#line 119 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str411},
      {-1,""}, {-1,""}, {-1,""},
#line 171 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str415},
#line 152 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str416, "I"},
      {-1,""},
#line 251 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str418, "S"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""},
#line 194 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str432, "I"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 101 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str438, "I"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""},
#line 197 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str446, "I"},
#line 200 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str447, "I"},
#line 78 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str448},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""},
#line 252 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str463, "S"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""},
#line 193 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str471, "I"},
#line 196 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str472, "I"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""},
#line 199 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str486, "I"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 147 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str503},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""},
#line 195 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str511, "I"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""},
#line 86 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str526, "R"},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
#line 162 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str563},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""}, {-1,""},
      {-1,""}, {-1,""}, {-1,""},
#line 209 "../../auxiliary/sfd_read_keywords.gperf"
      {(int)(long)&((struct stringpool_t *)0)->stringpool_str651}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register int o = wordlist[key].name;
          if (o >= 0)
            {
              register const char *s = o + stringpool;

              if ((((unsigned char)*str ^ (unsigned char)*s) & ~32) == 0 && !gperf_case_strncmp (str, s, len) && s[len] == '\0')
                return &wordlist[key];
            }
        }
    }
  return 0;
}
#line 264 "../../auxiliary/sfd_read_keywords.gperf"


/* *INDENT-ON* */

const char *
sfd_pool_string (int name)
{
  return (const char *) (stringpool + name);
}

/*-----------------------------------------------------------------------*/
/*
 * local variables:
 * mode: fundamental
 * c-file-style: "gnu"
 * end:
 */
