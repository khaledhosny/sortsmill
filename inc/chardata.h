#include <config.h>
#include <basics.h>

struct charmap {
    int first, last;
    unsigned char **table;
    unichar_t *totable;
};
struct charmap2 {
    int first, last;
    unsigned short **table;
    unichar_t *totable;
};

VISIBLE extern const unichar_t unicode_from_i8859_1[];
VISIBLE extern struct charmap i8859_1_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_2[];
VISIBLE extern struct charmap i8859_2_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_3[];
VISIBLE extern struct charmap i8859_3_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_4[];
VISIBLE extern struct charmap i8859_4_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_5[];
VISIBLE extern struct charmap i8859_5_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_6[];
VISIBLE extern struct charmap i8859_6_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_7[];
VISIBLE extern struct charmap i8859_7_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_8[];
VISIBLE extern struct charmap i8859_8_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_9[];
VISIBLE extern struct charmap i8859_9_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_10[];
VISIBLE extern struct charmap i8859_10_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_11[];
VISIBLE extern struct charmap i8859_11_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_13[];
VISIBLE extern struct charmap i8859_13_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_14[];
VISIBLE extern struct charmap i8859_14_from_unicode;
VISIBLE extern const unichar_t unicode_from_i8859_15[];
VISIBLE extern struct charmap i8859_15_from_unicode;
VISIBLE extern const unichar_t unicode_from_koi8_r[];
VISIBLE extern struct charmap koi8_r_from_unicode;
VISIBLE extern const unichar_t unicode_from_jis201[];
VISIBLE extern struct charmap jis201_from_unicode;
VISIBLE extern const unichar_t unicode_from_win[];
VISIBLE extern struct charmap win_from_unicode;
VISIBLE extern const unichar_t unicode_from_mac[];
VISIBLE extern struct charmap mac_from_unicode;
VISIBLE extern const unichar_t unicode_from_MacSymbol[];
VISIBLE extern struct charmap MacSymbol_from_unicode;
VISIBLE extern const unichar_t unicode_from_ZapfDingbats[];
VISIBLE extern struct charmap ZapfDingbats_from_unicode;

VISIBLE extern unichar_t *unicode_from_alphabets[];
VISIBLE extern struct charmap *alphabets_from_unicode[];

VISIBLE extern const unichar_t unicode_from_jis208[];
VISIBLE extern const unichar_t unicode_from_jis212[];
VISIBLE extern struct charmap2 jis_from_unicode;
/* Subtract 0xa100 before indexing this array */
VISIBLE extern const unichar_t unicode_from_big5[];
VISIBLE extern struct charmap2 big5_from_unicode;
/* Subtract 0x8100 before indexing this array */
VISIBLE extern const unichar_t unicode_from_big5hkscs[];
VISIBLE extern struct charmap2 big5hkscs_from_unicode;
VISIBLE extern const unichar_t unicode_from_ksc5601[];
VISIBLE extern struct charmap2 ksc5601_from_unicode;
/* Subtract 0x8400 before indexing this array */
VISIBLE extern const unichar_t unicode_from_johab[];
VISIBLE extern struct charmap2 johab_from_unicode;
VISIBLE extern const unichar_t unicode_from_gb2312[];
VISIBLE extern struct charmap2 gb2312_from_unicode;

/* a mask for each character saying what charset(s) it may be found in */
VISIBLE extern const unsigned long * const unicode_backtrans[];

VISIBLE extern const unichar_t *const * const unicode_alternates[];
