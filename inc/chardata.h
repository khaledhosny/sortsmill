#include <config.h>
#include <basics.h>

struct charmap {
    int first, last;
    unsigned char **table;
    uint32_t *totable;
};
struct charmap2 {
    int first, last;
    unsigned short **table;
    uint32_t *totable;
};

VISIBLE extern const uint32_t unicode_from_jis201[];
VISIBLE extern const uint32_t unicode_from_win[];
VISIBLE extern const uint32_t unicode_from_mac[];
VISIBLE extern struct charmap mac_from_unicode;

VISIBLE extern const uint32_t unicode_from_jis208[];
/* Subtract 0x8100 before indexing this array */
VISIBLE extern const uint32_t unicode_from_big5hkscs[];
VISIBLE extern const uint32_t unicode_from_ksc5601[];
/* Subtract 0x8400 before indexing this array */
VISIBLE extern const uint32_t unicode_from_johab[];
VISIBLE extern const uint32_t unicode_from_gb2312[];

/* a mask for each character saying what charset(s) it may be found in */
VISIBLE extern const unsigned long * const unicode_backtrans[];

VISIBLE extern const uint32_t *const * const unicode_alternates[];
