#include "splinefont.h"

struct str_lang_data {
    enum ttfnames strid;
    int lang;
    char **data;
};

VISIBLE extern struct str_lang_data ofl_str_lang_data[];
