#ifndef _ENCODING_H
#define _ENCODING_H

struct cidaltuni {
    struct cidaltuni *next;
    int uni;
    int cid;
};

struct cidmap {
    char *registry, *ordering;
    int supplement, maxsupple;
    int cidmax;			/* Max cid found in the charset */
    int namemax;		/* Max cid with useful info */
    uint32_t *unicode;
    char **name;
    struct cidaltuni *alts;
    struct cidmap *next;
};

VISIBLE extern struct cidmap *cidmaps;

VISIBLE extern void DeleteEncoding(Encoding *me);
VISIBLE extern void RemoveMultiples(Encoding *item);
#endif
