#include <ggadget.h>
#include <sortsmillff/usermenu.h>

extern SplineChar *sc_active_in_ui;
extern FontViewBase *fv_active_in_ui;
extern int layer_active_in_ui;

typedef struct flaglist
{
  char *name;
  int flag;
} FlagList;

void cv_tl2listcheck (GWindow gw, struct gmenuitem *mi, GEvent *e);
void fv_tl2listcheck (GWindow gw, struct gmenuitem *mi, GEvent *e);
