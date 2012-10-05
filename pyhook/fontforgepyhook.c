#include <config.h>

#include <Python.h>
#include <splinefont.h>

VISIBLE PyMODINIT_FUNC initfontforge(void);

PyMODINIT_FUNC initfontforge(void) {
    ff_init();
}
