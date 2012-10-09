#! /bin/env python

import psMat
import sys

eps = sys.float_info.epsilon

a = psMat.identity ()

if not isinstance (a, tuple):
    exit (10)
if len (a) != 6:
    exit (20)
for x in a:
    if not isinstance (x, float):
        exit (30)
for i in range (0, 6):
    if eps < abs (a[i] - (1, 0, 0, 1, 0, 0)[i]):
        exit (40)

exit (0)

