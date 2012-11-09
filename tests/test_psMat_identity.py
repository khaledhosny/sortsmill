#! /bin/env python

import sortsmillff.psMat as psMat
import sys

eps = sys.float_info.epsilon

a = psMat.identity ()
print (a)

if not isinstance (a, tuple):
    exit (10)
if len (a) != 6:
    exit (20)
for element in a:
    if not isinstance (element, float):
        exit (30)
for i in range (0, 6):
    if eps < abs (a[i] - (1, 0, 0, 1, 0, 0)[i]):
        exit (40)

exit (0)

