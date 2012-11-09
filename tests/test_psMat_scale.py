#! /bin/env python

import sortsmillff.psMat as psMat
import random
import sys

eps = sys.float_info.epsilon

def test_scale (x, y = None):

    if y is None:
        a = psMat.scale (x)
    else:
        a = psMat.scale (x, y)
    print (x, y, a)

    if not isinstance (a, tuple):
        exit (10)
    if len (a) != 6:
        exit (20)
    for element in a:
        if not isinstance (element, float):
            exit (30)

    if y is None:
        xx = x
        yy = x
    else:
        xx = x
        yy = y

    for i in range (0, 6):
        if 1000 * eps < abs (a[i] - (xx, 0, 0, yy, 0, 0)[i]):
            exit (50)

random.seed ()

for i in range (0, 1000):
    test_scale (random.uniform (-1000, 1000))

for i in range (0, 1000):
    test_scale (random.uniform (-1000, 1000),
                random.uniform (-1000, 1000))

for i in range (0, 1000):
    test_scale (random.randrange (-1000, 1001))

for i in range (0, 1000):
    test_scale (random.randrange (-1000, 1001),
                random.randrange (-1000, 1001))

exit (0)

