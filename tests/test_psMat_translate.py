#! /bin/env python

import psMat
import random
import sys

eps = sys.float_info.epsilon

def test_translate (x, y):
    a = psMat.translate (x, y)
    print (x, y, a)

    if not isinstance (a, tuple):
        exit (10)
    if len (a) != 6:
        exit (20)
    for element in a:
        if not isinstance (element, float):
            exit (30)

    for i in range (0, 6):
        if eps < abs (a[i] - (1, 0, 0, 1, x, y)[i]):
            exit (50)

random.seed ()

for i in range (0, 1000):
    test_translate (random.uniform (-1000, 1000),
                    random.uniform (-1000, 1000))

exit (0)

