#! /bin/env python

import sortsmillff.psMat as psMat
import random
import sys

eps = sys.float_info.epsilon

def matmul (m1, m2):
    return [m1[0] * m2[0] + m1[1] * m2[2],
            m1[0] * m2[1] + m1[1] * m2[3],
            m1[2] * m2[0] + m1[3] * m2[2],
            m1[2] * m2[1] + m1[3] * m2[3]]

def vecmul (v, m):
    return [v[0] * m[0] + v[1] * m[2],
            v[0] * m[1] + v[1] * m[3]]

def test_compose (a1, a2):
    a = psMat.compose (a1, a2)
    print (a1, a2, a)

    if not isinstance (a, tuple):
        exit (10)
    if len (a) != 6:
        exit (20)
    for element in a:
        if not isinstance (element, float):
            exit (30)

    for i in range (0, 6):
        m = matmul (a1[0:4], a2[0:4])
        v = vecmul (a1[4:6], a2[0:4])
        v[0] += a2[4]
        v[1] += a2[5]
        mm = m + v
        if 1e6 * eps < abs (a[i] - mm[i]):
            exit (50)

random.seed ()

for i in range (0, 1000):
    a1 = (random.uniform (-1000, 1000), random.uniform (-1000, 1000),
          random.uniform (-1000, 1000), random.uniform (-1000, 1000),
          random.uniform (-1000, 1000), random.uniform (-1000, 1000))
    a2 = (random.uniform (-1000, 1000), random.uniform (-1000, 1000),
          random.uniform (-1000, 1000), random.uniform (-1000, 1000),
          random.uniform (-1000, 1000), random.uniform (-1000, 1000))
    test_compose (a1, a2)

for i in range (0, 1000):
    a1 = (random.randrange (-1000, 1001), random.randrange (-1000, 1001),
          random.randrange (-1000, 1001), random.randrange (-1000, 1001),
          random.randrange (-1000, 1001), random.randrange (-1000, 1001))
    a2 = (random.randrange (-1000, 1001), random.randrange (-1000, 1001),
          random.randrange (-1000, 1001), random.randrange (-1000, 1001),
          random.randrange (-1000, 1001), random.randrange (-1000, 1001))
    test_compose (a1, a2)

exit (0)

