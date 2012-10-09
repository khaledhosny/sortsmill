#! /bin/env python

import psMat
from math import cos, sin, pi
import sys

eps = sys.float_info.epsilon

def test_angle (theta):

    a = psMat.rotate (theta)
    print (theta, a)

    if not isinstance (a, tuple):
        exit (10)
    if len (a) != 6:
        exit (20)
    for x in a:
        if not isinstance (x, float):
            exit (30)
    if theta == 0:
        for i in range (0, 6):
            if eps < abs (a[i] - (1, 0, 0, 1, 0, 0)[i]):
                exit (40)
    if abs (theta - pi / 2) <= eps:
        for i in range (0, 6):
            if eps < abs (a[i] - (0, 1, -1, 0, 0, 0)[i]):
                exit (42)
    if abs (theta - pi) <= eps:
        for i in range (0, 6):
            if eps < abs (a[i] - (-1, 0, 0, -1, 0, 0)[i]):
                exit (44)
    if abs (theta - 3 * pi / 2) <= eps:
        for i in range (0, 6):
            if eps < abs (a[i] - (0, -1, 1, 0, 0, 0)[i]):
                exit (46)
    for i in range (0, 6):
        if eps < abs (a[i] - (cos (theta), sin (theta),
                              - sin (theta), cos (theta),
                              0, 0)[i]):
            exit (50)

test_angle (0)
test_angle (pi / 2)
test_angle (pi)
test_angle (3 * pi / 2)

for i in range (0, 3600):
    test_angle (i * pi / 1800)

exit (0)

