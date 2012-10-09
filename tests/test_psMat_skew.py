#! /bin/env python

import psMat
from math import tan, pi
import sys

eps = sys.float_info.epsilon

def test_angle (theta):

    a = psMat.skew (theta)
    print (theta, a)

    if not isinstance (a, tuple):
        exit (10)
    if len (a) != 6:
        exit (20)
    for element in a:
        if not isinstance (element, float):
            exit (30)
    if theta == 0:
        for i in range (0, 6):
            if eps < abs (a[i] - (1, 0, 0, 1, 0, 0)[i]):
                exit (40)
    if abs (theta - pi / 4) <= eps:
        for i in range (0, 6):
            if eps < abs (a[i] - (1, 0, 1, 1, 0, 0)[i]):
                exit (42)
    if abs (theta + pi / 4) <= eps:
        for i in range (0, 6):
            if eps < abs (a[i] - (1, 0, -1, 1, 0, 0)[i]):
                exit (42)
    if abs (theta - pi) <= eps:
        for i in range (0, 6):
            if eps < abs (a[i] - (1, 0, 0, 1, 0, 0)[i]):
                exit (44)
    for i in range (0, 6):
        if eps < abs (a[i] - (1, 0, tan (theta), 1, 0, 0)[i]):
            exit (50)

test_angle (0)
test_angle (pi / 4)
test_angle (- pi / 4)
test_angle (pi)

for i in range (-890, 900):
    test_angle (i * pi / 1800)
    test_angle ((i + 1800) * pi / 1800)

for i in range (-1000, 1001):
    test_angle (i)

exit (0)

