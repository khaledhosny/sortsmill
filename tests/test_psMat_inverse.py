#! /bin/env python

# FIXME: There is a risk of failure on this test, as described below.
#
# NOTE: It is conceivable that this test could fail even if the
# programming is correct, if the random number generator happens to
# create some very ill-conditioned matrix.

import psMat
import random
import sys

eps = sys.float_info.epsilon

def matmul (m1, m2):
    return [m1[0] * m2[0] + m1[1] * m2[2],
            m1[0] * m2[1] + m1[1] * m2[3],
            m1[2] * m2[0] + m1[3] * m2[2],
            m1[2] * m2[1] + m1[3] * m2[3]]

def test_inverse (a1):
    try:
        a = psMat.inverse (a1)
        print (a1, a)

        if not isinstance (a, tuple):
            exit (10)
        if len (a) != 6:
            exit (20)
        for element in a:
            if not isinstance (element, float):
                exit (30)

        ident_mat = matmul (a1[0:4], a[0:4])
        print (ident_mat)
        for i in range (0, 4):
            if 1e-10 < abs (ident_mat[i] - (1, 0, 0, 1)[i]):
                exit (40)

        ident_mat2 = psMat.compose (a1, a)
        print (ident_mat2)
        for i in range (0, 6):
            if 1e-10 < abs (ident_mat2[i] - (1, 0, 0, 1, 0, 0)[i]):
                exit (45)

    except:
        # If the inversion threw an exception, check that that the
        # determinant is vanishing.
        if 1e6 * eps < abs (a1[0] * a1[3] - a1[1] * a1[2]):
            exit (100)

random.seed ()

test_inverse ((0, 0, 0, 0, 0, 0))         # singular
test_inverse ((1, 2, 1, 2, 1, 2))         # singular

for i in range (0, 1000):
    a1 = (random.uniform (-1000, 1000), random.uniform (-1000, 1000),
          random.uniform (-1000, 1000), random.uniform (-1000, 1000),
          random.uniform (-1000, 1000), random.uniform (-1000, 1000))
    test_inverse (a1)

for i in range (0, 1000):
    a1 = (random.randrange (-1000, 1001), random.randrange (-1000, 1001),
          random.randrange (-1000, 1001), random.randrange (-1000, 1001),
          random.randrange (-1000, 1001), random.randrange (-1000, 1001))
    test_inverse (a1)

exit (0)

