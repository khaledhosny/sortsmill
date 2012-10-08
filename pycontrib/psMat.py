# -*- coding: utf-8 -*-

# Copyright (C) 2012 by Barry Schwartz
# Based in part on python.c by George Williams, which is
#   Copyright (C) 2007-2012 by George Williams
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# The name of the author may not be used to endorse or promote products
# derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

from math import cos, sin, tan

try:
    import numpy
    import numpy.linalg

    # Use a high-quality, already written matrix inversion algorithm
    # if one is available.
    def psMat_invert (a):
        a = map (float, a)
        m = numpy.array ([a[0:2], a[2:4]])
        minv = numpy.linalg.inv (m)
        b11 = minv[0][0]
        b12 = minv[0][1]
        b21 = minv[1][0]
        b22 = minv[1][1]
        c1 = - a[4] * b11 - a[5] * b21;
        c2 = - a[4] * b12 - a[5] * b22;
        return (b11, b12, b21, b22, c1, c2)

except ImportError:

    # Fall back to the easy but numerically awful Cramer’s method,
    # which is what FontForge originally used, anyway. (Here we will
    # let singular matrices throw division-by-zero or overflow
    # exceptions, which is different from what FontForge did.)
    def psMat_invert (a):
        a = map (float, a)
        det = a[0] * a[3] - a[1] * a[2]
        b11 = a[3] / det
        b12 = - a[1] / det
        b21 = - a[2] / det
        b22 = a[0] / det
        c1 = - a[4] * b11 - a[5] * b21;
        c2 = - a[4] * b12 - a[5] * b22;
        return (b11, b12, b21, b22, c1, c2)

# f--
# f-- Returns an identity matrix as a 6-element tuple.
# f--
def identity ():
    return (1.0, 0.0, 0.0, 1.0, 0.0, 0.0)

# f--
# f-- Returns a matrix that is the composition of two input
# f-- transformations.
# f--
def compose (mat1, mat2):
    mat1 = map (float, mat1)
    mat2 = map (float, mat2)
    a0 = mat1[0] * mat2[0] + mat1[1] * mat2[2]
    a1 = mat1[0] * mat2[1] + mat1[1] * mat2[3]
    a2 = mat1[2] * mat2[0] + mat1[3] * mat2[2]
    a3 = mat1[2] * mat2[1] + mat1[3] * mat2[3]
    a4 = mat1[4] * mat2[0] + mat1[5] * mat2[2] + mat2[4];
    a5 = mat1[4] * mat2[1] + mat1[5] * mat2[3] + mat2[5];
    return (a0, a1, a2, a3, a4, a5)

# f--
# f-- Returns a matrix which is the inverse of the input
# f-- transformation.
# f-- (Note: There will not always be an inverse.)
# f--
def inverse (mat):
    return psMat_invert (mat)

# f--
# f-- Returns a matrix which will rotate by theta.
# f-- Theta is expressed in radians.
# f--
def rotate (theta):
    t = float (theta)
    cosine = cos (t)
    sine = sin (t)
    return (cosine, sine, -sine, cosine, 0.0, 0.0)

# f--
# f-- Returns a matrix that will scale by x horizontally
# f-- and y vertically.
# f-- If y is omitted, the matrix will scale by x in both
# f-- directions.
# f--
def scale (x, y = None):
    if y is None:
        y = x
    return (float (x), 0.0, 0.0, float (y), 0.0, 0.0)

# f--
# f-- Returns a matrix that will skew by theta
# f-- (to produce a oblique font). Theta is expressed in radians.
# f--
def skew (theta):
    return (1.0, 0.0, tan (float (theta)), 1.0, 0.0, 0.0)

# f--
# f-- Returns a matrix that will translate by x horizontally
# f-- and y vertically.
# f--
def translate (x, y):
    return (1.0, 0.0, 0.0, 1.0, float (x), float (y))
