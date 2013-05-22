# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
# This file is part of the Sorts Mill Tools.
# 
# Sorts Mill Tools is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# Sorts Mill Tools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

# Copyright (C) 2007-2012 by George Williams
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

"""Transformations on PostScript matrices.

The matrices are expressed as six-element tuples of floats.

"""

import gmpy
import math
import sys
from . import (pkg_info)

__version__ = pkg_info.version

# i--
# i-- @strong{FIXME:} This chapter needs an introductory section,
# i-- for instance describing the tuples that represent
# i-- PostScript matrices.
# i--

#--------------------------------------------------------------------------

#
# FIXME: Write Texinfo documentation for psMatException and the
# possible gmpy exceptions.
#

class psMatException (Exception):
  def __init__ (self, irritants):
    self.irritants = irritants
  def __str__ (self):
    return str (self.irritants)

def psMat_invert (a):
  # Invert a PostScript matrix. For simplicity, use Cramer’s rule in
  # exact arithmetic.

  a = map (gmpy.mpq, a)

  # Below, the letter ‘A’ represents the matrix:
  #
  #   A = a[0] a[1]
  #       a[2] a[3]

  # det A
  determinant_of_A = a[0] * a[3] - a[1] * a[2]

  # adj A =  a[3] -a[1]
  #         -a[2]  a[0]
  #
  adjugate_of_A = [a[3], -a[1], -a[2], a[0]]

  # A⁻¹ = adj A / det A
  #
  inverse_of_A = [x / determinant_of_A for x in adjugate_of_A]

  # The offset in the plane. (One day we should use conformal
  # geometric algebra instead of this old-fashioned stuff, so offsets
  # require no special handling ... Though, of course, we still need
  # to handle these PostScript notations, anyway.)
  b1 = - (a[4] * inverse_of_A[0] + a[5] * inverse_of_A[2])
  b2 = - (a[4] * inverse_of_A[1] + a[5] * inverse_of_A[3])

  result = map (float, inverse_of_A + [b1, b2])

  if any ([math.isinf (x) or math.isnan (x) for x in result]):
    raise psMatException ([a, result])

  return tuple (result)

def _compose_two_matrices (mat1, mat2):
  mat1 = map (float, mat1)
  mat2 = map (float, mat2)
  a0 = mat1[0] * mat2[0] + mat1[1] * mat2[2]
  a1 = mat1[0] * mat2[1] + mat1[1] * mat2[3]
  a2 = mat1[2] * mat2[0] + mat1[3] * mat2[2]
  a3 = mat1[2] * mat2[1] + mat1[3] * mat2[3]
  a4 = mat1[4] * mat2[0] + mat1[5] * mat2[2] + mat2[4];
  a5 = mat1[4] * mat2[1] + mat1[5] * mat2[3] + mat2[5];
  return (a0, a1, a2, a3, a4, a5)

# i--
# i-- @defun identity ()
# i--
# i-- Return an identity matrix as a six-element tuple.
# i--
# i-- @end defun
# i--
def identity ():
  """Return an identity matrix as a six-element tuple."""
  return (1.0, 0.0, 0.0, 1.0, 0.0, 0.0)

# i--
# i-- @defun compose (@var{mat1}, @var{mat2}, ...)
# i--
# i-- Return a matrix that is the composition of one or
# i-- more input transformations.
# i--
# i-- @end defun
# i--
def compose (mat1, mat2 = None, *rest):
  """Return a matrix that is the composition of one or
  more input transformations.
    
  """
  if mat2 is None:
    assert (len (rest) == 0)
    result = mat1
  else:
    result = _compose_two_matrices (mat1, mat2)
    for m in rest:
      result = _compose_two_matrices (result, m)
  return result

# i--
# i-- @defun inverse (@var{mat})
# i--
# i-- Return a matrix which is the inverse of the input
# i-- transformation.
# i-- (Note: There will not always be an inverse.)
# i--
# i-- @end defun
# i--
def inverse (mat):
  """Return a matrix which is the inverse of the input
  transformation.

  """
  return psMat_invert (mat)

# i--
# i-- @defun rotate (@var{theta})
# i--
# i-- Return a matrix which will rotate by an angle theta
# i-- expressed in radians.
# i--
# i-- @end defun
# i--
def rotate (theta):
  """Return a matrix which will rotate by an angle expressed
  in radians.

  """
  t = float (theta)
  cosine = math.cos (t)
  sine = math.sin (t)
  return (cosine, sine, -sine, cosine, 0.0, 0.0)

# i--
# i-- @defun scale (@var{x} [ , @var{y}])
# i--
# i-- Return a matrix that will scale by x horizontally
# i-- and y vertically.
# i-- If y is omitted, the matrix will scale by x in both
# i-- directions.
# i--
# i-- @end defun
# i--
def scale (x, y = None):
  """Return a matrix that will scale."""
  if y is None:
    y = x
  return (float (x), 0.0, 0.0, float (y), 0.0, 0.0)

# i--
# i-- @defun skew (@var{theta})
# i--
# i-- Return a matrix that will skew by theta
# i-- (to produce an oblique font).
# i-- Theta is expressed in radians.
# i--
# i-- @end defun
# i--
def skew (theta):
  """Return a matrix that will skew."""
  t = float (theta)
  tangent = math.tan (t)
  return (1.0, 0.0, tangent, 1.0, 0.0, 0.0)

# i--
# i-- @defun translate (@var{x}, @var{y})
# i--
# i-- Return a matrix that will translate by x horizontally
# i-- and y vertically.
# i--
# i-- @end defun
# i--
def translate (x, y):
  """Return a matrix that will translate."""
  return (1.0, 0.0, 0.0, 1.0, float (x), float (y))
