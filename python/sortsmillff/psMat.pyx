# -*- coding: utf-8; python-indent: 2; -*-

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

"""Transformations on PostScript matrices.

The matrices are expressed as six-element tuples of floats.

"""

cimport gsl
from libc.math cimport sin, cos, tan

import sys

include "psMat.pxi"

# i--
# i-- @strong{FIXME:} This chapter needs an introductory section,
# i-- for instance describing the tuples that represent
# i-- PostScript matrices.
# i--

#--------------------------------------------------------------------------
#
# This is ugly but something like it seems necessary.

def set_error_handler_off ():
  gsl.gsl_set_error_handler_off ()

set_error_handler_off ()

#--------------------------------------------------------------------------

#
# FIXME: Write Texinfo documentation for these exceptions.
#

class psMatException (Exception):
  """Base class for exceptions raised by psMat."""
  pass

class psMatGSLError (psMatException):
  """Raised when a GSL error occurs during a psMat operation."""
  def __init__ (self, errno):
    self.errno = errno
  def __str__ (self):
    return gsl.gsl_strerror (self.errno)

def psMat_invert (a):
  # Invert a PostScript matrix, by singular value decomposition.

  a = map (float, a)

  cdef double a0 = a[0]
  cdef double a1 = a[1]
  cdef double a2 = a[2]
  cdef double a3 = a[3]
  cdef double a4 = a[4]
  cdef double a5 = a[5]

  cdef int errval

  cdef double u_mat[4]
  cdef gsl.gsl_matrix_view u = gsl.gsl_matrix_view_array (u_mat, 2, 2)
  cdef double v_mat[4]
  cdef gsl.gsl_matrix_view v = gsl.gsl_matrix_view_array (v_mat, 2, 2)
  cdef double s_vec[2]
  cdef gsl.gsl_vector_view s = gsl.gsl_vector_view_array (s_vec, 2)
  cdef double work_vec[2]
  cdef gsl.gsl_vector_view work = gsl.gsl_vector_view_array (work_vec, 2)

  u_mat[0] = a0
  u_mat[1] = a1
  u_mat[2] = a2
  u_mat[3] = a3

  errval = gsl.gsl_linalg_SV_decomp (&u.matrix, &v.matrix, &s.vector, &work.vector)
  if errval != gsl.GSL_SUCCESS:
    raise psMatGSLError (errval)

  # Test for singularity. (Note: rather than do this test, we could
  # let the divisions below overflow.)
  if s_vec[1] <= s_vec[0] * 100.0 * <double> sys.float_info.epsilon:
    raise psMatGSLError (gsl.GSL_ESING)

  cdef double b11 = v_mat[0] * u_mat[0] / s_vec[0] + v_mat[1] * u_mat[1] / s_vec[1]
  cdef double b12 = v_mat[0] * u_mat[2] / s_vec[0] + v_mat[1] * u_mat[3] / s_vec[1]
  cdef double b21 = v_mat[2] * u_mat[0] / s_vec[0] + v_mat[3] * u_mat[1] / s_vec[1]
  cdef double b22 = v_mat[2] * u_mat[2] / s_vec[0] + v_mat[3] * u_mat[3] / s_vec[1]

  cdef double c1 = - (a4 * b11 + a5 * b21)
  cdef double c2 = - (a4 * b12 + a5 * b22)

  return (b11, b12, b21, b22, c1, c2)  

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
# i-- @defun compose (@var{mat1}, @var{mat2})
# i--
# i-- Return a matrix that is the composition of two input
# i-- transformations.
# i--
# i-- @end defun
# i--
def compose (mat1, mat2):
  """Return a matrix that is the composition of two input
  transformations.
    
  """
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
  cdef double t = float (theta)
  cdef double cosine = cos (t)
  cdef double sine = sin (t)
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
  cdef double t = float (theta)
  cdef double tangent = tan (t)
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
