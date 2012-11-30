# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012 Barry Schwartz
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

cimport libc.stddef

cdef extern from "gsl/gsl_errno.h":

  cdef enum:
    GSL_SUCCESS  = 0 
    GSL_FAILURE  = -1
    GSL_CONTINUE = -2
    GSL_EDOM     = 1
    GSL_ERANGE   = 2
    GSL_EFAULT   = 3
    GSL_EINVAL   = 4
    GSL_EFAILED  = 5
    GSL_EFACTOR  = 6
    GSL_ESANITY  = 7
    GSL_ENOMEM   = 8
    GSL_EBADFUNC = 9
    GSL_ERUNAWAY = 10
    GSL_EMAXITER = 11
    GSL_EZERODIV = 12
    GSL_EBADTOL  = 13
    GSL_ETOL     = 14
    GSL_EUNDRFLW = 15
    GSL_EOVRFLW  = 16
    GSL_ELOSS    = 17
    GSL_EROUND   = 18
    GSL_EBADLEN  = 19
    GSL_ENOTSQR  = 20
    GSL_ESING    = 21
    GSL_EDIVERGE = 22
    GSL_EUNSUP   = 23
    GSL_EUNIMPL  = 24
    GSL_ECACHE   = 25
    GSL_ETABLE   = 26
    GSL_ENOPROG  = 27
    GSL_ENOPROGJ = 28
    GSL_ETOLF    = 29
    GSL_ETOLX    = 30
    GSL_ETOLG    = 31
    GSL_EOF      = 32

  ctypedef void gsl_error_handler_t (char *reason, char *file, int line, int gsl_errno)

  gsl_error_handler_t *gsl_set_error_handler (gsl_error_handler_t *new_handler)
  gsl_error_handler_t *gsl_set_error_handler_off ()

  char *gsl_strerror (int gsl_errno)

cdef extern from "gsl/gsl_block.h":

  ctypedef struct gsl_block:
    size_t size
    double *data

cdef extern from "gsl/gsl_vector.h":

  ctypedef struct gsl_vector:
    size_t size
    size_t stride
    double *data
    gsl_block *block
    int owner

  ctypedef struct gsl_vector_view:
    gsl_vector vector

  gsl_vector *gsl_vector_alloc (size_t n)
  gsl_vector *gsl_vector_calloc (size_t n)
  void gsl_vector_free (gsl_vector *v)

  double gsl_vector_get (gsl_vector *v, size_t i)
  void gsl_vector_set (gsl_vector *v, size_t i, double x)
  double *gsl_vector_ptr (gsl_vector *v, size_t i)

  gsl_vector_view gsl_vector_view_array (double *v, size_t n)
  gsl_vector_view gsl_vector_view_array_with_stride (double *base, size_t stride, size_t n)

cdef extern from "gsl/gsl_matrix.h":

  ctypedef struct gsl_matrix:
    size_t size1
    size_t size2
    size_t tda
    double *data
    gsl_block *block
    int owner

  ctypedef struct gsl_matrix_view:
    gsl_matrix matrix

  gsl_matrix *gsl_matrix_alloc (size_t n1, size_t n2)
  gsl_matrix *gsl_matrix_calloc (size_t n1, size_t n2)
  void gsl_matrix_free (gsl_matrix *m)

  double gsl_matrix_get (gsl_matrix *m, size_t i, size_t j)
  void gsl_matrix_set (gsl_matrix *m, size_t i, size_t j, double x)
  double *gsl_matrix_ptr (gsl_matrix *m, size_t i, size_t j)

  gsl_matrix_view gsl_matrix_view_array (double *base, size_t n1, size_t n2)
  gsl_matrix_view gsl_matrix_view_array_with_tda (double *base, size_t n1, size_t n2, size_t tda)
  gsl_matrix_view gsl_matrix_view_vector (gsl_vector *v, size_t n1, size_t n2)
  gsl_matrix_view gsl_matrix_view_vector_with_tda (gsl_vector *v, size_t n1, size_t n2, size_t tda)

cdef extern from "gsl/gsl_permutation.h":
  ctypedef struct gsl_permutation:
    size_t size
    size_t *data

  gsl_permutation *gsl_permutation_alloc (size_t n)
  gsl_permutation *gsl_permutation_calloc (size_t n)
  void gsl_permutation_init (gsl_permutation *p)
  void gsl_permutation_free (gsl_permutation *p)
  int gsl_permutation_memcpy (gsl_permutation *dest, gsl_permutation *src)

  size_t gsl_permutation_get (gsl_permutation *p, size_t i)
  int gsl_permutation_swap (gsl_permutation *p, size_t i, size_t j)

cdef extern from "gsl/gsl_linalg.h":

  int gsl_linalg_SV_decomp (gsl_matrix *A, gsl_matrix *V, gsl_vector *S, gsl_vector *work)
  int gsl_linalg_SV_decomp_mod (gsl_matrix *A, gsl_matrix *X, gsl_matrix *V, gsl_vector *S, gsl_vector *work)
  int gsl_linalg_SV_decomp_jacobi (gsl_matrix *A, gsl_matrix *Q, gsl_vector *S)
  int gsl_linalg_SV_solve (gsl_matrix *U, gsl_matrix *Q, gsl_vector *S, gsl_vector *b, gsl_vector *x)

  int gsl_linalg_LU_decomp (gsl_matrix *A, gsl_permutation *p, int *signum)
  int gsl_linalg_LU_solve (gsl_matrix *LU, gsl_permutation *p, gsl_vector *b, gsl_vector *x)
  int gsl_linalg_LU_svx (gsl_matrix *LU, gsl_permutation *p, gsl_vector *x)
  int gsl_linalg_LU_refine (gsl_matrix *A, gsl_matrix *LU, gsl_permutation *p,
                            gsl_vector *b, gsl_vector *x, gsl_vector *residual)
  int gsl_linalg_LU_invert (gsl_matrix *LU, gsl_permutation *p, gsl_matrix *inverse)

  int gsl_linalg_QR_decomp (gsl_matrix *A, gsl_vector *tau)
  int gsl_linalg_QR_solve (gsl_matrix *QR, gsl_vector *tau, gsl_vector *b, gsl_vector *x)
  int gsl_linalg_QR_svx (gsl_matrix *QR, gsl_vector *tau, gsl_vector *x)
  int gsl_linalg_QR_lssolve (gsl_matrix *QR, gsl_vector *tau, gsl_vector *b, 
                             gsl_vector *x, gsl_vector *residual)
  int gsl_linalg_QR_QRsolve (gsl_matrix *Q, gsl_matrix *R, gsl_vector *b, gsl_vector *x)
  int gsl_linalg_QR_Rsolve (gsl_matrix *QR, gsl_vector *b, gsl_vector *x)
  int gsl_linalg_QR_Rsvx (gsl_matrix *QR, gsl_vector *x)
  int gsl_linalg_QR_update (gsl_matrix *Q, gsl_matrix *R, gsl_vector *w, gsl_vector *v)
  int gsl_linalg_QR_QTvec (gsl_matrix *QR, gsl_vector *tau, gsl_vector *v)
  int gsl_linalg_QR_Qvec (gsl_matrix *QR, gsl_vector *tau, gsl_vector *v)
  int gsl_linalg_QR_QTmat (gsl_matrix *QR, gsl_vector *tau, gsl_matrix *A)
  int gsl_linalg_QR_unpack (gsl_matrix *QR, gsl_vector *tau, gsl_matrix *Q, gsl_matrix *R)
  int gsl_linalg_R_solve (gsl_matrix *R, gsl_vector *b, gsl_vector *x)
  int gsl_linalg_R_svx (gsl_matrix *R, gsl_vector *x)
