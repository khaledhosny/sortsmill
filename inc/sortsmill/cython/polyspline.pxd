# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012 Khaled Hosny and Barry Schwartz
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

cdef extern from "sortsmill/math/polyspline.h":
  void change_basis_f64_mono_to_mono (size_t degree,
                                      ssize_t stride, double *spline,
                                      ssize_t result_stride, double *result)
  void change_basis_f64_mono_to_bern (size_t degree,
                                      ssize_t stride, double *spline,
                                      ssize_t result_stride, double *result)
  void change_basis_f64_mono_to_sbern (size_t degree,
                                       ssize_t stride, double *spline,
                                       ssize_t result_stride, double *result)
  void change_basis_f64_mono_to_spower (size_t degree,
                                        ssize_t stride, double *spline,
                                        ssize_t result_stride, double *result)
  void change_basis_f64_bern_to_mono (size_t degree,
                                      ssize_t stride, double *spline,
                                      ssize_t result_stride, double *result)
  void change_basis_f64_bern_to_bern (size_t degree,
                                      ssize_t stride, double *spline,
                                      ssize_t result_stride, double *result)
  void change_basis_f64_bern_to_sbern (size_t degree,
                                       ssize_t stride, double *spline,
                                       ssize_t result_stride, double *result)
  void change_basis_f64_bern_to_spower (size_t degree,
                                        ssize_t stride, double *spline,
                                        ssize_t result_stride, double *result)
  void change_basis_f64_sbern_to_mono (size_t degree,
                                       ssize_t stride, double *spline,
                                       ssize_t result_stride, double *result)
  void change_basis_f64_sbern_to_bern (size_t degree,
                                       ssize_t stride, double *spline,
                                       ssize_t result_stride, double *result)
  void change_basis_f64_sbern_to_sbern (size_t degree,
                                        ssize_t stride, double *spline,
                                        ssize_t result_stride, double *result)
  void change_basis_f64_sbern_to_spower (size_t degree,
                                         ssize_t stride, double *spline,
                                         ssize_t result_stride, double *result)
  void change_basis_f64_spower_to_mono (size_t degree,
                                        ssize_t stride, double *spline,
                                        ssize_t result_stride, double *result)
  void change_basis_f64_spower_to_bern (size_t degree,
                                        ssize_t stride, double *spline,
                                        ssize_t result_stride, double *result)
  void change_basis_f64_spower_to_sbern (size_t degree,
                                         ssize_t stride, double *spline,
                                         ssize_t result_stride, double *result)
  void change_basis_f64_spower_to_spower (size_t degree,
                                          ssize_t stride, double *spline,
                                          ssize_t result_stride, double *result)

  double eval_f64_sbern_schumaker_volk (unsigned int degree, int stride, double *spline, double t)
  double eval_f64_bern_schumaker_volk (unsigned int degree, int stride, double *spline, double t)
  double eval_f64_sbern_de_casteljau (unsigned int degree, int stride, double *spline, double t)
  double eval_f64_bern_de_casteljau (unsigned int degree, int stride, double *spline, double t)
  double eval_f64_mono (unsigned int degree, int stride, double *spline, double t)
  double eval_f64_spower (unsigned int degree, int stride, double *spline, double t)

  void subdiv_f64_sbern (size_t degree, ssize_t stride, double *spline,
                         double t, ssize_t stride_a, double *a, ssize_t stride_b, double *b)
  void subdiv_f64_bern (size_t degree, ssize_t stride, double *spline,
                        double t, ssize_t stride_a, double *a, ssize_t stride_b, double *b)
