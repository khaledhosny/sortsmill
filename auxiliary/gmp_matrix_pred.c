#include <config.h>

// Copyright (C) 2013 Barry Schwartz
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <sortsmill/gmp_matrix.h>

#undef _FF_GMP_ELEMENTWISE_PRED

#define _FF_GMP_ELEMENTWISE_PRED(NAME, ELEMENT_PRED)            \
  bool                                                          \
  NAME (unsigned int m, unsigned int n, _GMP_TYPE (_t) A[m][n]) \
  {                                                             \
    bool result = true;                                         \
    for (unsigned int i = 0; i < m; i++)                        \
      for (unsigned int j = 0; j < n; j++)                      \
        {                                                       \
          /* The following is coded purposely to make early  */ \
          /* exit necessary for correct results. Thus we can */ \
          /* more easily detect if the code gets broken.     */ \
          result = ELEMENT_PRED (A[i][j]);                      \
          if (result == false)                                  \
            {                                                   \
              i = m - 1;                                        \
              j = n - 1;                                        \
            }                                                   \
        }                                                       \
    return result;                                              \
  }

static inline bool _GMP_TYPE (_isnull) (_GMP_TYPE (_t) x)
{
  return (_GMP_TYPE (_sgn (x)) == 0);
}

static inline bool _GMP_TYPE (_ispos) (_GMP_TYPE (_t) x)
{
  return (0 < _GMP_TYPE (_sgn (x)));
}

static inline bool _GMP_TYPE (_isneg) (_GMP_TYPE (_t) x)
{
  return (_GMP_TYPE (_sgn (x)) < 0);
}

static inline bool _GMP_TYPE (_isnonneg) (_GMP_TYPE (_t) x)
{
  return (0 <= _GMP_TYPE (_sgn (x)));
}

VISIBLE _FF_GMP_ELEMENTWISE_PRED (_GMP_TYPE (_matrix_isnull),
                                  _GMP_TYPE (_isnull));

VISIBLE _FF_GMP_ELEMENTWISE_PRED (_GMP_TYPE (_matrix_ispos),
                                  _GMP_TYPE (_ispos));

VISIBLE _FF_GMP_ELEMENTWISE_PRED (_GMP_TYPE (_matrix_isneg),
                                  _GMP_TYPE (_isneg));

VISIBLE _FF_GMP_ELEMENTWISE_PRED (_GMP_TYPE (_matrix_isnonneg),
                                  _GMP_TYPE (_isnonneg));
