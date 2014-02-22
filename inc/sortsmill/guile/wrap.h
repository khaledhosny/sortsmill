/*
 * Copyright (C) 2013 Khaled Hosny and Barry Schwartz
 * This file is part of the Sorts Mill Tools.
 * 
 * Sorts Mill Tools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Sorts Mill Tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_GUILE_WRAP_H
#define _SORTSMILL_GUILE_WRAP_H

#include <sortsmill/guile/core.h>

#define SCM_FF_API_CALL_0(PROC_NAME)                            \
  (scm_call_0 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME)))

#define SCM_FF_API_CALL_1(PROC_NAME, A1)                        \
  (scm_call_1 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME), (A1)))

#define SCM_FF_API_CALL_2(PROC_NAME, A1, A2)                    \
  (scm_call_2 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME), (A1), (A2)))

#define SCM_FF_API_CALL_3(PROC_NAME, A1, A2, A3)                \
  (scm_call_3 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME), (A1), (A2), (A3)))

#define SCM_FF_API_CALL_4(PROC_NAME, A1, A2, A3, A4)            \
  (scm_call_4 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME),                    \
               (A1), (A2), (A3), (A4)))

#define SCM_FF_API_CALL_5(PROC_NAME, A1, A2, A3, A4, A5)        \
  (scm_call_5 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME),                    \
               (A1), (A2), (A3), (A4), (A5)))

#define SCM_FF_API_CALL_6(PROC_NAME, A1, A2, A3, A4, A5, A6)    \
  (scm_call_6 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME),                    \
               (A1), (A2), (A3), (A4), (A5), (A6)))

#endif /* _SORTSMILL_GUILE_WRAP_H */
