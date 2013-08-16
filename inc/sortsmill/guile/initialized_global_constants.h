/* -*- coding: utf-8 -*- */
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

#ifndef _SORTSMILL_GUILE_INITIALIZED_GLOBAL_CONSTANTS_H
#define _SORTSMILL_GUILE_INITIALIZED_GLOBAL_CONSTANTS_H

#include <sortsmill/initialized_global_constants.h>
#include <libguile.h>

/* An INITIALIZER function for Guile data specified as Scheme source
   code in a C string. */
void scm_c_initialize_from_eval_string (SCM *proc, const char *s);

#define SCM_SYMBOL_CONSTANT(MODIFIER, C_NAME, SCM_NAME)                 \
  INITIALIZED_CONSTANT (STM_ATTRIBUTE_PURE MODIFIER, SCM, C_NAME,       \
                        scm_c_initialize_from_eval_string,              \
                        "(quote " SCM_NAME ")");

#endif /* _SORTSMILL_GUILE_INITIALIZED_GLOBAL_CONSTANTS_H */
