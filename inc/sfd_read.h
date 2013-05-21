/*
 * Copyright (C) 2012 Khaled Hosny and Barry Schwartz
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

#ifndef _SFD_READ_H
#define _SFD_READ_H 1

#include <config.h>

#include <stdio.h>
#include <libguile.h>

struct sfd_keyword
{
  int name;
  const char *arg_types;
};

const char *sfd_pool_string (int name);

const struct sfd_keyword *sfd_keyword_lookup (const char *str,
                                              unsigned int len);



SCM get_sfd_line (SCM port, SCM continuation_allowed);







VISIBLE char *get_sfd_line_from_file (void *file);
VISIBLE void sfd_to_scheme (char *(get_next_line) (void *), void *source_of_lines);

#endif // _SFD_READ_H
