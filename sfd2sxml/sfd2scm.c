#include <config.h>

// Copyright (C) 2012 Khaled Hosny and Barry Schwartz
// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <stdio.h>
#include <progname.h>
#include <sfd_read.h>

int
main (int argc, char **argv)
{
  set_program_name (argv[0]);

  FILE *f = fopen (argv[1], "r");
  if (f != NULL)
    {
      sfd_to_scheme (get_sfd_line_from_file, f);
    }

  return 0;
}

