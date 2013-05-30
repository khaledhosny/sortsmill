#include <config.h>

// libspiro - conversion between spiro control points and bezier's
// Copyright (C) 2007 Raph Levien
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


/* Interface routines to Raph's spiro package. */

#include "spiroentrypoints.h"
#include <stdlib.h>

bool
SpiroCPsToBezier (spiro_cp *spiros, int n, int isclosed, bezctx *bc)
{
  bool success = false;
  if (1 <= n)
    {
      spiro_seg *s;
      if (!isclosed)
        {
          char oldty_start = spiros[0].ty;
          char oldty_end = spiros[n - 1].ty;
          spiros[0].ty = '{';
          spiros[n - 1].ty = '}';
          s = run_spiro (spiros, n);
          spiros[n - 1].ty = oldty_end;
          spiros[0].ty = oldty_start;
        }
      else
        s = run_spiro (spiros, n);
      if (s != NULL)
        {
          spiro_to_bpath (s, n, bc);
          free_spiro (s);
          success = true;
        }
    }
  return success;
}

bool
TaggedSpiroCPsToBezier (spiro_cp *spiros, bezctx *bc)
{
  bool success = false;

  int n = 0;
  while (spiros[n].ty != 'z' && spiros[n].ty != '}')
    n++;
  if (spiros[n].ty == '}')
    n++;

  if (1 <= n)
    {
      spiro_seg *s = run_spiro (spiros, n);
      if (s != NULL)
        {
          spiro_to_bpath (s, n, bc);
          free_spiro (s);
          success = true;
        }
    }

  return success;
}
