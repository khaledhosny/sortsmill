#include <config.h>

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

#include "basics.h"
#include <string.h>
#include "flaglist.h"

int
FindFlagByName (struct flaglist *flags, const char *name)
{
  int i;
  for (i = 0; flags[i].name != NULL; ++i)
    {
      if (strcmp (name, flags[i].name) == 0)
        return (flags[i].flag);
    }
  return FLAG_UNKNOWN;
}

const char *
FindNameOfFlag (struct flaglist *flags, int flag)
{
  int i;
  for (i = 0; flags[i].name != NULL; ++i)
    {
      if (flags[i].flag == flag)
        return flags[i].name;
    }
  return NULL;
}
