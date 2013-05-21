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

/* flaglist.h */
#ifndef _FLAGLIST_H_
#define _FLAGLIST_H_

#include "basics.h"

struct flaglist
{
  const char *name;
  int flag;
};

#define FLAGLIST_EMPTY { NULL, 0 }
#define FLAG_UNKNOWN ((int32_t)0x80000000)

extern int FindFlagByName (struct flaglist *flaglist, const char *name);
extern const char *FindNameOfFlag (struct flaglist *flaglist, int flag);

#endif
