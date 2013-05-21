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

/* Copyright (C) 2002-2012 by George Williams */
/*
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.

 * The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <string.h>
#include <fontforge.h>
#include <gfile.h>

char **
GetFontNames (char *filename)
{
  FILE *foo;
  char **ret = NULL;

  if (GFileIsDir (filename))
    {
      char *temp =
        xmalloc (strlen (filename) + strlen ("/glyphs/contents.plist") + 1);
      strcpy (temp, filename);
      strcat (temp, "/glyphs/contents.plist");
      if (GFileExists (temp))
        ret = NamesReadUFO (filename);
      else
        {
          strcpy (temp, filename);
          strcat (temp, "/font.props");
          if (GFileExists (temp))
            ret = NamesReadSFD (temp);
          /* The fonts.prop file will look just like an sfd file as far */
          /* as fontnames are concerned, we don't need a separate routine */
        }
      free (temp);
    }
  else
    {
      foo = fopen (filename, "rb");
      if (foo != NULL)
        {
          /* Try to guess the file type from the first few characters... */
          int ch1 = getc (foo);
          int ch2 = getc (foo);
          int ch3 = getc (foo);
          int ch4 = getc (foo);
          int ch5, ch6;
          fseek (foo, 98, SEEK_SET);
          ch5 = getc (foo);
          ch6 = getc (foo);
          fclose (foo);
          if ((ch1 == 0 && ch2 == 1 && ch3 == 0 && ch4 == 0)
              || (ch1 == 'O' && ch2 == 'T' && ch3 == 'T' && ch4 == 'O')
              || (ch1 == 't' && ch2 == 'r' && ch3 == 'u' && ch4 == 'e')
              || (ch1 == 't' && ch2 == 't' && ch3 == 'c' && ch4 == 'f'))
            {
              ret = NamesReadTTF (filename);
            }
          else if ((ch1 == '%' && ch2 == '!')
		   || (ch1 == 0x80 && ch2 == '\01'))
            {                   /* PFB header */
              ret = NamesReadPostScript (filename);
            }
          else if (ch1 == '%' && ch2 == 'P' && ch3 == 'D' && ch4 == 'F')
            {
              ret = NamesReadPDF (filename);
            }
          else if (ch1 == '<' && ch2 == '?' && (ch3 == 'x' || ch3 == 'X')
                   && (ch4 == 'm' || ch4 == 'M'))
            {
              ret = NamesReadSVG (filename);
            }
          else if (ch1 == 'S' && ch2 == 'p' && ch3 == 'l' && ch4 == 'i')
            {
              ret = NamesReadSFD (filename);
            }
          else if (ch1 == 1 && ch2 == 0 && ch3 == 4)
            {
              ret = NamesReadCFF (filename);
            }
          else                  /* Too hard to figure out a valid mark for a mac resource file */
            ret = NamesReadMacBinary (filename);
        }
    }
  return (ret);
}
