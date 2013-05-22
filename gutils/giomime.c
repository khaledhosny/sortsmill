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

#include <glib.h>
#include <gio/gio.h>
#include "gio.h"
#include "ustring.h"

char *
GIOGetMimeType (const char *path)
{
  char *content_type, *mime;
  int sniff_length = 4096;
  guchar sniff_buffer[sniff_length];
  gboolean uncertain;
  FILE *fp;
  size_t res;

  content_type = g_content_type_guess (path, NULL, 0, NULL);

  fp = fopen (path, "rb");
  if (fp)
    {
      res = fread (sniff_buffer, 1, sniff_length, fp);
      fclose (fp);
      if (res >= 0)
        {
          g_free (content_type);
	  // first force guessing file type from the content only by passing
	  // NULL for file name, if the result is not certain try again with
	  // file name
          content_type = g_content_type_guess (NULL, sniff_buffer, res, &uncertain);
          if (uncertain)
            {
              g_free (content_type);
              content_type = g_content_type_guess (path, sniff_buffer, res, NULL);
            }
        }
    }

  mime = g_content_type_get_mime_type (content_type);
  g_free (content_type);

  if (!mime)
    mime = "*/*";

  return mime;
}
