#include <config.h>

// Copyright (C) 2012 Khaled Hosny
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

#include <glib.h>
#include <gio/gio.h>
#include "gio.h"
#include "ustring.h"

char *
GIOGetMimeType (const char *path)
{
  GFile *file;
  GFileInfo *file_info;
  GError *error = NULL;
  const char *content_type;
  char *mime;

  file = g_file_new_for_path (path);
  file_info = g_file_query_info (file,
                                 G_FILE_ATTRIBUTE_STANDARD_CONTENT_TYPE,
                                 0, NULL, &error);
  if (!error)
    {
      content_type = g_file_info_get_content_type (file_info);
      mime = g_content_type_get_mime_type (content_type);
    }
  else
    {
      mime = "*/*";
    }

  return mime;
}
