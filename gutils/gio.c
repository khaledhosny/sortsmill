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

/* Copyright (C) 2000-2003 by George Williams */
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

#include "giofuncP.h"
#include <gfile.h>
#include <ustring.h>
#include <errno.h>

gdraw_sync_thread_t *gdraw_sync_thread;

static struct protocols
{
  int index;
  uint32_t *proto;
  void *handle;
  void *(*dispatcher) (GIOControl * gc);
  void (*cancel) (GIOControl * gc);
  void (*term) (void *);
  bool dothread;
} *protocols;
static int plen, pmax;
typedef void *(ptread_startfunc_t) (void *);

static uint32_t err501[] =
  { ' ', 'N', 'o', 't', ' ', 'I', 'm', 'p', 'l', 'e', 'm', 'e', 'n', 't', 'e',
'd', '\0' };

static int
AddProtocol (uint32_t *prefix, int len)
{

  if (plen >= pmax)
    {
      pmax += 20;               /* We're never going to support 20 protocols? */
      if (plen == 0)
        {
          protocols =
            (struct protocols *) xmalloc (pmax * sizeof (struct protocols));
        }
      else
        {
          protocols =
            (struct protocols *) xrealloc (protocols,
                                           pmax * sizeof (struct protocols));
        }
    }
  memset (protocols + plen, 0, sizeof (struct protocols));
  if (u8_strncmp (x_gc_u32_to_u8 (u32_force_valid (prefix)), "file", len) == 0)
    {
      protocols[plen].handle = NULL;
      protocols[plen].dispatcher = _GIO_fileDispatch;
      protocols[plen].cancel = NULL;
      protocols[plen].term = NULL;
      protocols[plen].dothread = false;
    }
  else
    {
      return (false);
    }
  protocols[plen].index = plen;
  protocols[plen].proto = x_u32_strmbndup (prefix, len);
  ++plen;
  return (true);
}

static void
GIOdispatch (GIOControl * gc, enum giofuncs gf)
{
  uint32_t *temp, *pt;
  int i;

  gc->gf = gf;

  temp = _GIO_translateURL (gc->path, gf);
  if (temp != NULL)
    {
      if (gc->origpath == NULL)
        gc->origpath = gc->path;
      else
        free (gc->path);
      gc->path = temp;
    }
  if (gc->topath != NULL)
    {
      temp = _GIO_translateURL (gc->topath, gf);
      if (temp != NULL)
        {
          free (gc->topath);
          gc->topath = temp;
        }
    }

  pt = uc_strstr (gc->path, "://");
  if (pt != NULL)
    {
      for (i = 0; i < plen; ++i)
        if (u32_ncasecompare (protocols[i].proto, gc->path, pt - gc->path) == 0)
          break;
      if (i >= plen && !AddProtocol (gc->path, pt - gc->path))
        {
          gc->protocol_index = -2;
          gc->return_code = 501;
          gc->error = err501;
          u32_strcpy (gc->status, x_gc_u8_to_u32 ("No support for browsing: "));
          u32_strncat (gc->status, gc->path, pt - gc->path);
          gc->done = true;
          (gc->receiveerror) (gc);
          return;
        }
      gc->protocol_index = i;
      if (!protocols[i].dothread)
        (protocols[i].dispatcher) (gc);
      else
        {
#ifndef HAVE_PTHREAD_H
          gc->return_code = 501;
          gc->error = err501;
          u32_strcpy (gc->status, x_gc_u8_to_u32 ("No support for protocol"));
          gc->done = true;
          (gc->receiveerror) (gc);
          return;
#else
          static pthread_cond_t initcond = PTHREAD_COND_INITIALIZER;
          static pthread_mutex_t initmutex = PTHREAD_MUTEX_INITIALIZER;
          /* could put stuff here to queue functions if we get too many */
          /*  threads, or perhaps even a thread pool */
          u32_strcpy (gc->status, x_gc_u8_to_u32 ("Queued"));
          gc->threaddata =
            (struct gio_threaddata *) xmalloc (sizeof (struct gio_threaddata));
          gc->threaddata->mutex = initmutex;
          gc->threaddata->cond = initcond;
          if (gdraw_sync_thread != NULL)
            (gdraw_sync_thread) (NULL, NULL, NULL);
          pthread_create (&gc->threaddata->thread, NULL,
                          (ptread_startfunc_t *) (protocols[i].dispatcher), gc);
#endif
        }
    }
  else
    {
      gc->protocol_index = -1;
      _GIO_localDispatch (gc);
    }
}

void
GIOdir (GIOControl * gc)
{
  GIOdispatch (gc, gf_dir);
}

void
GIOfileExists (GIOControl * gc)
{
  /* We can probably do some optimizations here, based on caching and whatnot */
  GIOdispatch (gc, gf_statfile);
}

void
GIOmkDir (GIOControl * gc)
{
  GIOdispatch (gc, gf_mkdir);
}

static void
GIOFreeDirEntries (GDirEntry * ent)
{
  GDirEntry *next;

  while (ent != NULL)
    {
      next = ent->next;
      free (ent->name);
      free (ent->mimetype);
      free (ent);
      ent = next;
    }
}

GDirEntry *
GIOgetDirData (GIOControl * gc)
{

  if (gc->direntrydata)
    return ((GDirEntry *) gc->iodata);

  return (NULL);
}

void
GIOcancel (GIOControl * gc)
{
#ifdef HAVE_PTHREAD_H
  if (gc->protocol_index >= 0 && protocols[gc->protocol_index].dothread &&
      gc->threaddata != NULL && !gc->done)
    {
      void *ret;
      gc->abort = true;
      pthread_cancel (gc->threaddata->thread);
      pthread_join (gc->threaddata->thread, &ret);
    }
#endif
  if (gc->protocol_index >= 0 && protocols[gc->protocol_index].cancel != NULL)
    /* Per connection cleanup, cancels io if not done and removes from any queues */
    (protocols[gc->protocol_index].cancel) (gc);
  if (gc->direntrydata)
    GIOFreeDirEntries ((GDirEntry *) gc->iodata);
  else
    free (gc->iodata);
  free (gc->threaddata);
  free (gc->path);
  free (gc->origpath);
  free (gc->topath);
  free (gc);
}

void
GIOclose (GIOControl * gc)
{
  GIOcancel (gc);
}

GIOControl *
GIOCreate (uint32_t *path, void *userdata,
           void (*receivedata) (struct giocontrol *),
           void (*receiveerror) (struct giocontrol *))
{
  GIOControl *gc = xcalloc (1, sizeof (GIOControl));

  gc->path = x_u32_strdup_or_null (path);
  gc->userdata = userdata;
  gc->receivedata = receivedata;
  gc->receiveerror = receiveerror;
  return (gc);
}

void
GIO_SetThreadCallback (void (*callback) (void *, void *, void *))
{
  gdraw_sync_thread = callback;
}
