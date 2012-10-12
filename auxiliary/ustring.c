#include <config.h>

/* Copyright (C) 2000-2012 by George Williams */
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

#include <stddef.h>
#include <xunistring.h>
#include "ustring.h"
#include "utype.h"

long
uc_strmatch (const uint32_t *str1, const char *str2)
{
  long ch1, ch2;
  for (;;)
    {
      ch1 = *str1++;
      ch2 = *(unsigned char *) str2++;
      ch1 = tolower (ch1);
      ch2 = tolower (ch2);
      if (ch1 != ch2 || ch1 == '\0')
        return (ch1 - ch2);
    }
}

long
uc_strnmatch (const uint32_t *str1, const char *str2, int len)
{
  long ch1, ch2;
  for (; --len >= 0;)
    {
      ch1 = *str1++;
      ch2 = *(unsigned char *) str2++;
      ch1 = tolower (ch1);
      ch2 = tolower (ch2);
      if (ch1 != ch2 || ch1 == '\0' || len <= 0)
        return (ch1 - ch2);
    }
  return (0);
}

long
u_strnmatch (const uint32_t *str1, const uint32_t *str2, int len)
{
  long ch1, ch2;
  for (; --len >= 0;)
    {
      ch1 = *str1++;
      ch2 = *str2++;
      ch1 = tolower (ch1);
      ch2 = tolower (ch2);
      if (ch1 != ch2 || ch1 == '\0' || len <= 0)
        return (ch1 - ch2);
    }
  return (0);
}

void
cu_strcpy (char *to, const uint32_t *from)
{
  register uint32_t ch;
  while ((ch = *from++) != '\0')
    *(to++) = ch;
  *to = 0;
}

void
uc_strcpy (uint32_t *to, const char *from)
{
  register uint32_t ch;
  while ((ch = *(unsigned char *) from++) != '\0')
    *(to++) = ch;
  *to = 0;
}

void
cu_strncpy (register char *to, const uint32_t *from, int len)
{
  register uint32_t ch;
  while ((ch = *from++) != '\0' && --len >= 0)
    *(to++) = ch;
  *to = 0;
}

void
uc_strncpy (register uint32_t *to, const char *from, int len)
{
  register uint32_t ch;
  while ((ch = *(unsigned char *) from++) != '\0' && --len >= 0)
    *(to++) = ch;
  *to = 0;
}

void
uc_strcat (uint32_t *to, const char *from)
{
  uc_strcpy (to + u32_strlen (to), from);
}

void
uc_strncat (uint32_t *to, const char *from, int len)
{
  uc_strncpy (to + u32_strlen (to), from, len);
}

void
cu_strcat (char *to, const uint32_t *from)
{
  cu_strcpy (to + strlen (to), from);
}

void
cu_strncat (char *to, const uint32_t *from, int len)
{
  cu_strncpy (to + strlen (to), from, len);
}

uint32_t *
uc_strstr (const uint32_t *longer, const char *substr)
{
  long ch1, ch2;
  const uint32_t *lpt, *str1;
  const char *str2;

  for (lpt = longer; *lpt != '\0'; ++lpt)
    {
      str1 = lpt;
      str2 = substr;
      for (;;)
        {
          ch1 = *str1++;
          ch2 = *(unsigned char *) str2++;
          if (ch2 == '\0')
            return ((uint32_t *) lpt);
          if (ch1 != ch2)
            break;
        }
    }
  return (NULL);
}

uint32_t *
uc_strstrmatch (const uint32_t *longer, const char *substr)
{
  long ch1, ch2;
  const uint32_t *lpt, *str1;
  const unsigned char *str2;

  for (lpt = longer; *lpt != '\0'; ++lpt)
    {
      str1 = lpt;
      str2 = (unsigned char *) substr;
      for (;;)
        {
          ch1 = *str1++;
          ch2 = *str2++;
          ch1 = tolower (ch1);
          ch2 = tolower (ch2);
          if (ch2 == '\0')
            return ((uint32_t *) lpt);
          if (ch1 != ch2)
            break;
        }
    }
  return (NULL);
}

uint32_t *
cu_strstartmatch (const char *key, const uint32_t *str)
{
  if (key && str)
    {
      while (*key)
        {
          if (tolower (*key) != tolower (*str))
            return 0;
          key++;
          str++;
        }
    }
  return (uint32_t *) str;
}

uint32_t *
u_strstartmatch (const uint32_t *initial, const uint32_t *full)
{
  int ch1, ch2;
  for (;;)
    {
      ch1 = *initial++;
      ch2 = *full++;
      if (ch1 == '\0')
        return ((uint32_t *) full);
      ch1 = tolower (ch1);
      ch2 = tolower (ch2);
      if (ch1 != ch2 || ch1 == '\0')
        return (NULL);
    }
}

uint32_t *
utf82u_strncpy (uint32_t *ubuf, const char *utf8buf, int len)
{
  uint32_t *upt = ubuf, *uend = ubuf + len - 1;
  const uint8_t *pt = (const uint8_t *) utf8buf, *end = pt + strlen (utf8buf);
  int w, w2;

  while (pt < end && *pt != '\0' && upt < uend)
    {
      if (*pt <= 127)
        *upt = *pt++;
      else if (*pt <= 0xdf)
        {
          *upt = ((*pt & 0x1f) << 6) | (pt[1] & 0x3f);
          pt += 2;
        }
      else if (*pt <= 0xef)
        {
          *upt = ((*pt & 0xf) << 12) | ((pt[1] & 0x3f) << 6) | (pt[2] & 0x3f);
          pt += 3;
        }
      else
        {
          w = (((*pt & 0x7) << 2) | ((pt[1] & 0x30) >> 4)) - 1;
          w = (w << 6) | ((pt[1] & 0xf) << 2) | ((pt[2] & 0x30) >> 4);
          w2 = ((pt[2] & 0xf) << 6) | (pt[3] & 0x3f);
          *upt = w * 0x400 + w2 + 0x10000;
          pt += 4;
        }
      ++upt;
    }
  *upt = '\0';
  return (ubuf);
}

uint32_t *
utf82u_strcpy (uint32_t *ubuf, const char *utf8buf)
{
  return (utf82u_strncpy (ubuf, utf8buf, strlen (utf8buf) + 1));
}

uint32_t *
utf82u_copyn (const char *utf8buf, int len)
{
  uint32_t *ubuf = (uint32_t *) xmalloc ((len + 1) * sizeof (uint32_t));
  return (utf82u_strncpy (ubuf, utf8buf, len + 1));
}

uint32_t *
utf82u_copy (const char *utf8buf)
{
  int len;
  uint32_t *ubuf;

  if (utf8buf == NULL)
    return (NULL);

  len = strlen (utf8buf);
  ubuf = (uint32_t *) xmalloc ((len + 1) * sizeof (uint32_t));
  return (utf82u_strncpy (ubuf, utf8buf, len + 1));
}

void
utf82u_strcat (uint32_t *to, const char *from)
{
  utf82u_strcpy (to + u32_strlen (to), from);
}

char *
u2utf8_strcpy (char *utf8buf, const uint32_t *ubuf)
{
  char *pt = utf8buf;

  while (*ubuf)
    {
      if (*ubuf < 0x80)
        *pt++ = *ubuf;
      else if (*ubuf < 0x800)
        {
          *pt++ = 0xc0 | (*ubuf >> 6);
          *pt++ = 0x80 | (*ubuf & 0x3f);
        }
      else if (*ubuf < 0x10000)
        {
          *pt++ = 0xe0 | (*ubuf >> 12);
          *pt++ = 0x80 | ((*ubuf >> 6) & 0x3f);
          *pt++ = 0x80 | (*ubuf & 0x3f);
        }
      else
        {
          uint32_t val = *ubuf - 0x10000;
          int u = ((val & 0xf0000) >> 16) + 1, z = (val & 0x0f000) >> 12, y =
            (val & 0x00fc0) >> 6, x = val & 0x0003f;
          *pt++ = 0xf0 | (u >> 2);
          *pt++ = 0x80 | ((u & 3) << 4) | z;
          *pt++ = 0x80 | y;
          *pt++ = 0x80 | x;
        }
      ++ubuf;
    }
  *pt = '\0';
  return (utf8buf);
}

char *
utf8_strchr (const char *str, int search)
{
  int ch;
  const char *old = str;

  while ((ch = utf8_ildb (&str)) != 0)
    {
      if (ch == search)
        return ((char *) old);
      old = str;
    }
  return (NULL);
}

char *
latin1_2_utf8_strcpy (char *utf8buf, const char *lbuf)
{
  char *pt = utf8buf;
  const unsigned char *lpt = (const unsigned char *) lbuf;

  while (*lpt)
    {
      if (*lpt < 0x80)
        *pt++ = *lpt;
      else
        {
          *pt++ = 0xc0 | (*lpt >> 6);
          *pt++ = 0x80 | (*lpt & 0x3f);
        }
      ++lpt;
    }
  *pt = '\0';
  return (utf8buf);
}

char *
latin1_2_utf8_copy (const char *lbuf)
{
  int len;
  char *utf8buf;

  if (lbuf == NULL)
    return (NULL);

  len = strlen (lbuf);
  utf8buf = (char *) xmalloc (2 * len + 1);
  return (latin1_2_utf8_strcpy (utf8buf, lbuf));
}

char *
utf8_2_latin1_copy (const char *utf8buf)
{
  int len;
  int ch;
  char *lbuf, *pt;
  const char *upt;

  if (utf8buf == NULL)
    return (NULL);

  len = strlen (utf8buf);
  pt = lbuf = (char *) xmalloc (len + 1);
  for (upt = utf8buf; (ch = utf8_ildb (&upt)) != '\0';)
    if (ch >= 0xff)
      *pt++ = '?';
    else
      *pt++ = ch;
  *pt = '\0';
  return (lbuf);
}

char *
u2utf8_copy (const uint32_t *ubuf)
{
  int len;
  char *utf8buf;

  if (ubuf == NULL)
    return (NULL);

  len = u32_strlen (ubuf);
  utf8buf = (char *) xmalloc ((len + 1) * 4);
  return (u2utf8_strcpy (utf8buf, ubuf));
}

char *
u2utf8_copyn (const uint32_t *ubuf, int len)
{
  int i;
  char *utf8buf, *pt;

  if (ubuf == NULL)
    return (NULL);

  utf8buf = pt = (char *) xmalloc ((len + 1) * 4);
  for (i = 0; i < len && *ubuf != '\0'; ++i)
    pt = utf8_idpb (pt, *ubuf++);
  *pt = '\0';
  return (utf8buf);
}

int32_t
utf8_ildb (const char **_text)
{
  int32_t val = -1;
  int ch;
  const uint8_t *text = (const uint8_t *) *_text;
  /* Increment and load character */

  if ((ch = *text++) < 0x80)
    {
      val = ch;
    }
  else if (ch <= 0xbf)
    {
      /* error */
    }
  else if (ch <= 0xdf)
    {
      if (*text >= 0x80 && *text < 0xc0)
        val = ((ch & 0x1f) << 6) | (*text++ & 0x3f);
    }
  else if (ch <= 0xef)
    {
      if (*text >= 0x80 && *text < 0xc0 && text[1] >= 0x80 && text[1] < 0xc0)
        {
          val =
            ((ch & 0xf) << 12) | ((text[0] & 0x3f) << 6) | (text[1] & 0x3f);
          text += 2;
        }
    }
  else
    {
      int w = (((ch & 0x7) << 2) | ((text[0] & 0x30) >> 4)) - 1, w2;
      w = (w << 6) | ((text[0] & 0xf) << 2) | ((text[1] & 0x30) >> 4);
      w2 = ((text[1] & 0xf) << 6) | (text[2] & 0x3f);
      val = w * 0x400 + w2 + 0x10000;
      if (*text < 0x80 || text[1] < 0x80 || text[2] < 0x80 ||
          *text >= 0xc0 || text[1] >= 0xc0 || text[2] >= 0xc0)
        val = -1;
      else
        text += 3;
    }
  *_text = (const char *) text;
  return (val);
}

char *
utf8_idpb (char *utf8_text, uint32_t ch)
{
  /* Increment and deposit character */
  if (ch < 0 || ch >= 17 * 65536)
    return (utf8_text);

  if (ch <= 127)
    *utf8_text++ = ch;
  else if (ch <= 0x7ff)
    {
      *utf8_text++ = 0xc0 | (ch >> 6);
      *utf8_text++ = 0x80 | (ch & 0x3f);
    }
  else if (ch <= 0xffff)
    {
      *utf8_text++ = 0xe0 | (ch >> 12);
      *utf8_text++ = 0x80 | ((ch >> 6) & 0x3f);
      *utf8_text++ = 0x80 | (ch & 0x3f);
    }
  else
    {
      uint32_t val = ch - 0x10000;
      int u = ((val & 0xf0000) >> 16) + 1, z = (val & 0x0f000) >> 12, y =
        (val & 0x00fc0) >> 6, x = val & 0x0003f;
      *utf8_text++ = 0xf0 | (u >> 2);
      *utf8_text++ = 0x80 | ((u & 3) << 4) | z;
      *utf8_text++ = 0x80 | y;
      *utf8_text++ = 0x80 | x;
    }
  return (utf8_text);
}


char *
utf8_ib (char *utf8_text)
{
  int ch;

  /* Increment character */
  if ((ch = *utf8_text) == '\0')
    return (utf8_text);
  else if (ch <= 127)
    return (utf8_text + 1);
  else if (ch < 0xe0)
    return (utf8_text + 2);
  else if (ch < 0xf0)
    return (utf8_text + 3);
  else
    return (utf8_text + 4);
}

int
utf8_valid (const char *str)
{
  /* Is this a valid utf8 string? */
  int ch;

  while ((ch = utf8_ildb (&str)) != '\0')
    if (ch == -1)
      return (false);

  return (true);
}

void
utf8_truncatevalid (char *str)
{
  /* There are certain cases where we have a fixed amount of space to display */
  /*  something, and if it doesn't fit in that, then we truncate it. But... */
  /*  that can leave us with a half completed utf8 byte sequence. So truncate */
  /*  again, right before the start of the bad sequence */
  int ch;
  char *old;

  old = str;
  while ((ch = utf8_ildb ((const char **) &str)) != '\0')
    {
      if (ch == -1)
        {
          *old = '\0';
          return;
        }
      old = str;
    }
}

char *
utf8_db (char *utf8_text)
{
  /* Decrement utf8 pointer */
  unsigned char *pt = (unsigned char *) utf8_text;

  --pt;
  if (*pt >= 0xc0)
    /* This should never happen. The pointer was looking at an intermediate */
    /*  character. However, if it does happen then we are now properly */
    /*  positioned at the start of a new char */ ;
  else if (*pt >= 0x80)
    {
      --pt;
      if (*pt >= 0xc0)
        /* Done */ ;
      else if (*pt >= 0x80)
        {
          --pt;
          if (*pt >= 0xc0)
            /* Done */ ;
          else if (*pt >= 0x80)
            --pt;
        }
    }
  return ((char *) pt);
}

int
utf82u_strlen (const char *utf8_str)
{
  /* how many shorts needed to represent it in UCS2 */
  int ch;
  int len = 0;

  while ((ch = utf8_ildb (&utf8_str)) > 0)
    if (ch > 0x10000)
      len += 2;
    else
      ++len;
  return (len);
}

void
utf8_strncpy (register char *to, const char *from, int len)
{
  /* copy n characters NOT bytes */
  const char *old = from;
  while (len && *old)
    {
      utf8_ildb (&old);
      len--;
    }
  strncpy (to, from, old - from);
  to[old - from] = 0;
}

#include <chardata.h>
char *
StripToASCII (const char *utf8_str)
{
  /* Remove any non-ascii characters: Special case, convert the copyright symbol to (c) */
  char *newcr, *pt, *end;
  int len, ch;
  const uint32_t *alt;

  len = strlen (utf8_str);
  pt = newcr = (char *) xmalloc (len + 1);
  end = pt + len;
  while ((ch = utf8_ildb (&utf8_str)) != '\0')
    {
      if (pt >= end)
        {
          int off = pt - newcr;
          newcr = (char *) xrealloc (newcr, (off + 10) + 1);
          pt = newcr + off;
          end = pt + 10;
        }
      if ((ch >= ' ' && ch < '\177') || ch == '\n' || ch == '\t')
        *pt++ = ch;
      else if (ch == '\r' && *utf8_str != '\n')
        *pt++ = '\n';
      else if (ch == 0xa9 /* Copyright sign */ )
        {
          char *str = "(c)";
          if (pt + strlen (str) >= end)
            {
              int off = pt - newcr;
              newcr =
                (char *) xrealloc (newcr, (off + 10 + strlen (str)) + 1);
              pt = newcr + off;
              end = pt + 10;
            }
          while (*str)
            *pt++ = *str++;
        }
      else if (unicode_alternates[ch >> 8] != NULL &&
               (alt = unicode_alternates[ch >> 8][ch & 0xff]) != NULL)
        {
          while (*alt != '\0')
            {
              if (pt >= end)
                {
                  int off = pt - newcr;
                  newcr = (char *) xrealloc (newcr, (off + 10) + 1);
                  pt = newcr + off;
                  end = pt + 10;
                }
              if (*alt >= ' ' && *alt < '\177')
                *pt++ = *alt;
              else if (*alt == 0x300)
                *pt++ = '`';
              else if (*alt == 0x301)
                *pt++ = '\'';
              else if (*alt == 0x302)
                *pt++ = '^';
              else if (*alt == 0x303)
                *pt++ = '~';
              else if (*alt == 0x308)
                *pt++ = ':';
              ++alt;
            }
        }
    }
  *pt = '\0';
  return (newcr);
}

int
AllAscii (const char *txt)
{
  for (; *txt != '\0'; ++txt)
    {
      if (*txt == '\t' || *txt == '\n' || *txt == '\r')
        /* All right */ ;
      else if (*txt < ' ' || *txt >= '\177')
        return (false);
    }
  return (true);
}

int
uAllAscii (const uint32_t *txt)
{
  for (; *txt != '\0'; ++txt)
    {
      if (*txt == '\t' || *txt == '\n' || *txt == '\r')
        /* All right */ ;
      else if (*txt < ' ' || *txt >= '\177')
        return (false);
    }
  return (true);
}
