#include <config.h>

// Copyright (C) 2000-2012 by George Williams
// Copyright (C) 2012 by Barry Schwartz
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// The name of the author may not be used to endorse or promote products
// derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
// WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
// EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <contour_interface.h>
#include <xalloc.h>

VISIBLE int
SSFromContourData (SplineSet **result, double *x_vals, double *y_vals,
                   int *on_curve_vals, int *selected_vals, int pt_cnt,
                   int is_closed, int is_quadratic, char *name, int *tt_start)
{
  int start;
  int next;
  int i;
  int index;
  int nexti;
  int previ;
  int skipped;
  SplinePoint *sp;

  SplineSet *ss = NULL;
  int errval = CONTOUR_INTERFACE_SUCCESS;

  if (0 < pt_cnt)
    {
      skipped = 0;
      start = *tt_start;
      next = start;
      i = 0;

      ss = xzalloc (sizeof (SplineSet));
      ss->contour_name = xstrdup (name);

      if (is_quadratic)
        {
          if (!on_curve_vals[0])
            {
              if (pt_cnt == 1)
                {
                  ss->first = ss->last =
                    SplinePointCreate (x_vals[0], y_vals[0]);
                  ss->first->selected = selected_vals[0];
                  goto done;
                }
              ++i;
              ++next;
              skipped = true;
            }
          while (i < pt_cnt)
            {
              if (on_curve_vals[i])
                {
                  sp = SplinePointCreate (x_vals[i], y_vals[i]);
                  sp->selected = selected_vals[i];
                  sp->ttfindex = next++;
                  index = -1;
                  if (i > 0 && !on_curve_vals[i - 1])
                    index = i - 1;
                  else if (i == 0 && !on_curve_vals[pt_cnt - 1])
                    index = pt_cnt - 1;
                  if (index != -1)
                    {
                      sp->prevcp.x = x_vals[index];
                      sp->prevcp.y = y_vals[index];
                      sp->noprevcp = false;
                    }
                  if (ss->last == NULL)
                    ss->first = sp;
                  else
                    SplineMake2 (ss->last, sp);
                  ss->last = sp;
                }
              else
                {
                  if (!on_curve_vals[i - 1])
                    {
                      sp =
                        SplinePointCreate ((x_vals[i] + x_vals[i - 1]) / 2,
                                           (y_vals[i] + y_vals[i - 1]) / 2);
                      sp->selected = selected_vals[i];
                      sp->ttfindex = -1;
                      sp->prevcp.x = x_vals[i - 1];
                      sp->prevcp.y = y_vals[i - 1];
                      sp->noprevcp = false;
                      if (ss->last == NULL)
                        ss->first = sp;
                      else
                        SplineMake2 (ss->last, sp);
                      ss->last = sp;
                    }
                  ss->last->nextcp.x = x_vals[i];
                  ss->last->nextcp.y = y_vals[i];
                  ss->last->nonextcp = false;
                  ss->last->nextcpindex = next++;
                }
              ++i;
            }
          if (skipped)
            {
              i = pt_cnt;
              if (!on_curve_vals[i - 1])
                {
                  sp =
                    SplinePointCreate ((x_vals[0] + x_vals[i - 1]) / 2,
                                       (y_vals[0] + y_vals[i - 1]) / 2);
                  sp->selected = selected_vals[0];
                  sp->ttfindex = -1;
                  sp->prevcp.x = x_vals[i - 1];
                  sp->prevcp.y = y_vals[i - 1];
                  sp->noprevcp = false;
                  if (ss->last == NULL)
                    ss->first = sp;
                  else
                    SplineMake2 (ss->last, sp);
                  ss->last = sp;
                }
              ss->last->nextcp.x = x_vals[0];
              ss->last->nextcp.y = y_vals[0];
              ss->last->nonextcp = false;
              ss->last->nextcpindex = start;
            }
        }
      else
        {
          for (i = 0; i < pt_cnt; ++i)
            {
              if (on_curve_vals[i])
                break;
              ++next;
            }
          for (i = 0; i < pt_cnt; ++i)
            {
              if (!on_curve_vals[i])
                continue;
              sp = SplinePointCreate (x_vals[i], y_vals[i]);
              sp->selected = selected_vals[i];
              sp->ttfindex = next++;
              nexti = previ = -1;
              if (i == 0)
                previ = pt_cnt - 1;
              else
                previ = i - 1;
              if (!on_curve_vals[previ])
                {
                  sp->prevcp.x = x_vals[previ];
                  sp->prevcp.y = y_vals[previ];
                  if (sp->prevcp.x != sp->me.x || sp->prevcp.y != sp->me.y)
                    sp->noprevcp = false;
                }
              if (i == pt_cnt - 1)
                nexti = 0;
              else
                nexti = i + 1;
              if (!on_curve_vals[nexti])
                {
                  sp->nextcp.x = x_vals[nexti];
                  sp->nextcp.y = y_vals[nexti];
                  next += 2;
                  if (sp->nextcp.x != sp->me.x || sp->nextcp.y != sp->me.y)
                    sp->nonextcp = false;
                  if (nexti == pt_cnt - 1)
                    nexti = 0;
                  else
                    ++nexti;
                  if (on_curve_vals[nexti])
                    {
                      free (ss->contour_name);
                      free (ss);
                      errval = CONTOUR_INTERFACE_BAD_CUBIC;
                      goto leave;
                    }
                  if (nexti == pt_cnt - 1)
                    nexti = 0;
                  else
                    ++nexti;
                  if (!on_curve_vals[nexti])
                    {
                      free (ss->contour_name);
                      free (ss);
                      errval = CONTOUR_INTERFACE_BAD_CUBIC;
                      goto leave;
                    }
                }
              if (ss->last == NULL)
                ss->first = sp;
              else
                SplineMake3 (ss->last, sp);
              ss->last = sp;
            }
          if (ss->last == NULL)
            {
              free (ss->contour_name);
              free (ss);
              errval = CONTOUR_INTERFACE_EMPTY_CONTOUR;
              goto leave;
            }
        }
      if (is_closed)
        {
          SplineMake (ss->last, ss->first, is_quadratic);
          ss->last = ss->first;
        }
      *tt_start = next;
    done:
      SPLCategorizePoints (ss);
      *result = ss;
    }
leave:
  return errval;
}

VISIBLE int
ContourDataSizeFromSS (SplineSet *ss)
{
  int cnt;
  SplinePoint *sp;
  SplinePoint *skip;

  if (ss->first->next == NULL)
    {
      cnt = 1;
    }
  else if (ss->first->next->order2)
    {
      cnt = 0;
      skip = NULL;
      if (SPInterpolate (ss->first))
        {
          skip = ss->first->prev->from;
          ++cnt;
        }
      for (sp = ss->first;;)
        {
          if (!SPInterpolate (sp))
            ++cnt;
          if (!sp->nonextcp && sp != skip)
            ++cnt;
          if (sp->next == NULL)
            break;
          sp = sp->next->to;
          if (sp == ss->first)
            break;
        }
    }
  else
    {
      for (sp = ss->first, cnt = 0;;)
        {
          ++cnt;                /* Sp itself */
          if (sp->next == NULL)
            break;
          if (!sp->nonextcp || !sp->next->to->noprevcp)
            cnt += 2;           /* not a line => 2 control points */
          sp = sp->next->to;
          if (sp == ss->first)
            break;
        }
    }
  return cnt;
}

VISIBLE void
ContourDataFromSS (SplineSet *ss, double *x_vals, double *y_vals,
                   int *on_curve_vals, int *selected_vals)
{
  int cnt;
  SplinePoint *sp;
  SplinePoint *skip;

  if (ss->first->next == NULL)
    {
      x_vals[0] = ss->first->me.x;
      y_vals[0] = ss->first->me.y;
      on_curve_vals[0] = true;
      selected_vals[0] = ss->first->selected;
    }
  else if (ss->first->next->order2)
    {
      cnt = 0;
      skip = NULL;
      if (SPInterpolate (ss->first))
        {
          skip = ss->first->prev->from;
          x_vals[cnt] = skip->nextcp.x;
          y_vals[cnt] = skip->nextcp.y;
          on_curve_vals[cnt] = false;
          selected_vals[cnt] = skip->selected;
          ++cnt;
        }
      for (sp = ss->first;;)
        {
          if (!SPInterpolate (sp))
            {
              x_vals[cnt] = sp->me.x;
              y_vals[cnt] = sp->me.y;
              on_curve_vals[cnt] = true;
              selected_vals[cnt] = sp->selected;
              ++cnt;
            }
          if (!sp->nonextcp && sp != skip)
            {
              x_vals[cnt] = sp->nextcp.x;
              y_vals[cnt] = sp->nextcp.y;
              on_curve_vals[cnt] = false;
              selected_vals[cnt] = sp->selected && SPInterpolate (sp);
              ++cnt;
            }
          if (sp->next == NULL)
            break;
          sp = sp->next->to;
          if (sp == ss->first)
            break;
        }
    }
  else
    {
      for (sp = ss->first, cnt = 0;;)
        {
          x_vals[cnt] = sp->me.x;
          y_vals[cnt] = sp->me.y;
          on_curve_vals[cnt] = true;
          selected_vals[cnt] = sp->selected;
          ++cnt;                /* Sp itself */
          if (sp->next == NULL)
            break;
          if (!sp->nonextcp || !sp->next->to->noprevcp)
            {
              x_vals[cnt] = sp->nextcp.x;
              y_vals[cnt] = sp->nextcp.y;
              on_curve_vals[cnt] = false;
              selected_vals[cnt] = false;
              x_vals[cnt + 1] = sp->next->to->prevcp.x;
              y_vals[cnt + 1] = sp->next->to->prevcp.y;
              on_curve_vals[cnt + 1] = false;
              selected_vals[cnt + 1] = false;
              cnt += 2;         /* not a line => 2 control points */
            }
          sp = sp->next->to;
          if (sp == ss->first)
            break;
        }
    }
}
