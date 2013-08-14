#include <config.h>

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
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

#include <sortsmill/guile.h>
#include <sortsmill/rexp.h>
#include <sortsmill/initialized_global_constants.h>
#include <intl.h>
#include <assert.h>

static const char my_module[] = "sortsmill fonts peg_spacing";

//-------------------------------------------------------------------------

static void
initialize_u8_re (rexp_t *re, rexp_buffer_t *re_buf, const char *re_string)
{
  *re = u8_rexp_compile_once_study (re_buf, re_string);
}

// Examples:
//
//   r;bottom-serif
//   l;k;my-peg-identifier
//
// The characters legal in identifiers are similar to those for
// identifiers and symbols in R⁵RS.
static const uint8_t peg_name_re_string[] =
  "^([lr]);([ks];|)([[:alnum:]!$%&*+-./:<=>?@^_~]+)$";

static rexp_buffer_t peg_name_re_buf = REXP_BUFFER_T_INITIALIZER;

INITIALIZED_CONSTANT (STM_ATTRIBUTE_PURE static, rexp_t,
                      peg_name_re, initialize_u8_re, &peg_name_re_buf,
                      peg_name_re_string);

static rexp_match_t
peg_name_match (uint8_t *name)
{
  return u8_rexp_match (peg_name_re (), name);
}

VISIBLE bool
scm_is_spacing_peg_name (SCM name)
{
  uint8_t *s = (uint8_t *) scm_to_utf8_stringn (name, NULL);
  const bool result = (bool) peg_name_match (s);
  free (s);
  return result;
}

VISIBLE SCM
scm_spacing_peg_name_p (SCM name)
{
  return scm_from_bool (scm_is_spacing_peg_name (name));
}

VISIBLE SCM
scm_spacing_peg_side (SCM name)
{
  SCM result = SCM_BOOL_F;
  uint8_t *s = (uint8_t *) scm_to_utf8_stringn (name, NULL);
  rexp_match_t m = peg_name_match (s);
  if (m)
    {
      const rexp_interval_t interv = rexp_interval (m, 1);
      switch (s[interv.i_start])
        {
        case 'l':
          result = scm_symbol__left ();
          break;
        case 'r':
          result = scm_symbol__right ();
          break;
        default:
          assert (false);
          break;
        }
    }
  free (s);
  return result;
}

VISIBLE SCM
scm_spacing_peg_modifier (SCM name)
{
  SCM result = SCM_BOOL_F;
  uint8_t *s = (uint8_t *) scm_to_utf8_stringn (name, NULL);
  rexp_match_t m = peg_name_match (s);
  if (m)
    {
      const rexp_interval_t interv = rexp_interval (m, 2);
      if (interv.i_start != interv.i_end)
        switch (s[interv.i_start])
          {
          case 'k':
            result = scm_symbol__kerning_only ();
            break;
          case 's':
            result = scm_symbol__special ();
            break;
          default:
            break;
          }
    }
  free (s);
  return result;
}

VISIBLE SCM
scm_spacing_peg_identifier (SCM name)
{
  SCM result = SCM_BOOL_F;
  uint8_t *s = (uint8_t *) scm_to_utf8_stringn (name, NULL);
  rexp_match_t m = peg_name_match (s);
  if (m)
    {
      const rexp_interval_t interv = rexp_interval (m, 3);
      result = scm_from_utf8_stringn (s + interv.i_start,
                                      interv.i_end - interv.i_start);
    }
  free (s);
  return result;
}

VISIBLE SCM
scm_spacing_pegs (SCM anchor_points)
{
  const char *who = "scm_spacing_pegs";

  SCM pegs = SCM_EOL;
  SCM p = anchor_points;
  while (!scm_is_null (p))
    {
      scm_c_assert_can_be_list_link (who, anchor_points, p);
      SCM anchor_name = scm_anchor_point_name_2 (SCM_CAR (p));
      if (scm_is_spacing_peg_name (anchor_name))
        pegs = scm_cons (SCM_CAR (p), pegs);
      p = SCM_CDR (p);
    }
  return pegs;
}

VISIBLE SCM
scm_left_spacing_pegs (SCM anchor_points)
{
  const char *who = "scm_left_spacing_pegs";

  SCM pegs = SCM_EOL;
  SCM p = anchor_points;
  while (!scm_is_null (p))
    {
      scm_c_assert_can_be_list_link (who, anchor_points, p);
      SCM anchor_name = scm_anchor_point_name_2 (SCM_CAR (p));
      if (scm_is_eq (scm_spacing_peg_side (anchor_name), scm_symbol__left ()))
        pegs = scm_cons (SCM_CAR (p), pegs);
      p = SCM_CDR (p);
    }
  return pegs;
}

VISIBLE SCM
scm_right_spacing_pegs (SCM anchor_points)
{
  const char *who = "scm_right_spacing_pegs";

  SCM pegs = SCM_EOL;
  SCM p = anchor_points;
  while (!scm_is_null (p))
    {
      scm_c_assert_can_be_list_link (who, anchor_points, p);
      SCM anchor_name = scm_anchor_point_name_2 (SCM_CAR (p));
      if (scm_is_eq (scm_spacing_peg_side (anchor_name), scm_symbol__right ()))
        pegs = scm_cons (SCM_CAR (p), pegs);
      p = SCM_CDR (p);
    }
  return pegs;
}

VISIBLE SCM
scm_ordinary_spacing_pegs (SCM anchor_points)
{
  const char *who = "scm_ordinary_spacing_pegs";

  SCM pegs = SCM_EOL;
  SCM p = anchor_points;
  while (!scm_is_null (p))
    {
      scm_c_assert_can_be_list_link (who, anchor_points, p);
      SCM anchor_name = scm_anchor_point_name_2 (SCM_CAR (p));
      if (scm_is_spacing_peg_name (anchor_name)
          && scm_is_eq (scm_spacing_peg_modifier (anchor_name), SCM_BOOL_F))
        pegs = scm_cons (SCM_CAR (p), pegs);
      p = SCM_CDR (p);
    }
  return pegs;
}

VISIBLE SCM
scm_kerning_only_spacing_pegs (SCM anchor_points)
{
  const char *who = "scm_kerning_only_spacing_pegs";

  SCM pegs = SCM_EOL;
  SCM p = anchor_points;
  while (!scm_is_null (p))
    {
      scm_c_assert_can_be_list_link (who, anchor_points, p);
      SCM anchor_name = scm_anchor_point_name_2 (SCM_CAR (p));
      if (scm_is_eq (scm_spacing_peg_modifier (anchor_name),
                     scm_symbol__kerning_only ()))
        pegs = scm_cons (SCM_CAR (p), pegs);
      p = SCM_CDR (p);
    }
  return pegs;
}

VISIBLE SCM
scm_special_spacing_pegs (SCM anchor_points)
{
  const char *who = "scm_special_spacing_pegs";

  SCM pegs = SCM_EOL;
  SCM p = anchor_points;
  while (!scm_is_null (p))
    {
      scm_c_assert_can_be_list_link (who, anchor_points, p);
      SCM anchor_name = scm_anchor_point_name_2 (SCM_CAR (p));
      if (scm_is_eq (scm_spacing_peg_modifier (anchor_name),
                     scm_symbol__special ()))
        pegs = scm_cons (SCM_CAR (p), pegs);
      p = SCM_CDR (p);
    }
  return pegs;
}


VISIBLE SCM
scm_nonspecial_spacing_pegs (SCM anchor_points)
{
  const char *who = "scm_nonspecial_spacing_pegs";

  SCM pegs = SCM_EOL;
  SCM p = anchor_points;
  while (!scm_is_null (p))
    {
      scm_c_assert_can_be_list_link (who, anchor_points, p);
      SCM anchor_name = scm_anchor_point_name_2 (SCM_CAR (p));
      if (scm_is_spacing_peg_name (anchor_name)
          && !scm_is_eq (scm_spacing_peg_modifier (anchor_name),
                         scm_symbol__special ()))
        pegs = scm_cons (SCM_CAR (p), pegs);
      p = SCM_CDR (p);
    }
  return pegs;
}

VISIBLE SCM
scm_left_ordinary_spacing_pegs (SCM anchor_points)
{
  const char *who = "scm_left_ordinary_spacing_pegs";

  SCM pegs = SCM_EOL;
  SCM p = anchor_points;
  while (!scm_is_null (p))
    {
      scm_c_assert_can_be_list_link (who, anchor_points, p);
      SCM anchor_name = scm_anchor_point_name_2 (SCM_CAR (p));
      if (scm_is_eq (scm_spacing_peg_side (anchor_name), scm_symbol__left ())
          && scm_is_eq (scm_spacing_peg_modifier (anchor_name), SCM_BOOL_F))
        pegs = scm_cons (SCM_CAR (p), pegs);
      p = SCM_CDR (p);
    }
  return pegs;
}

VISIBLE SCM
scm_right_ordinary_spacing_pegs (SCM anchor_points)
{
  const char *who = "scm_right_ordinary_spacing_pegs";

  SCM pegs = SCM_EOL;
  SCM p = anchor_points;
  while (!scm_is_null (p))
    {
      scm_c_assert_can_be_list_link (who, anchor_points, p);
      SCM anchor_name = scm_anchor_point_name_2 (SCM_CAR (p));
      if (scm_is_eq (scm_spacing_peg_side (anchor_name), scm_symbol__right ())
          && scm_is_eq (scm_spacing_peg_modifier (anchor_name), SCM_BOOL_F))
        pegs = scm_cons (SCM_CAR (p), pegs);
      p = SCM_CDR (p);
    }
  return pegs;
}

VISIBLE SCM
scm_within_peg_spacing_tolerance_p (SCM a, SCM b)
{
  SCM tolerance =
    scm_fluid_ref (scm_variable_ref (scm_c_lookup ("peg-spacing-tolerance")));
  if (scm_is_false (scm_real_p (tolerance))
      || scm_is_true (scm_negative_p (tolerance)))
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition
        ("scm_within_peg_spacing_tolerance_p"),
        rnrs_c_make_message_condition (_("fluid `peg-spacing-tolerance' "
                                         "must be set "
                                         "to a nonnegative real number")),
        rnrs_make_irritants_condition (scm_list_1 (tolerance))));
  return scm_leq_p (scm_abs (scm_difference (a, b)), tolerance);
}

#if 0                           // NOT YET USED

static int
compare_anchor_point_coords (const void *coords1, const void *coords2)
{
  SCM x1 = ((const SCM *) coords1)[0];
  SCM y1 = ((const SCM *) coords1)[1];
  SCM x2 = ((const SCM *) coords2)[0];
  SCM y2 = ((const SCM *) coords2)[1];

  int result;
  if (scm_is_true (scm_less_p (y1, y2)))
    result = -1;
  else if (scm_is_true (scm_num_eq_p (y1, y2)))
    {
      if (scm_is_true (scm_less_p (x1, x2)))
        result = -1;
      else if (scm_is_true (scm_num_eq_p (x1, x2)))
        result = 0;
      else
        result = 1;
    }
  else
    result = 1;
  return result;
}

#endif // NOT YET USED

typedef SCM _my_scm_compare_func_t (SCM, SCM);

static _my_scm_compare_func_t *
_my_scm_compare_func (const char *who, SCM side)
{
  _my_scm_compare_func_t *result = NULL;
  if (scm_is_eq (side, scm_symbol__left ()))
    result = scm_less_p;
  else if (scm_is_eq (side, scm_symbol__right ()))
    result = scm_gr_p;
  else
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("expected 'left or 'right")),
        rnrs_make_irritants_condition (scm_list_1 (side))));
  return result;
}

#if 0                           // NOT YET USED

static void
make_peg_signature (const char *who, SCM side, size_t *peg_count,
                    SCM coords[*peg_count][2])
{
  assert (0 < *peg_count);

  size_t count = *peg_count;
  _my_scm_compare_func_t *compare = _my_scm_compare_func (who, side);

  qsort (&coords[0][0], count, 2 * sizeof (SCM), compare_anchor_point_coords);

  size_t i = 0;
  while (i < count - 1)
    {
      if (scm_is_true (scm_within_peg_spacing_tolerance_p (coords[i][1],
                                                           coords[i + 1][1])))
        {
          // Two pegs are within tolerance of each other
          // vertically. Choose the one that juts out the most on the
          // given side.
          count--;
          if (scm_is_true (compare (coords[i][0], coords[i + 1][0])))
            // Choose the first.
            memmove (&coords[i + 1][0], &coords[i + 2][0],
                     (count - 1 - i) * 2 * sizeof (SCM));
          else
            // Choose the second.
            memmove (&coords[i][0], &coords[i + 1][0],
                     (count - i) * 2 * sizeof (SCM));
        }
      else
        i++;
    }
  *peg_count = count;
}

static void
normalize_peg_signature (SCM spacing, size_t peg_count,
                         SCM coords[peg_count][2])
{
  for (size_t i = 0; i < peg_count; i++)
    coords[i][0] = scm_difference (coords[i][0], spacing);
}

#endif // NOT YET USED

static SCM
calculate_spacing (const char *who, SCM side, size_t peg_count,
                   SCM coords[peg_count][2])
{
  // Choose the x coordinate that juts out the most on the given side.

  assert (0 < peg_count);

  _my_scm_compare_func_t *compare = _my_scm_compare_func (who, side);

  SCM choice = coords[0][0];
  for (size_t i = 1; i < peg_count; i++)
    if (scm_is_true (compare (coords[i][0], choice)))
      choice = coords[i][0];
  return choice;
}

static void
make_coords_array (SCM anchor_points, size_t *peg_count, SCM **coords)
{
  size_t count = scm_to_size_t (scm_length (anchor_points));
  SCM *array = (SCM *) scm_gc_malloc (2 * sizeof (SCM) * count,
                                      "peg spacing coords array");

  SCM p = anchor_points;
  for (size_t i = 0; i < count; i++)
    {
      scm_c_anchor_point_coords (SCM_CAR (p), &array[2 * i], &array[2 * i + 1]);
      p = SCM_CDR (p);
    }

  *peg_count = count;
  *coords = array;
}

VISIBLE SCM
scm_peg_spacing_left_spacing (SCM anchor_points)
{
  size_t peg_count;
  SCM *coords;

  SCM pegs = scm_left_ordinary_spacing_pegs (anchor_points);
  SCM spacing = SCM_BOOL_F;
  if (!scm_is_null (pegs))
    {
      make_coords_array (pegs, &peg_count, &coords);
      spacing = calculate_spacing ("scm_peg_spacing_left_spacing",
                                   scm_symbol__left (), peg_count,
                                   (SCM (*)[2]) coords);
    }
  return spacing;
}

VISIBLE SCM
scm_peg_spacing_right_spacing (SCM anchor_points)
{
  size_t peg_count;
  SCM *coords;

  SCM pegs = scm_right_ordinary_spacing_pegs (anchor_points);
  SCM spacing = SCM_BOOL_F;
  if (!scm_is_null (pegs))
    {
      make_coords_array (pegs, &peg_count, &coords);
      spacing = calculate_spacing ("scm_peg_spacing_right_spacing",
                                   scm_symbol__right (), peg_count,
                                   (SCM (*)[2]) coords);
    }
  return spacing;
}

//-------------------------------------------------------------------------

void init_guile_fonts_peg_spacing (void);

VISIBLE void
init_guile_fonts_peg_spacing (void)
{
  scm_c_define_gsubr ("spacing-peg-name?", 1, 0, 0, scm_spacing_peg_name_p);
  scm_c_define_gsubr ("spacing-peg-side", 1, 0, 0, scm_spacing_peg_side);
  scm_c_define_gsubr ("spacing-peg-modifier", 1, 0, 0,
                      scm_spacing_peg_modifier);
  scm_c_define_gsubr ("spacing-peg-identifier", 1, 0, 0,
                      scm_spacing_peg_identifier);

  scm_c_define_gsubr ("spacing-pegs", 1, 0, 0, scm_spacing_pegs);
  scm_c_define_gsubr ("left-spacing-pegs", 1, 0, 0, scm_left_spacing_pegs);
  scm_c_define_gsubr ("right-spacing-pegs", 1, 0, 0, scm_right_spacing_pegs);
  scm_c_define_gsubr ("ordinary-spacing-pegs", 1, 0, 0,
                      scm_ordinary_spacing_pegs);
  scm_c_define_gsubr ("kerning-only-spacing-pegs", 1, 0, 0,
                      scm_kerning_only_spacing_pegs);
  scm_c_define_gsubr ("special-spacing-pegs", 1, 0, 0,
                      scm_special_spacing_pegs);
  scm_c_define_gsubr ("nonspecial-spacing-pegs", 1, 0, 0,
                      scm_nonspecial_spacing_pegs);
  scm_c_define_gsubr ("left-ordinary-spacing-pegs", 1, 0, 0,
                      scm_left_ordinary_spacing_pegs);
  scm_c_define_gsubr ("right-ordinary-spacing-pegs", 1, 0, 0,
                      scm_right_ordinary_spacing_pegs);

  scm_c_define_gsubr ("within-peg-spacing-tolerance?", 2, 0, 0,
                      scm_within_peg_spacing_tolerance_p);
  scm_c_define_gsubr ("peg-spacing-left-spacing", 1, 0, 0,
                      scm_peg_spacing_left_spacing);
  scm_c_define_gsubr ("peg-spacing-right-spacing", 1, 0, 0,
                      scm_peg_spacing_right_spacing);
}

//-------------------------------------------------------------------------
