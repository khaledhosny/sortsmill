#! /bin/env python
#-*- coding:utf-8; python-indent: 2; -*-

import array
import sortsmillff.polyspline as spline
import sys

epsilon = sys.float_info.epsilon
write = sys.stdout.write

mono = []
for a in sys.argv[1:]:
  mono.append (float (a))
mono = array.array ('d', mono)

sbern = spline.fl_mono_to_sbern (mono)
bern = spline.fl_mono_to_bern (mono)
mono2s = spline.fl_sbern_to_mono (sbern)
mono2b = spline.fl_bern_to_mono (bern)

for i in range (0, len (mono)):
  if 10 * epsilon < abs (mono[i] - mono2s[i]):
    sys.exit (10)

for i in range (0, len (mono)):
  if 10 * epsilon < abs (mono[i] - mono2b[i]):
    sys.exit (20)

for i in range (0, 101):
  t = i / 100.0
  x1 = spline.fl_eval_mono (mono, t)
  x2 = spline.fl_eval_sbern (sbern, t)
  x3 = spline.fl_eval_bern (bern, t)
  if 10 * epsilon < abs (x1 - x2):
    sys.exit (30)
  if 10 * epsilon < abs (x1 - x3):
    sys.exit (40)

sys.exit (0)