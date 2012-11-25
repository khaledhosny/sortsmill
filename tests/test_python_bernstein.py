#! /bin/env python
#-*- coding:utf-8; python-indent: 2; -*-

import array
import sortsmillff.polyspline as spline
import sys

epsilon = sys.float_info.epsilon
write = sys.stdout.write

spline1 = [5, 4, -3, 2, 1, 0, 1, -2, 3, 4, 5]
times = [0, 0.25, 0.5, 0.75, 1]

write ("eval_sbern_double\n")
for deg in range (0, 11):
  for t in times:
    b = array.array ("d", spline1[:deg + 1])
    write ("{:.6f}|".format (spline.fl_eval_sbern (b, t)))
  write ("\n")

write ("eval_bern_double\n")
for deg in range (0, 11):
  for t in times:
    b = array.array ("d", spline1[:deg + 1])
    write ("{:.6f}|".format (spline.fl_eval_bern (b, t)))
  write ("\n")

write ("evaldc_sbern_double\n")
for deg in range (0, 11):
  for t in times:
    b = array.array ("d", spline1[:deg + 1])
    write ("{:.6f}|".format (spline.fl_evaldc_sbern (b, t)))
  write ("\n")

write ("evaldc_bern_double\n")
for deg in range (0, 11):
  for t in times:
    b = array.array ("d", spline1[:deg + 1])
    write ("{:.6f}|".format (spline.fl_evaldc_bern (b, t)))
  write ("\n")

write ("subdiv_sbern_double\n")
for deg in range (0, 5):
  for t in times:
    write ("t={:.6f}|".format (t))
    s = array.array ("d", spline1[:deg + 1])
    (a, b) = spline.fl_subdiv_sbern (s, t)
    for j in range (0, deg + 1):
      write ("{:.6f}|".format (a[j]))
    for j in range (0, deg + 1):
      write ("{:.6f}|".format (b[j]))
    write ("\n")

write ("subdiv_bern_double\n")
for deg in range (0, 5):
  for t in times:
    write ("t={:.6f}|".format (t))
    s = array.array ("d", spline1[:deg + 1])
    (a, b) = spline.fl_subdiv_bern (s, t)
    for j in range (0, deg + 1):
      write ("{:.6f}|".format (a[j]))
    for j in range (0, deg + 1):
      write ("{:.6f}|".format (b[j]))

    # Check that subdivision gives the same result as
    # evaluation.
    v = spline.fl_eval_bern (s, t)
    difference = abs (b[0] - v)
    close_enough = (difference <= 10 * epsilon)
    write ("{:1d}".format (close_enough))

    write ("\n")
