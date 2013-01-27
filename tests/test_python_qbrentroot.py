#! /bin/env python
#-*- coding:utf-8; python-indent: 2; -*-

from sortsmill.brentroot import qbrentroot
from sortsmill.brentroot import qbrentroot_values
from gmpy import mpq
import sys
import math

write = sys.stdout.write

func_string = sys.argv[1]
t1 = float (sys.argv[2]);
t2 = float (sys.argv[3]);

if 5 <= len (sys.argv):
    max_iters = int (sys.argv[4])
else:
    max_iters = -1

if 6 <= len (sys.argv):
    tol = float (sys.argv[5])
else:
    tol = -1

func = eval (func_string)

(root1, err, iter_no) = qbrentroot_values (t1, t2, func,
                                           max_iters=max_iters,
                                           tol=tol)
if err == 0:
    write ("err = {:d}, root = {:.6f}, iter_no = {:d}"
           .format (err, float (root1), iter_no))
    if type (root1) == type (mpq (1)):
      write (", exact")
else:
    write ("err = {:d}".format (err))

# Check that brentroot returns the same result as brentroot_values.
root2 = qbrentroot (t1, t2, func, max_iters=max_iters,
                    tol=tol)
if root2 == root1:
    exit (0)
else:
    exit (1)
