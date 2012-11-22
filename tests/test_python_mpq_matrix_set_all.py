#! /bin/env python
#-*- coding:utf-8; python-indent: 2; -*-

import gmpy
import sys
import sortsmillff.linalg as lin

write = sys.stdout.write

rows = int (sys.argv[1])
cols = int (sys.argv[2])
x = gmpy.mpq (sys.argv[3])

A = lin.mpq_matrix_set_all (rows, cols, x)

B = [[None for i in range (0, cols)] for j in range (0, rows)]
C = lin.mpq_matrix_set_all (B, x)
for i in range (0, rows):
  for j in range (0, cols):
    if A[i][j] != B[i][j]:
      exit (10)
for i in range (0, rows):
  for j in range (0, cols):
    if A[i][j] != C[i][j]:
      exit (20)

for i in range (0, rows):
  for j in range (0, cols):
    if type (A[i][j]) != type (gmpy.mpq (0)):
      exit (100)
    if type (B[i][j]) != type (gmpy.mpq (0)):
      exit (110)
    if type (C[i][j]) != type (gmpy.mpq (0)):
      exit (120)

for i in range (0, rows):
  for j in range (0, cols):
    write (" ")
    write (str (A[i][j]))
  write (" |")

for i in range (0, 10):
  try:
    lin.mpq_matrix_set_all (*[None for j in range (0, i)])
  except:
    pass
