#! /bin/env python
#-*- coding:utf-8; python-indent: 2; -*-

import gmpy
import sys
import sortsmillff.linalg as lin

write = sys.stdout.write

rows = int (sys.argv[1])
cols = int (sys.argv[2])

A = lin.mpz_matrix_set_zero (rows, cols)

B = [[None for i in range (0, cols)] for j in range (0, rows)]
C = lin.mpz_matrix_set_zero (B)
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
    write (" ")
    write (str (A[i][j]))
  write (" |")
