#! /bin/env python
#-*- coding:utf-8; python-indent: 2; -*-

import gmpy
import sys
import sortsmillff.linalg as lin

write = sys.stdout.write

rows = int (sys.argv[1])
cols = int (sys.argv[2])

i_argv = 3

A = [[sys.argv[i_argv + i * cols + j] for j in range (0, cols)]
     for i in range (0, rows)]

i_argv += rows * cols

B = [[sys.argv[i_argv + i * cols + j] for j in range (0, cols)]
     for i in range (0, rows)]

C, D = lin.mpz_matrix_swap (A, B)

for i in range (0, rows):
  for j in range (0, cols):
    if B[i][j] != C[i][j]:
      exit (10)
for i in range (0, rows):
  for j in range (0, cols):
    if B[i][j] != D[i][j]:
      exit (20)

for i in range (0, rows):
  for j in range (0, cols):
    if type (B[i][j]) != type (gmpy.mpz (0)):
      exit (100)
    if type (C[i][j]) != type (gmpy.mpz (0)):
      exit (110)
    if type (D[i][j]) != type (gmpy.mpz (0)):
      exit (120)

for i in range (0, rows):
  for j in range (0, cols):
    write (" ")
    write (str (B[i][j]))
  write (" |")

for i in range (0, 10):
  try:
    lin.mpz_matrix_memcpy (*[None for j in range (0, i)])
  except:
    pass
