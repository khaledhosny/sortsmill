# -*- coding: utf-8 -*-
#
# Copyright (C) 2012 by Barry Schwartz
# This file is part of the Sorts Mill Tools.
# 
# Sorts Mill Tools is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# Sorts Mill Tools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

#
# FIXME FIXME FIXME: Rewrite this in Guile. This Python code won’t
# even run now that I have deprecated python-libunicodenames.
#

import unicodenames
import locale, re, sys

locale.setlocale(locale.LC_ALL, 'en_US')

uniname_re_str = sys.argv[1]
uniname_re = re.compile(uniname_re_str)

function_name = sys.argv[2]

names_db = unicodenames.unicodenames(unicodenames.find_names_db())
matches = []
codepoint = 0
while codepoint <= 0x10FFFF:
    uniname = names_db.name(codepoint)
    if uniname is not None and uniname_re.search(uniname) is not None:
        matches.append(codepoint)
    codepoint += 1

print('/* This is a generated file. */')
print('')
print('#include <stdlib.h>')
print('')
print('/*')
print(' *  Codepoints whose names match the regular expression')
print(' *')
print(' *      ' + uniname_re_str)
print(' *')
print(' */')
print('static unsigned int matching_codepoints[{}] ='.format(len(matches)))
print('{')
matches_str = str(matches)
print(matches_str[1:-1])
print('};')
print('')

print('static int compare_codepoints (const void *codepoint1, const void *codepoint2)')
print('{')
print('    const unsigned int *cp1 = (const unsigned int *) codepoint1;')
print('    const unsigned int *cp2 = (const unsigned int *) codepoint2;')
print('    return ((*cp1 < *cp2) ? -1 : ((*cp1 == *cp2) ? 0 : 1));')
print('}')
print('')

print('int {}(unsigned int codepoint);'.format(function_name))
print('')

print('int {}(unsigned int codepoint)'.format(function_name))
print('{')
print('    unsigned int *p =')
print('        (unsigned int *) bsearch (&codepoint, matching_codepoints,')
print('                                  {}, sizeof (unsigned int),'.format(len(matches)))
print('                                  compare_codepoints);')
print('    return (p != (unsigned int *) 0);')
print('}')
print('')
