# Copyright (C) 2012 by Khaled Hosny and Barry Schwartz
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

#--------------------------------------------------------------------------

%.c: %.pyx
	$(AM_V_GEN)
	$(AM_V_at)$(CYTHON) $(AM_CYTHONFLAGS) $(CYTHONFLAGS) --force -o $@-tmp $<
	$(AM_V_at)$(PERL) -i -n -e 'print ("#include <config.h>\n") if $$. == 1;				\
		s%^PyMODINIT_FUNC(\s*(init|PyInit_).*/\*\s*proto\s*\*/)%VISIBLE PyMODINIT_FUNC\1%;	\
		print $$_' $@-tmp
	$(AM_V_at)mv $@-tmp $@

# Sometimes we will want to use ‘pure’ Python, if only to demonstrate
# how to write CPython code for Sorts Mill Tools. Nevertheless, we can
# compile such code with Cython, which has a ‘pure’ Python mode.
# Cython checks the code more thoroughly than does CPython, and
# produces C code that is compatible with both CPython 2.7 and
# CPython 3.x.
%.c: %.py
	$(AM_V_GEN)
	$(AM_V_at)$(CYTHON) $(AM_CYTHONFLAGS) $(CYTHONFLAGS) --force -o $@-tmp $<
	$(AM_V_at)$(PERL) -i -n -e 'print ("#include <config.h>\n") if $$. == 1;				\
		s%^PyMODINIT_FUNC(\s*(init|PyInit_).*/\*\s*proto\s*\*/)%VISIBLE PyMODINIT_FUNC\1%;	\
		print $$_' $@-tmp
	$(AM_V_at)mv $@-tmp $@
