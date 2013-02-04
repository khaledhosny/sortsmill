# Copyright (C) 2012 by Barry Schwartz
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# The name of the author may not be used to endorse or promote products
# derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#--------------------------------------------------------------------------

define compile-cython =
	$(AM_V_GEN)
	$(AM_V_at)$(CYTHON) $(AM_CYTHONFLAGS) $(CYTHONFLAGS) --force -o $@-tmp $<
	$(AM_V_at)$(PERL) -i -n -e 'print ("#include <config.h>\n") if $$. == 1;				\
		s%^PyMODINIT_FUNC(\s*(init|PyInit_).*/\*\s*proto\s*\*/)%VISIBLE PyMODINIT_FUNC\1%;	\
		print $$_' $@-tmp
	$(AM_V_at)mv $@-tmp $@
endef

%.c: %.pyx
	$(compile-cython)

# Sometimes we will want to use ‘pure’ Python, if only to demonstrate
# how to write CPython code for Sorts Mill Tools. Nevertheless, we can
# compile such code with Cython, which has a ‘pure’ Python mode.
# Cython checks the code more thoroughly than does CPython, and
# produces C code that is compatible with both CPython 2.7 and
# CPython 3.x.
%.c: %.py
	$(compile-cython)
