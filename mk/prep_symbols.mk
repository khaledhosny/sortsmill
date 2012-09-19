# Copyright (C) 2000-2012 by George Williams
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
#
# Preparing lists of global symbols that shared libraries should
# export.
#
# (Please keep in mind that some linkers will export all possible
# symbols, regardless of our suggestions to the contrary.)
#
#--------------------------------------------------------------------------

PREP_SYMBOLS = $(TR) ' ' '\n' | LC_ALL=C $(MY_SORT)

prep_symbols_echo = echo "echo $($1) | $(PREP_SYMBOLS) > $@"
prep_symbols_without_echo = echo $($1) | $(PREP_SYMBOLS) > $@
prep_symbols_with_echo = $(call prep_symbols_echo,$1); $(call prep_symbols_without_echo,$1)

prep_symbols = \
	@if test x"$(AM_V_lt)" = x; then \
		$(call prep_symbols_with_echo,$1); \
	else \
		echo 'Preparing global symbols listed in $1 for export...'; \
		$(call prep_symbols_without_echo,$1); \
	fi

#--------------------------------------------------------------------------
