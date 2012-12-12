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

%.c: %.scm
	${BIGLOO} ${AM_BGLFLAGS} ${BGLFLAGS} --to-stdout $< > $@

BIGLOO_CFLAGS = -I$(BIGLOO_LIBRARY_DIRECTORY)

BIGLOO_LIBS = -L$(BIGLOO_LIBRARY_DIRECTORY)							\
	'-lbigloo_s-$(BIGLOO_RELEASE_NUMBER)' $(BIGLOO_USER_LIBRARIES)

# $(call bigloo_main_name,my_main)
bigloo_main_name = -DBIGLOO_MAIN=$(1)

# $(call bigloo_no_exit)
bigloo_no_exit = -DBIGLOO_EXIT='BUNSPEC,'

# To create a dynamically loadable module with initialization procedure
#
#    int my_init_proc (int argc, char *argv[], char *env[]);
#
# put the following in the CFLAGS:
#
#    $(call bigloo_init_proc,my_init_proc)
#
bigloo_init_proc = $(call bigloo_main_name,$(1)) $(call bigloo_no_exit)

