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

include $(top_srcdir)/mk/flags.am

bin_PROGRAMS =
dist_man_MANS =

EXTRA_DIST =

MOSTLYCLEANFILES =
CLEANFILES =
DISTCLEANFILES =

AM_CPPFLAGS += -I$(top_builddir)/fontforge -I$(top_srcdir)/fontforge
AM_LDFLAGS = $(MY_CFLAGS) $(MY_LIBS)
LDADD = $(top_builddir)/fontforge/libsortsmillff_fontforge.la	\
	$(top_builddir)/lib/libgnu.la

#--------------------------------------------------------------------------

if THE_PROGRAMS
if HAVE_GUI

bin_PROGRAMS += fontforge
dist_man_MANS += fontforge.1

fontforge_SOURCES = main.c
fontforge_LDADD =												\
	$(top_builddir)/fontforge/libsortsmillff_fontforgeexe.la	\
	$(top_builddir)/lib/libgnu.la
fontforge_LDFLAGS = $(AM_LDFLAGS) -export-dynamic

endif HAVE_GUI
endif THE_PROGRAMS

#--------------------------------------------------------------------------

if THE_PROGRAMS

# FIXME: Update the manpages for the new gengetopt-based options.
bin_PROGRAMS += fontlint fontimage sfddiff
dist_man_MANS += fontlint.1 fontimage.1 sfddiff.1

dist_fontlint_SOURCES = $(srcdir)/fontlint.c			\
	$(srcdir)/fontlint_opts.c $(srcdir)/fontlint_opts.h
$(srcdir)/fontlint.c: $(srcdir)/fontlint_opts.h

dist_fontimage_SOURCES = $(srcdir)/fontimage.c				\
	$(srcdir)/fontimage_opts.c $(srcdir)/fontimage_opts.h
$(srcdir)/fontimage.c: $(srcdir)/fontimage_opts.h

dist_sfddiff_SOURCES = $(srcdir)/sfddiff.c $(srcdir)/sfddiff_opts.c	\
	$(srcdir)/sfddiff_opts.h
$(srcdir)/sfddiff.c: $(srcdir)/sfddiff_opts.h

$(srcdir)/%.c $(srcdir)/%.h: stamp-%-ggo
	@if test ! -f $@; then rm -f $<; else :; fi
	@if test ! -f $@; then $(MAKE) $(AM_MAKEFLAGS) $<; else :; fi

stamp-%-ggo: %.ggo
	$(GENGETOPT) --unamed-opts --output-dir=$(srcdir) --file-name=$* < $<
	@touch $@

EXTRA_DIST += fontlint_opts.ggo fontimage_opts.ggo sfddiff_opts.ggo

endif THE_PROGRAMS

#--------------------------------------------------------------------------
