# Copyright (C) 2000-2012 by George Williams
# Copyright (C) 2012, 2013 by Barry Schwartz
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

EXTRA_DIST =

MOSTLYCLEANFILES =
CLEANFILES =
DISTCLEANFILES =

AM_CPPFLAGS += -I$(top_builddir)/fontforge -I$(top_srcdir)/fontforge
AM_LDFLAGS = $(MY_CFLAGS) $(MY_LIBS)
LDADD = $(top_builddir)/fontforge/libsortsmill_fontforge.la \
		$(top_builddir)/auxiliary/libsortsmill_aux.la \
		$(top_builddir)/gutils/libsortsmill_gutils.la \
		$(top_builddir)/lib/libgnu.la

INSTALL_HOOKS =
UNINSTALL_HOOKS =

install-exec-hook: $(INSTALL_HOOKS)
uninstall-hook: $(UNINSTALL_HOOKS)

#--------------------------------------------------------------------------

if THE_PROGRAMS
if HAVE_GUI

bin_PROGRAMS += sortsmill-editor

sortsmill_editor_SOURCES = editor.c
sortsmill_editor_LDADD =									\
	$(top_builddir)/fontforge/libsortsmill_fontforgeexe.la	\
	$(top_builddir)/lib/libgnu.la
sortsmill_editor_LDFLAGS = $(AM_LDFLAGS) -export-dynamic

INSTALL_HOOKS += install-smed
UNINSTALL_HOOKS += uninstall-smed

install-smed: uninstall-smed
	cd $(DESTDIR)$(bindir) && $(LN_S) sortsmill-editor$(EXEEXT) smed$(EXEEXT)

uninstall-smed:
	-rm -f $(DESTDIR)$(bindir)/smed$(EXEEXT)

endif HAVE_GUI
endif THE_PROGRAMS

#--------------------------------------------------------------------------

if THE_PROGRAMS

# FIXME: Update the manpages for the new gengetopt-based options.
bin_PROGRAMS += sortsmill-lint sortsmill-image sortsmill-compare

dist_sortsmill_lint_SOURCES = $(srcdir)/lint.c

dist_sortsmill_image_SOURCES = $(srcdir)/image.c

dist_sortsmill_compare_SOURCES = $(srcdir)/compare.c

INSTALL_HOOKS += install-lint-etc
UNINSTALL_HOOKS += uninstall-lint-etc

install-lint-etc: uninstall-lint-etc
	cd $(DESTDIR)$(bindir) &&									\
		$(LN_S) sortsmill-lint$(EXEEXT) smlint$(EXEEXT) &&		\
		$(LN_S) sortsmill-image$(EXEEXT) smimage$(EXEEXT) &&	\
		$(LN_S) sortsmill-compare$(EXEEXT) smcompare$(EXEEXT)

uninstall-lint-etc:
	-rm -f $(DESTDIR)$(bindir)/smlint$(EXEEXT)	\
		$(DESTDIR)$(bindir)/smimage$(EXEEXT)	\
		$(DESTDIR)$(bindir)/smcompare$(EXEEXT)

endif THE_PROGRAMS

#--------------------------------------------------------------------------
